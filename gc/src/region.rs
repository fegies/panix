use std::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit, num::NonZeroI16};

use crate::{GcArrayPtr, GcPointer, MemoryManager, RawGcPointer, RegionId, Trace};

/// a pointer to the exact 8-byte offset of the header to the provided value
#[derive(Clone, Copy)]
pub struct RegionValuePointer {
    object_id: u16,
}

pub struct GcRegion {
    size: usize,
    region_id: RegionId,
    base_address: *mut GcRegionHeader,
    free_top: UnsafeCell<*mut GcRegionHeader>,
    scavenge_top: *mut GcRegionHeader,
}

type ErasedTraceFnPtr = fn(*const u8, &mut dyn FnMut(&mut RawGcPointer));

// we don't use a proper rust enum because the added padding
// would blow up the entry header to 16 bytes for no benefit
#[repr(align(8))]
struct GcRegionHeader {
    // data: GcRegionEntry,
    marked_alive: UnsafeCell<bool>,
    data_type: EntryType,
    data: u32,
    data_size: u16,
    trace_pointer: ErasedTraceFnPtr,
}
enum EntryType {
    ForwardingPointer,
    InlineData,
    LargeObjectReference,
}

impl GcRegionHeader {
    #[inline]
    const fn parse(&self) -> GcRegionEntry {
        match self.data_type {
            EntryType::ForwardingPointer => {
                let region = unsafe { NonZeroI16::new_unchecked((self.data >> 16) as i16) };
                let value = (self.data % 16) as u16;
                GcRegionEntry::ForwardingPointer(RawGcPointer {
                    value: RegionValuePointer { object_id: value },
                    region,
                })
            }
            EntryType::InlineData => GcRegionEntry::InlineData {
                next_header_offset: self.data,
            },
            EntryType::LargeObjectReference => GcRegionEntry::LargeObjectReference(self.data),
        }
    }
}

enum GcRegionEntry {
    ForwardingPointer(RawGcPointer),
    InlineData { next_header_offset: u32 },
    LargeObjectReference(u32),
}

impl GcRegionEntry {
    #[inline]
    const fn unparse(self) -> (u32, EntryType) {
        match self {
            GcRegionEntry::ForwardingPointer(p) => {
                let val = ((p.region.get() as u32) << 16) | p.value.object_id as u32;
                (val, EntryType::ForwardingPointer)
            }
            GcRegionEntry::InlineData { next_header_offset } => {
                (next_header_offset, EntryType::InlineData)
            }
            GcRegionEntry::LargeObjectReference(r) => (r, EntryType::LargeObjectReference),
        }
    }
}

const HEADER_SIZE: usize = core::mem::size_of::<GcRegionHeader>();
const HEADER_ALIGN: usize = core::mem::align_of::<GcRegionHeader>();

struct ReserveResult {
    value_ptr: RegionValuePointer,
    data_addr: usize,
}

pub enum LoadResult<'gc, T> {
    ForwardReference(RawGcPointer),
    LargeObjectReference(u32),
    Data(&'gc T),
}

impl GcRegion {
    fn try_reserve(
        &self,
        data_size: usize,
        trace_pointer: ErasedTraceFnPtr,
    ) -> Option<ReserveResult> {
        // this should not go there but instead on the loh
        debug_assert!(data_size < 65_000);

        let alloced_data_size = data_size + HEADER_SIZE;
        let free_top = unsafe { (*self.free_top.get()) as usize };

        let next_header_addr = (free_top - alloced_data_size) % HEADER_ALIGN;

        // performing this allocation would actually go over the limit of this region.
        if next_header_addr <= self.base_address as usize {
            return None;
        }

        let next_header_offset = (free_top - next_header_addr) as u32;

        let (data, data_type) = GcRegionEntry::InlineData { next_header_offset }.unparse();
        unsafe {
            core::ptr::write(*self.free_top.get(), {
                GcRegionHeader {
                    marked_alive: UnsafeCell::new(false),
                    data,
                    data_type,
                    data_size: data_size as u16,
                    trace_pointer,
                }
            });
            *self.free_top.get() = next_header_addr as *mut GcRegionHeader;
        }
        let data_addr = next_header_addr + core::mem::size_of::<GcRegionHeader>();

        let object_id = ((free_top - self.base_address as usize) / 8) as u16;

        Some(ReserveResult {
            data_addr,
            value_ptr: RegionValuePointer { object_id },
        })
    }

    pub fn try_alloc<D: Trace + Sized>(&self, data: D) -> Option<GcPointer<D>> {
        debug_assert!(core::mem::align_of_val(&data) <= core::mem::align_of::<GcRegionEntry>());

        fn raw_trace<D: Trace>(data: *const u8, mark_op: &mut dyn FnMut(&mut RawGcPointer)) {
            let data_ptr = data as *const D;
            let data_ref = unsafe { &*data_ptr };
            data_ref.trace(mark_op)
        }

        let ReserveResult {
            value_ptr,
            data_addr,
        } = self.try_reserve(core::mem::size_of_val(&data), raw_trace::<D>)?;

        let data_ptr = data_addr as *mut D;
        unsafe {
            core::ptr::write(data_ptr, data);
        }

        Some(GcPointer {
            ptr: RawGcPointer {
                region: self.region_id,
                value: value_ptr,
            },
            data: PhantomData,
        })
    }

    pub fn try_alloc_list(&self, count: u32) -> Option<(GcArrayPtr, &mut [Option<RawGcPointer>])> {
        type Optptr = Option<RawGcPointer>;

        fn array_trace(data: *const u8, mark_op: &mut dyn FnMut(&mut RawGcPointer)) {
            let item_count = unsafe { *(data as *const u32) };
            let item_pointer_begin =
                (data as usize + core::mem::size_of::<u32>()) as *mut Option<RawGcPointer>;
            let item_pointers =
                unsafe { core::slice::from_raw_parts_mut(item_pointer_begin, item_count as usize) };
            for item in item_pointers {
                if let Some(item) = item.as_mut() {
                    mark_op(item)
                }
            }
        }

        let data_size = count as usize * core::mem::size_of::<u32>();

        let ReserveResult {
            value_ptr,
            data_addr,
        } = self.try_reserve(data_size, array_trace)?;

        let num_item_ptr = data_addr as *mut u32;
        let items_begin = (data_addr + 4) as *mut MaybeUninit<Option<RawGcPointer>>;
        unsafe {
            let items_slice = core::slice::from_raw_parts_mut(items_begin, count as usize);
            for item in items_slice {
                item.write(None);
            }
            core::ptr::write(num_item_ptr, count);
        }

        let items_begin = items_begin as *mut Option<RawGcPointer>;
        let items_space = unsafe { core::slice::from_raw_parts_mut(items_begin, count as usize) };

        let ptr = GcArrayPtr {
            ptr: RawGcPointer {
                value: value_ptr,
                region: self.region_id,
            },
        };
        Some((ptr, items_space))
    }

    pub fn resolve<T: Trace>(&self, ptr: GcPointer<T>) -> LoadResult<'_, T> {
        debug_assert_eq!(self.region_id, ptr.ptr.region);

        let header_offset = ptr.ptr.value.object_id as usize * 8;
        let header = (self.base_address as usize + header_offset) as *mut GcRegionEntry;
        let header = unsafe { &*header };
        match header {
            GcRegionEntry::ForwardingPointer(f) => LoadResult::ForwardReference(*f),
            GcRegionEntry::InlineData { next_header_offset } => {
                let data_ptr = self.base_address as usize
                    + *next_header_offset as usize
                    + core::mem::size_of::<GcRegionHeader>();
                let data_ptr = data_ptr as *const T;
                LoadResult::Data(unsafe { &*data_ptr })
            }
            GcRegionEntry::LargeObjectReference(r) => LoadResult::LargeObjectReference(*r),
        }
    }
}
