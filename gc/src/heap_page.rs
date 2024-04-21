use std::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit};

use crate::{
    object::{widen, HeapObject, WideningAccessor},
    GcPointer, RawGcPointer, GC_PAGE_SIZE,
};

pub(crate) struct HeapEntry {
    widening_function: WideningAccessor,
}
impl HeapEntry {
    #[inline]
    pub(crate) fn for_object<T: HeapObject + 'static>(_object: &T) -> Self {
        Self {
            widening_function: widen::<T>,
        }
    }
}

pub(crate) struct Page {
    /// the lowest valid address that belongs to this page
    base_address: *const u8,
    /// the latest non-free byte of this page.
    /// (e.g. a value 1 past the free range)
    free_top: UnsafeCell<*mut HeapEntry>,
    // scavenge_top: UnsafeCell<*mut HeapEntry>,
}

impl Page {
    pub(crate) fn new(base_address: *const u8) -> Self {
        debug_assert!(base_address as usize % GC_PAGE_SIZE == 0);

        let free_top = unsafe { base_address.add(GC_PAGE_SIZE) } as *mut HeapEntry;
        Self {
            base_address,
            free_top: UnsafeCell::new(free_top),
        }
    }

    /// attempt to move the provided object into this heap page.
    /// Will return the object on failure.
    pub fn try_alloc<T: HeapObject + 'static>(&mut self, object: T) -> Result<GcPointer<T>, T> {
        if let Some((header_ptr, data_ptr)) =
            self.try_reserve(object.allocation_size(), core::mem::align_of_val(&object))
        {
            let cast_data_ptr = data_ptr as *mut T;
            unsafe {
                core::ptr::write(cast_data_ptr, object);
                core::ptr::write(header_ptr, HeapEntry::for_object(&*cast_data_ptr));
                let gc_ptr = RawGcPointer::from_heap_addr(header_ptr);
                Ok(GcPointer {
                    ptr: gc_ptr,
                    data: PhantomData,
                })
            }
        } else {
            Err(object)
        }
    }

    /// attempt to reserve enough space for the requested allocation.
    /// The requested alignment must be a power of 2.
    ///
    /// The returned data pointer will always point to the beginning of the allocation.
    pub fn try_reserve(
        &self,
        requested_space: usize,
        requested_alignment: usize,
    ) -> Option<(*mut HeapEntry, *mut u8)> {
        debug_assert!(requested_alignment.is_power_of_two());

        let free_top = unsafe { *self.free_top.get() } as *mut u8;

        #[cold]
        fn align_down_general(ptr: *mut u8, alignment: usize) -> *mut u8 {
            let val = (ptr as usize / alignment) * alignment;
            val as *mut u8
        }

        // ensure that we align to at least the alignment of our heap items.
        // if we align to more, it would still be fine for the header.
        let data_alignment = core::mem::align_of::<HeapEntry>().max(requested_alignment);

        // compute the pointer to our data object.
        // because we ensured the alignment, it has also legal alignment for the object header.
        let data_ptr = {
            // first requirement: enough space in the allocation.
            let ptr = unsafe { free_top.sub(requested_space) };
            // second requirement: enough alignment
            const HEAP_ENTRY_ALIGN: usize = core::mem::align_of::<HeapEntry>();
            if requested_alignment <= HEAP_ENTRY_ALIGN {
                // align to the minimum object header.
                ((ptr as usize / HEAP_ENTRY_ALIGN) * HEAP_ENTRY_ALIGN) as *mut u8
            } else {
                // align to the requested alignment, as it exceeds the minimum.
                // we do this in an extra function to not pay the cost for the div in the common path
                align_down_general(ptr, data_alignment)
            }
        };

        // this is legal because we ensured that the data pointer is at least aligned
        // as much as the object header needs.
        let header_ptr = unsafe { (data_ptr as *mut HeapEntry).sub(1) };

        if (header_ptr as usize) < self.base_address as usize {
            // our allocation is legal, so perform it by writing the free top.
            unsafe { *self.free_top.get() = header_ptr };
            // and return the pointer to the allocation
            Some((header_ptr, data_ptr))
        } else {
            // performing this allocation would result in us going off the page.
            None
        }
    }
}
