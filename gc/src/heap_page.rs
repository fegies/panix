use crate::{
    object::{widen, HeapObject, WideningAccessor},
    pointer::{HeapGcPointer, RawHeapGcPointer},
    GcPointer, RawGcPointer, GC_PAGE_SIZE,
};
use std::cell::UnsafeCell;

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
    pub(crate) fn load(&mut self) -> &mut dyn HeapObject {
        let data_reference = unsafe { &mut *((self as *mut HeapEntry).add(1) as *mut u8) };
        (self.widening_function)(data_reference)
    }
}

pub(crate) struct Page {
    /// the lowest valid address that belongs to this page
    base_address: *const u8,
    /// the latest non-free byte of this page.
    /// (e.g. a value 1 past the free range)
    free_top: UnsafeCell<*mut HeapEntry>,
}

struct ScavengeStatus {
    /// a past-the-end pointer to the highest non-scavengable memory address
    scavenge_end: *const u8,
    /// a pointer to the address that scavenging this page began at.
    scavenge_start: *const u8,
    /// a pointer to the current heap entry being scavenged.
    scavenge_current: *const u8,
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

    pub(crate) fn scavenge_content(&self) {
        // a past-the-end pointer to the highest non-scavengable memory address
        let mut scavenge_end_addr = self.base_address as usize + GC_PAGE_SIZE;
        // a pointer to the place we began to scavenge.
        let mut scavenge_start = unsafe { *self.free_top.get() };

        while (scavenge_start as usize) < scavenge_end_addr {
            let mut scavenge_current = scavenge_start as *mut HeapEntry;
            while (scavenge_current as usize) < scavenge_end_addr {
                // scavenge pointed-to object.
                let object = unsafe {
                    // the pointed_to object is actually a hole in the heap caused by alignment.
                    // instead, search for the next value by searching for the next properly-aligned heap entry
                    if core::ptr::read(scavenge_current as *const usize) == 0 {
                        scavenge_current = scavenge_current.add(1);
                        continue;
                    }
                    (&mut *scavenge_current).load()
                };

                object.trace(&mut |gc_pointer| todo!("implement trace callback"));
                scavenge_current = advance_entrypointer(scavenge_current, object.allocation_size());
            }
            // set the end to where we started as everything up to here has been scavenged
            scavenge_end_addr = scavenge_start as usize;
            // and the start to the allocation pointer, as there may have been additional objects added that need to be scavenged too
            scavenge_start = unsafe { *self.free_top.get() };
        }

        fn advance_entrypointer(ptr: *mut HeapEntry, allocation_size: usize) -> *mut HeapEntry {
            let mut addr = ptr as usize;
            // the header itself
            addr += core::mem::size_of::<HeapEntry>();
            // the allocated data
            addr += allocation_size;

            // ensure the correct alignment to account for padding.
            const ALIGN: usize = core::mem::align_of::<HeapEntry>();
            let align = addr % ALIGN;
            if align > 0 {
                addr += ALIGN - align;
            }

            addr as *mut HeapEntry
        }
    }

    /// attempt to move the provided object into this heap page.
    /// Will return the object on failure.
    pub fn try_alloc<T: HeapObject + 'static>(&mut self, object: T) -> Result<HeapGcPointer<T>, T> {
        if let Some((header_ptr, data_ptr)) =
            self.try_reserve(object.allocation_size(), core::mem::align_of_val(&object))
        {
            let cast_data_ptr = data_ptr as *mut T;
            unsafe {
                core::ptr::write(cast_data_ptr, object);
                core::ptr::write(header_ptr, HeapEntry::for_object(&*cast_data_ptr));

                // unroot all data pointers in there to ensure we don't get leaks
                (*cast_data_ptr).trace(&mut |ptr| ptr.unroot());

                let gc_ptr = RawHeapGcPointer::from_addr(header_ptr);
                Ok(HeapGcPointer::from_raw_unchecked(gc_ptr))
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

            // zero out the new allocation.
            unsafe {
                let len = free_top as usize - header_ptr as usize;
                core::slice::from_raw_parts_mut(header_ptr as *mut u8, len)
            }
            .fill(0);

            unsafe { *self.free_top.get() = header_ptr };
            // and return the pointer to the allocation
            Some((header_ptr, data_ptr))
        } else {
            // performing this allocation would result in us going off the page.
            None
        }
    }
}
