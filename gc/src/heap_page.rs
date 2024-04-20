use std::{cell::UnsafeCell, marker::PhantomData};

use crate::{object::HeapObject, GcPointer, RawGcPointer};

struct HeapEntry {
    widening_function: usize,
}

pub(crate) struct Page {
    size: usize,
    base_address: *const (),
    free_top: UnsafeCell<*mut HeapEntry>,
    scavenge_top: UnsafeCell<*mut HeapEntry>,
}

impl Page {
    /// attempt to move the provided object into this heap page.
    /// Will return the object on failure.
    pub fn try_alloc<T: HeapObject>(&mut self, object: T) -> Result<GcPointer<T>, T> {
        if let Some(data_ptr) = self.try_reserve(object.allocation_size()) {
            let cast_data_ptr = data_ptr as *mut T;
            unsafe {
                core::ptr::write(cast_data_ptr, object);
                let gc_ptr = RawGcPointer::from_heap_addr(data_ptr);
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
    pub fn try_reserve(&self, requested_space: usize) -> Option<*mut u8> {
        let mut data_location = unsafe { *self.free_top.get() };
        let mut free_top = data_location as usize;
        free_top -= requested_space;
        free_top -= core::mem::size_of::<HeapEntry>();
        // align the new free pointer to 8 bytes.
        free_top = (free_top / 8) * 8;

        // and now see if the new freet op is actually valid...
        if free_top >= self.base_address as usize {
            // the allocated object is still within the bounds of the page
            unsafe { core::ptr::write(self.free_top.get(), free_top as *mut HeapEntry) };

            todo!()
        } else {
            // the page is full!
            None
        }
    }
}
