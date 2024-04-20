use std::cell::UnsafeCell;

use crate::{heap::Heap, object::HeapObject, GcPointer};

struct HeapEntry {
    widening_function: usize,
}

struct Page {
    size: usize,
    base_address: *mut HeapEntry,
    free_top: UnsafeCell<*mut HeapEntry>,
    scavenge_top: UnsafeCell<*mut HeapEntry>,
}

impl Page {
    pub fn try_alloc<T: HeapObject>(&mut self) -> Result<GcPointer<T>, T> {
        todo!()
    }

    pub fn try_reserve(&self) -> Option<*mut u8> {}
}
