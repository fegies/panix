pub(crate) mod constants;
// mod heap;
pub mod heap_page;
mod object;
// mod rootset;
pub mod pointer;
pub mod specialized_types;

use std::{alloc::Layout, num::NonZeroI16};

use object::HeapObject;
pub use pointer::{GcPointer, RawGcPointer};

pub(crate) const GC_PAGE_SIZE: usize = 4096 * 128;

// impl<TData> GcPointer<TData> {
//     pub fn as_raw(&self) -> RawGcPointer {
//         self.ptr
//     }
// }

type RegionId = NonZeroI16;

// A pointer to the GC header of the next entry.

struct MemoryManager {
    heap_backing: *mut u8,
}

const HEAP_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(1 << 32, 1 << 32) };

static mut HEAP_BASE: *mut u8 = core::ptr::null_mut();

pub(crate) fn get_heap_base() -> *mut u8 {
    unsafe { HEAP_BASE }
}

impl MemoryManager {
    pub fn alloc<TData: HeapObject>(&self, data: TData) -> GcPointer<TData> {
        todo!()
    }

    pub fn new() -> Self {
        let ptr = unsafe { std::alloc::alloc(HEAP_LAYOUT) };
        unsafe {
            HEAP_BASE = ptr;
        }
        println!("allocated heap at {ptr:?}");
        Self { heap_backing: ptr }
    }

    // /// announce that referencing_pointer now has a reference to data_ptr.
    // /// This gives the GC an opportunity to promote the object to that generation immediately.
    // pub fn announce(
    //     &self,
    //     referencing_pointer: GcPointer<impl Trace>,
    //     data_ptr: &mut GcPointer<impl Trace>,
    // ) {
    // }
}

impl Drop for MemoryManager {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.heap_backing, HEAP_LAYOUT);
        }
    }
}

pub fn init() {
    MemoryManager::new();
}
