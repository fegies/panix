// mod heap;
pub mod heap_page;
mod object;
// mod rootset;
pub mod pointer;
pub mod specialized_types;

use std::{alloc::Layout, num::NonZeroI16};

use heap_page::Page;
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

pub struct MemoryManager {
    heap_backing: *mut u8,
}

const HEAP_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(1 << 32, 1 << 32) };

static mut HEAP_BASE: *mut u8 = core::ptr::null_mut();

pub(crate) fn get_heap_base() -> *mut u8 {
    unsafe { HEAP_BASE }
}

enum GcError {
    OutOfMemory,
}

struct CollectionHandle<'a> {
    fun: &'a (),
}
impl<'a> CollectionHandle<'a> {
    pub fn get_allocation_page(&mut self) -> &'a mut Page {
        todo!()
    }
}

impl MemoryManager {
    pub fn alloc<TData: HeapObject>(&self, data: TData) -> GcPointer<TData> {
        todo!()
    }

    pub fn get() -> Self {
        let ptr = unsafe { std::alloc::alloc(HEAP_LAYOUT) };
        unsafe {
            HEAP_BASE = ptr;
        }
        println!("allocated heap at {ptr:?}");
        Self { heap_backing: ptr }
    }

    pub fn load_raw<'s>(&'s self, ptr: &RawGcPointer) -> &'s dyn HeapObject {
        ptr.get_heapref().resolve().load()
    }

    pub fn load<'s, TData>(&'s self, ptr: &GcPointer<TData>) -> &'s TData {
        let data_ptr = ptr.as_ref().get_heapref().resolve().get_dataref() as *const ();
        unsafe { &*data_ptr.cast::<TData>() }
    }

    /// replace the value the pointer points to to instead refer to the new value.
    /// Since this operation may trigger a garbage collection due to the promotion of new_value
    /// the gc needs to be taken by mut.
    pub fn replace<'s, TData>(&mut self, ptr: GcPointer<TData>, new_value: GcPointer<TData>) {
        todo!()
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
