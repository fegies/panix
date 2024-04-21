pub(crate) mod constants;
// mod heap;
pub mod heap_page;
mod object;
// mod rootset;
pub mod specialized_types;

use std::{alloc::Layout, marker::PhantomData, num::NonZeroI16};

use heap_page::HeapEntry;

pub struct GcPointer<TData> {
    ptr: RawGcPointer,
    data: PhantomData<TData>,
}

pub(crate) const GC_PAGE_SIZE: usize = 4096 * 8;

impl<T> Clone for GcPointer<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            data: PhantomData,
        }
    }
}

impl<TData> GcPointer<TData> {
    pub fn as_raw(&self) -> RawGcPointer {
        self.ptr
    }
}

pub struct GcArrayPtr {
    ptr: RawGcPointer,
}

type RegionId = NonZeroI16;

// A pointer to the GC header of the next entry.
#[derive(Clone, Copy)]
pub struct RawGcPointer {
    content: u32,
}

impl RawGcPointer {
    /// Create a new instance of this pointer.
    /// This function is only valid if the heap address
    /// is properly allocated on the gc heap and
    /// valid.
    pub(crate) unsafe fn from_heap_addr(raw_ptr: *mut HeapEntry) -> Self {
        let value = (raw_ptr as usize >> 32) as u32;
        Self { content: value }
    }
}

// struct MemoryManager {}

// impl MemoryManager {
//     pub fn load_mut<TData: Trace>(&self, ptr: GcPointer<TData>) -> &mut TData {
//         todo!()
//     }
//     pub fn load<TData: Trace>(&self, ptr: GcPointer<TData>) -> &TData {
//         todo!()
//     }

//     pub fn alloc<TData: Trace>(&self, data: TData) -> GcPointer<TData> {
//         todo!()
//     }

//     /// announce that referencing_pointer now has a reference to data_ptr.
//     /// This gives the GC an opportunity to promote the object to that generation immediately.
//     pub fn announce(
//         &self,
//         referencing_pointer: GcPointer<impl Trace>,
//         data_ptr: &mut GcPointer<impl Trace>,
//     ) {
//     }
// }

pub fn init() {
    let heap_layout = Layout::from_size_align(1 << 32, 1 << 32).unwrap();

    unsafe {
        let ptr = std::alloc::alloc(heap_layout);
        println!("allocated heap at {ptr:?}");
    }
}
