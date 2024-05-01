// mod heap;
pub mod heap_page;
mod object;
// mod rootset;
mod heap;
pub mod pointer;
pub mod specialized_types;

use std::{
    alloc::Layout,
    num::NonZeroI16,
    sync::{Arc, Mutex},
};

use heap::{GenerationAnalyzer, Heap};
use heap_page::Page;
use init::get_global_gc;
use object::HeapObject;
pub use pointer::{GcPointer, RawGcPointer};

pub(crate) const GC_PAGE_SIZE: usize = 4096 * 128;
pub(crate) const GC_GEN_HIGHEST: u8 = 8;

// impl<TData> GcPointer<TData> {
//     pub fn as_raw(&self) -> RawGcPointer {
//         self.ptr
//     }
// }

type RegionId = NonZeroI16;

// A pointer to the GC header of the next entry.

mod init;

struct GlobalGc {
    heap: Heap,
}

pub struct GcHandle {
    global: Arc<Mutex<GlobalGc>>,
    /// the gen0 page new allocations are placed in.
    nursery_page: Page,
}

const HEAP_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(1 << 32, 1 << 32) };

static mut HEAP_BASE: *mut u8 = core::ptr::null_mut();

pub(crate) fn get_heap_base() -> *mut u8 {
    unsafe { HEAP_BASE }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Generation(u8);
impl Generation {
    fn next_higher(&self) -> Generation {
        Generation(self.0.wrapping_add(1).min(GC_GEN_HIGHEST))
    }
}

impl PartialOrd for Generation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for Generation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

pub enum GcError {
    OutOfPages,
    ObjectBiggerThanPage,
}
pub type GcResult<T> = Result<T, GcError>;

struct CollectionHandle<'a> {
    fun: &'a (),
    pub generation_table: GenerationAnalyzer,
}
impl<'a> CollectionHandle<'a> {
    pub fn get_allocation_page(&mut self, generation: Generation) -> &'a mut Page {
        todo!()
    }
    pub fn finish_allocation_page(&mut self, generation: Generation) {
        todo!()
    }
}

pub fn with_gc<F, R>(action: F) -> GcResult<R>
where
    F: FnOnce(&mut GcHandle) -> R + Send,
    R: Send,
{
    let mut manager = GcHandle::get()?;
    let res = action(&mut manager);
    Ok(res)
}

impl GcHandle {
    fn clear_nursery(&mut self) -> GcResult<()> {
        // self.nursery_page.scavenge_content(gc_handle, 0)
        Ok(())
    }

    pub fn alloc<TData: HeapObject + 'static>(
        &mut self,
        data: TData,
    ) -> GcResult<GcPointer<TData>> {
        let heap_ptr = match self.nursery_page.try_alloc(data) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.clear_nursery()?;
                self.nursery_page
                    .try_alloc(value)
                    .map_err(|_| GcError::ObjectBiggerThanPage)?
            }
        };
        Ok(heap_ptr.root())
    }

    fn get() -> GcResult<Self> {
        let global = get_global_gc();
        let nursery_page = global
            .lock()
            .unwrap()
            .heap
            .get_page(Generation(0))
            .ok_or(GcError::OutOfPages)?;
        Ok(Self {
            global,
            nursery_page,
        })
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
}
