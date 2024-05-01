// mod heap;
mod heap_page;
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

use heap::{GenerationAnalyzer, Pagetracker};
use heap_page::{scavenge_object, Page};
use init::get_global_gc;
use object::HeapObject;
use pointer::{inspect_roots, RawHeapGcPointer};
pub use pointer::{GcPointer, RawGcPointer};

use crate::heap_page::promote_object;

pub(crate) const GC_PAGE_SIZE: usize = 4096 * 128;
pub(crate) const GC_GEN_HIGHEST: u8 = 8;

mod init;

struct GlobalGc {
    heap: Pagetracker,
}

pub struct GcHandle {
    global: Arc<Mutex<GlobalGc>>,
    alloc_pages: AllocationPages,
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
    pages: &'a mut AllocationPages,
    global: &'a Mutex<GlobalGc>,
}
impl<'a> CollectionHandle<'a> {
    pub fn get_allocation_page(&mut self, generation: Generation) -> &mut Page {
        self.pages.get_allocation_page(generation)
    }
    pub fn finish_allocation_page(&mut self, generation: Generation) {
        let mut guard = self.global.lock().unwrap();
        self.pages
            .finish_allocation_page(generation, &mut guard.heap);
    }
    pub fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation {
        self.pages.generations.get_generation(ptr)
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

struct AllocationPages {
    pages: [Page; GC_GEN_HIGHEST as usize],
    scavenge_pending_set: [Vec<Page>; GC_GEN_HIGHEST as usize],
    generations: GenerationAnalyzer,
}
impl AllocationPages {
    pub fn new(heap: &mut Pagetracker) -> Self {
        todo!()
    }
    pub fn get_fresh_allocpages(&mut self, heap: &mut Pagetracker, target_generation: Generation) {
        for gen in 0..target_generation.0 + 1 {
            let gen = Generation(gen);
            let old_page =
                core::mem::replace(&mut self.pages[gen.0 as usize], heap.get_page(gen).unwrap());
            heap.track_used_page(old_page, gen);
        }
    }

    pub fn get_allocation_page(&mut self, generation: Generation) -> &mut Page {
        &mut self.pages[generation.0 as usize]
    }
    pub fn finish_allocation_page(&mut self, generation: Generation, heap: &mut Pagetracker) {
        let prev_page = core::mem::replace(
            self.get_allocation_page(generation),
            heap.get_page(generation).unwrap(),
        );
        heap.track_used_page(prev_page, generation);
    }
}

impl GcHandle {
    fn run_gc(&mut self, target_generation: Generation) {
        let mut guard = self.global.lock().unwrap();

        self.alloc_pages
            .get_fresh_allocpages(&mut guard.heap, target_generation);

        drop(guard);

        let mut handle = CollectionHandle {
            pages: &mut self.alloc_pages,
            global: &self.global,
        };

        inspect_roots(|root| {
            let gen = handle.get_generation(root);
            if gen > target_generation {
                return;
            }

            scavenge_object(&mut handle, root, target_generation);
        })

        // TODO: scavenge only once
    }

    fn clear_nursery(&mut self) -> GcResult<()> {
        self.run_gc(Generation(0));
        Ok(())
    }

    pub fn alloc<TData: HeapObject + 'static>(
        &mut self,
        data: TData,
    ) -> GcResult<GcPointer<TData>> {
        let heap_ptr = match self.alloc_pages.pages[0].try_alloc(data) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.clear_nursery()?;
                self.alloc_pages.pages[0]
                    .try_alloc(value)
                    .map_err(|_| GcError::ObjectBiggerThanPage)?
            }
        };
        Ok(heap_ptr.root())
    }

    fn get() -> GcResult<Self> {
        let global = get_global_gc();
        let mut guard = global.lock().unwrap();
        let alloc_pages = AllocationPages::new(&mut guard.heap);
        drop(guard);
        Ok(Self {
            global,
            alloc_pages,
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
    ///
    /// will return a pointer to the new value.
    pub fn replace<TData>(
        &mut self,
        ptr: GcPointer<TData>,
        new_value: GcPointer<TData>,
    ) -> GcPointer<TData> {
        let target_generation = self
            .alloc_pages
            .generations
            .get_generation(&ptr.as_ref().get_heapref());

        let replacement_gen = self
            .alloc_pages
            .generations
            .get_generation(&new_value.as_ref().get_heapref());

        if replacement_gen >= target_generation {
            // the replacement is already in a generation that will live at least as long as the source.
            // so we can just set up the forwarding.
            ptr.as_ref()
                .get_heapref()
                .resolve_mut()
                .forward_to(new_value.as_ref().get_heapref());
            new_value
        } else {
            // the source reference would outlive the replacement value.
            // to ensure this does not happen, we need to promote the value to the generation the source is in.
            let new_value = promote_object(
                &mut CollectionHandle {
                    pages: &mut self.alloc_pages,
                    global: &self.global,
                },
                &mut new_value.as_ref().get_heapref(),
                replacement_gen,
            );

            ptr.as_ref()
                .get_heapref()
                .resolve_mut()
                .forward_to(new_value.clone());

            let ptr = new_value.root();
            unsafe { GcPointer::<TData>::from_raw_unchecked(ptr) }
        }
    }
}
