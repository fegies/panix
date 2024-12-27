// mod heap;
mod heap_page;
mod object;
// mod rootset;
mod heap;
pub mod pointer;
pub mod specialized_types;

use std::{
    alloc::Layout,
    collections::VecDeque,
    rc::Rc,
    sync::{Arc, Mutex},
};

use heap::{GenerationAnalyzer, Pagetracker};
use heap_page::{scavenge_heap_pointer, Page};
use init::get_global_gc;
use object::HeapObject;
use pointer::{inspect_roots, RawHeapGcPointer};

pub use object::{Trace, TraceCallback};
pub use pointer::{GcPointer, RawGcPointer};
use specialized_types::{array::Array, string::SimpleGcString};
use thiserror::Error;

use crate::heap_page::promote_object;

pub(crate) const GC_PAGE_SIZE: usize = 4096 * 128;
pub(crate) const GC_GEN_HIGHEST: u8 = 7;
pub(crate) const GC_NUM_GENERATIONS: usize = GC_GEN_HIGHEST as usize + 1;

mod init;

pub struct GcHandle {
    alloc_pages: AllocationPages,
}

pub type GcString = GcPointer<SimpleGcString>;

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

#[derive(Debug, Error, Clone)]
pub enum GcError {
    #[error("Ran out of free pages when allocating")]
    OutOfPages,
    #[error("The requested object is too big")]
    ObjectBiggerThanPage,
    #[error("tried to access an object out of the valid range")]
    AccessOutOfRange,
}
pub type GcResult<T> = Result<T, GcError>;

struct ScavengePendingSet {
    entries: [VecDeque<Rc<Page>>; GC_GEN_HIGHEST as usize],
}
impl ScavengePendingSet {
    pub const fn new() -> Self {
        const EMPTY_QUEUE: VecDeque<Rc<Page>> = VecDeque::new();
        Self {
            entries: [EMPTY_QUEUE; GC_GEN_HIGHEST as usize],
        }
    }
}

struct CollectionHandle<'a> {
    pages: &'a mut AllocationPages,
    scavenge_pending_set: ScavengePendingSet,
}
struct PromotionHandle<'a> {
    pages: &'a mut AllocationPages,
}

impl PageSource for CollectionHandle<'_> {
    fn get_allocation_page(&mut self, generation: Generation) -> &Page {
        self.pages.get_allocation_page(generation)
    }
    fn finish_allocation_page(&mut self, generation: Generation) {
        let gen = generation.0 as usize;

        let new_page = self
            .pages
            .global
            .lock()
            .unwrap()
            .get_page(generation)
            .unwrap();
        let new_page = Rc::new(new_page);
        self.pages.pages[gen] = new_page.clone();
        self.scavenge_pending_set.entries[gen].push_back(new_page);
    }
    fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation {
        self.pages.generations.get_generation(ptr)
    }
}
impl PageSource for PromotionHandle<'_> {
    fn get_allocation_page(&mut self, generation: Generation) -> &Page {
        self.pages.get_allocation_page(generation)
    }

    fn finish_allocation_page(&mut self, generation: Generation) {
        let gen = generation.0 as usize;
        let new_page = self
            .pages
            .global
            .lock()
            .unwrap()
            .get_page(generation)
            .unwrap();
        self.pages.pages[gen] = Rc::new(new_page);
    }

    fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation {
        self.pages.generations.get_generation(ptr)
    }
}

trait PageSource {
    fn get_allocation_page(&mut self, generation: Generation) -> &Page;
    fn finish_allocation_page(&mut self, generation: Generation);
    fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation;
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
    pages: [Rc<Page>; GC_GEN_HIGHEST as usize],
    generations: GenerationAnalyzer,
    global: Arc<Mutex<Pagetracker>>,
}
impl AllocationPages {
    pub fn new(heap: Arc<Mutex<Pagetracker>>) -> Self {
        let mut guard = heap.lock().unwrap();
        let pages =
            std::array::from_fn(|gen| Rc::new(guard.get_page(Generation(gen as u8)).unwrap()));
        let generations = guard.get_analyzer().clone();
        drop(guard);
        Self {
            pages,
            generations,
            global: heap,
        }
    }
    pub fn get_fresh_allocpages(&mut self) -> Generation {
        let mut heap = self.global.lock().unwrap();
        let target_generation = heap.suggest_collection_target_generation();

        heap.mark_current_pages_as_previous(target_generation);

        for gen in 0..=target_generation.0 {
            let gen = Generation(gen);
            self.pages[gen.0 as usize] = Rc::new(heap.get_page(gen).unwrap());
        }

        target_generation
    }

    pub fn get_allocation_page(&mut self, generation: Generation) -> &Page {
        &self.pages[generation.0 as usize]
    }
}

impl Drop for AllocationPages {
    fn drop(&mut self) {
        let mut heap = self.global.lock().unwrap();
        heap.mark_current_pages_as_previous(Generation(GC_GEN_HIGHEST));
        heap.rotate_used_pages_to_generation(Generation(GC_GEN_HIGHEST));
    }
}

impl GcHandle {
    fn with_retry<TResult>(
        &mut self,
        mut func: impl FnMut(&mut GcHandle) -> Option<TResult>,
    ) -> GcResult<TResult> {
        match func(self) {
            Some(res) => Ok(res),
            None => {
                self.clear_nursery()?;
                func(self).ok_or(GcError::ObjectBiggerThanPage)
            }
        }
    }
    fn run_gc(&mut self) {
        println!("gc triggered!");
        let target_generation = self.alloc_pages.get_fresh_allocpages();

        let mut handle = CollectionHandle {
            pages: &mut self.alloc_pages,
            scavenge_pending_set: ScavengePendingSet::new(),
        };

        // we will never allocate anything in gen0 during GC
        for gen in 1..=target_generation.next_higher().0 as usize {
            handle.scavenge_pending_set.entries[gen].push_back(handle.pages.pages[gen].clone());
        }

        inspect_roots(|root| {
            scavenge_heap_pointer(&mut handle, root, target_generation);
        });

        for gen in 1..=target_generation.next_higher().0 {
            while let Some(next_page) =
                handle.scavenge_pending_set.entries[gen as usize].pop_front()
            {
                next_page
                    .scavenge_content(&mut handle, target_generation)
                    .unwrap();
            }
        }

        self.alloc_pages
            .global
            .lock()
            .unwrap()
            .rotate_used_pages_to_generation(target_generation);
    }

    pub fn force_collect(&mut self) {
        self.run_gc();
    }

    fn clear_nursery(&mut self) -> GcResult<()> {
        self.run_gc();
        Ok(())
    }

    pub fn alloc<TData: HeapObject + 'static>(
        &mut self,
        data: TData,
    ) -> GcResult<GcPointer<TData>> {
        let heap_ptr = match self.get_nursery_page().try_alloc(data) {
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

    /// Allocates an array on the heap, then filles it by cloning the
    /// contents of the passed slice.
    pub fn alloc_slice<TData: HeapObject + Clone + 'static>(
        &mut self,
        data: &[TData],
    ) -> GcResult<GcPointer<Array<TData>>> {
        let heap_ptr =
            self.with_retry(|gc_handle| gc_handle.get_nursery_page().try_alloc_slice(data))?;
        Ok(heap_ptr.root())
    }

    /// Allocate an array of the same length as the passed vector, then moves
    /// the vector entries into the array.
    /// On success the vector is empty.
    /// On error, the vector will not be modified.
    pub fn alloc_vec<TData: HeapObject + 'static>(
        &mut self,
        data: &mut Vec<TData>,
    ) -> GcResult<GcPointer<Array<TData>>> {
        let heap_ptr =
            self.with_retry(|gc_handle| gc_handle.get_nursery_page().try_alloc_vec(data))?;
        Ok(heap_ptr.root())
    }

    fn get_nursery_page(&self) -> &Page {
        self.alloc_pages.pages[0].as_ref()
    }

    fn get() -> GcResult<Self> {
        let global = get_global_gc();
        let alloc_pages = AllocationPages::new(global);
        Ok(Self { alloc_pages })
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
        ptr: &GcPointer<TData>,
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
            let mut handle = PromotionHandle {
                pages: &mut self.alloc_pages,
            };
            let new_value = promote_object(
                &mut handle,
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
