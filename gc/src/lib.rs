mod heap;
mod heap_page;
mod object;
pub mod pointer;
pub mod specialized_types;

use std::{
    alloc::Layout,
    cell::Cell,
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
    scavenge_pending_set: Vec<Rc<Page>>,
}

impl PageSource for CollectionHandle<'_> {
    fn get_allocation_page(&mut self, generation: Generation) -> (&Page, &GenerationCounter) {
        self.pages.get_allocation_page(generation)
    }
    fn finish_allocation_page(&mut self, generation: Generation) {
        let new_page = self.pages.refresh_allocation_page(generation);
        self.scavenge_pending_set.entries[generation.0 as usize].push_back(new_page);
    }
    fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation {
        self.pages.generations.get_generation(ptr)
    }
}
impl PageSource for PromotionHandle<'_> {
    fn get_allocation_page(&mut self, generation: Generation) -> (&Page, &GenerationCounter) {
        self.pages.get_allocation_page(generation)
    }

    fn finish_allocation_page(&mut self, generation: Generation) {
        let new_page = self.pages.refresh_allocation_page(generation);
        self.scavenge_pending_set.push(new_page);
    }

    fn get_generation(&self, ptr: &RawHeapGcPointer) -> Generation {
        self.pages.generations.get_generation(ptr)
    }
}

trait PageSource {
    fn get_allocation_page(&mut self, generation: Generation) -> (&Page, &GenerationCounter);
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

#[derive(Default)]
struct GenerationCounter {
    current_live_objects: Cell<usize>,
    current_size_bytes: Cell<usize>,
    lifetime_allocation_count: Cell<usize>,
    lifetime_allocation_bytes: Cell<usize>,
}
impl GenerationCounter {
    pub fn record_allocation(&self, size: usize) {
        self.current_size_bytes
            .set(self.current_size_bytes.get() + size);
        self.current_live_objects
            .set(self.current_live_objects.get() + 1);
        self.lifetime_allocation_count
            .set(self.lifetime_allocation_count.get() + 1);
        self.lifetime_allocation_bytes
            .set(self.lifetime_allocation_bytes.get() + size);
    }

    fn mark_cleared(&self) {
        self.current_live_objects.set(0);
        self.current_size_bytes.set(0);
    }
}

struct AllocationPages {
    active_pages: [Rc<Page>; GC_NUM_GENERATIONS as usize],
    generations: GenerationAnalyzer,
    global: Arc<Mutex<Pagetracker>>,
    alloc_counters: [GenerationCounter; GC_NUM_GENERATIONS as usize],
    used_pages_current: [Vec<Rc<Page>>; GC_NUM_GENERATIONS as usize],
}

impl AllocationPages {
    pub fn suggest_collection_target_generation(&self) -> Generation {
        let mut budget = 1;
        for gen in 1..=GC_GEN_HIGHEST {
            budget *= 4;
            if self.used_pages_current[gen as usize].len() < budget {
                return Generation(gen - 1);
            }
        }

        // everything is over budget. do a full collection instead.
        Generation(GC_GEN_HIGHEST)
    }

    // refresh the allocation page for the current generation and return a reference to it.
    pub fn refresh_allocation_page(&mut self, generation: Generation) -> Rc<Page> {
        let gen = generation.0 as usize;
        let new_page = self
            .global
            .lock()
            .unwrap()
            .get_page(generation)
            .expect("heap exhausted");
        let new_page = Rc::new(new_page);

        self.active_pages[gen] = new_page.clone();
        self.used_pages_current[gen].push(new_page.clone());

        new_page
    }

    pub fn new(heap: Arc<Mutex<Pagetracker>>) -> Self {
        let mut guard = heap.lock().unwrap();
        let pages =
            std::array::from_fn(|gen| Rc::new(guard.get_page(Generation(gen as u8)).unwrap()));
        let generations = guard.get_analyzer().clone();
        drop(guard);
        let mut used_pages_current = core::array::from_fn(|_| Vec::new());

        // ensure that the current page tracker starts off containing the initial page set.
        used_pages_current
            .iter_mut()
            .zip(pages.iter())
            .for_each(|(vec, page)| vec.push(page.clone()));

        Self {
            active_pages: pages,
            generations,
            global: heap,
            alloc_counters: core::array::from_fn(|_| GenerationCounter::default()),
            used_pages_current,
        }
    }

    pub fn get_allocation_page(&mut self, generation: Generation) -> (&Page, &GenerationCounter) {
        let gen = generation.0 as usize;
        (&self.active_pages[gen], &self.alloc_counters[gen])
    }

    fn print_heap_sizes(&self) {
        println!("---------\nHeap statistics: ");
        for (gen, counter) in self.alloc_counters.iter().enumerate() {
            println!("\ngeneration {gen}: \n");
            println!("current live count: {}", counter.current_live_objects.get());
            println!("current live bytes: {}", counter.current_size_bytes.get());
            println!(
                "total alloc count:  {}",
                counter.lifetime_allocation_count.get()
            );
            println!(
                "total alloc bytes:  {}",
                counter.lifetime_allocation_bytes.get()
            );

            if counter.lifetime_allocation_count.get() == 0 {
                println!("....");
                break;
            }
        }
        println!("\n");
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

        // self.alloc_pages.refresh_allocation_page(Generation(0));
        // return;

        self.alloc_pages.print_heap_sizes();
        let target_generation = self.alloc_pages.suggest_collection_target_generation();
        println!("collecting gen {}", target_generation.0);

        let mut previous_pages = Vec::new();

        // clear out the previous alloc pages and used tracker.

        for gen in 0..=target_generation.0 {
            // move the entries of the previous tracker to a local vec.
            previous_pages.append(&mut self.alloc_pages.used_pages_current[gen as usize]);
            self.alloc_pages.refresh_allocation_page(Generation(gen));
        }

        // at this point, the previous page tracker is cleared and the alloc pages filled with
        // fresh pages for all generations to be collected.

        let mut handle = CollectionHandle {
            pages: &mut self.alloc_pages,
            scavenge_pending_set: ScavengePendingSet::new(),
        };

        // now, put all new pages into the bucket that will potentially be scavenged

        // we will never allocate anything in gen0 during GC. so we do not need to put the gen0
        // page into the scavenge set. We will however need to put the next higher gen into the set
        // because we might allocate there during collection.
        for gen in 1..=target_generation.next_higher().0 as usize {
            handle.scavenge_pending_set.entries[gen]
                .push_back(handle.pages.active_pages[gen].clone());
        }

        // trace all root pointers, copying their content to the new space.

        let mut num_roots = 0;
        inspect_roots(|root| {
            num_roots += 1;
            scavenge_heap_pointer(&mut handle, root, target_generation);
        });
        println!("traced {num_roots} roots");

        // now, scavenge the allocation pages, starting with the lowest generation and working our
        // way up. note that the current allocating page may very well be a part of the set to be
        // scavenged.

        for gen in 1..=target_generation.next_higher().0 {
            while let Some(next_page) =
                handle.scavenge_pending_set.entries[gen as usize].pop_front()
            {
                next_page
                    .scavenge_content(&mut handle, target_generation)
                    .unwrap();
            }
        }

        core::mem::drop(handle);
        // at this point, no references to any of the previous pages
        // should exist any longer.
        // this means we can return them to the heap for future reuse.
        {
            let mut guard = self.alloc_pages.global.lock().unwrap();
            guard.return_pages(
                previous_pages
                    .into_iter()
                    .filter_map(|page| Rc::<Page>::into_inner(page))
                    .map(|page| page.into_allocated()),
            )
        }

        for counter in self
            .alloc_pages
            .alloc_counters
            .iter()
            .take(target_generation.0 as usize + 1)
        {
            counter.mark_cleared();
        }

        println!("gc done.");
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
        let heap_ptr = match self.alloc_pages.active_pages[0]
            .try_alloc(data, &mut self.alloc_pages.alloc_counters[0])
        {
            Ok(ptr) => ptr,
            Err(value) => {
                self.clear_nursery()?;
                self.alloc_pages.active_pages[0]
                    .try_alloc(value, &mut self.alloc_pages.alloc_counters[0])
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
        let heap_ptr = self.with_retry(|gc_handle| {
            gc_handle.alloc_pages.active_pages[0]
                .try_alloc_slice(data, &mut gc_handle.alloc_pages.alloc_counters[0])
        })?;
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
        let heap_ptr = self.with_retry(|gc_handle| {
            let (page, counter) = gc_handle.get_nursery_page();
            page.try_alloc_vec(data, counter)
        })?;
        Ok(heap_ptr.root())
    }

    fn get_nursery_page(&self) -> (&Page, &GenerationCounter) {
        let page = self.alloc_pages.active_pages[0].as_ref();
        let counter = &self.alloc_pages.alloc_counters[0];
        (page, counter)
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
                scavenge_pending_set: Vec::new(),
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
