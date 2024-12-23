use std::{cell::Cell, sync::Arc};

use crate::{
    get_heap_base,
    heap_page::{HeapEntry, Page},
    pointer::RawHeapGcPointer,
    Generation, GC_GEN_HIGHEST, GC_NUM_GENERATIONS, GC_PAGE_SIZE,
};

#[derive(Clone)]
pub struct GenerationAnalyzer {
    inner: Arc<[Cell<Generation>]>,
}
impl GenerationAnalyzer {
    pub fn get_generation(&self, pointer: &RawHeapGcPointer) -> Generation {
        const ENTRIES_PER_PAGE: usize = GC_PAGE_SIZE / core::mem::size_of::<HeapEntry>();
        let page = pointer.to_bits() as usize / ENTRIES_PER_PAGE;
        self.inner[page].get()
    }
    fn set_generation(&self, page: *const u8, generation: Generation) {
        let page = (page as usize - get_heap_base() as usize) / GC_PAGE_SIZE;
        self.inner[page].replace(generation);
    }
}
// safe because all writing operations are protected by the mutex of the heap.
unsafe impl Send for GenerationAnalyzer {}

pub struct Pagetracker {
    base: *mut u8,
    size: usize,
    next_free_base: *mut u8,
    free_pages: Vec<AllocatedPage>,
    used_pages_current: [Vec<AllocatedPage>; GC_NUM_GENERATIONS],
    used_pages_previous: [Vec<AllocatedPage>; GC_NUM_GENERATIONS],
    generations: GenerationAnalyzer,
}
unsafe impl Send for Pagetracker {}

impl Pagetracker {
    pub fn new(base: *mut u8, size: usize) -> Self {
        let num_pages = size / GC_PAGE_SIZE;
        let generations = vec![Cell::new(Generation(u8::MAX)); num_pages]
            .into_boxed_slice()
            .into();
        let analyzer = GenerationAnalyzer { inner: generations };
        const EMPTYVEC: Vec<AllocatedPage> = Vec::new();
        Self {
            base,
            size,
            next_free_base: base,
            free_pages: Vec::new(),
            used_pages_current: [EMPTYVEC; GC_NUM_GENERATIONS],
            used_pages_previous: [EMPTYVEC; GC_NUM_GENERATIONS],
            generations: analyzer,
        }
    }
    pub fn get_analyzer(&self) -> &GenerationAnalyzer {
        &self.generations
    }

    /// compute the optimal collection target according to internal metrics
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

    /// rotate out the tracked pages in the specified generations.
    /// free the previously used pages and marke the currently used ones as previously used.
    pub fn rotate_used_pages_to_generation(&mut self, target_generation: Generation) {
        for gen in 0..=target_generation.0 {
            let gen = gen as usize;

            self.free_pages.append(&mut self.used_pages_previous[gen]);

            core::mem::swap(
                &mut self.used_pages_current[gen],
                &mut self.used_pages_previous[gen],
            );
        }
    }
    /// move the pages from the current generation to the previous one, ensuring the current set is empty.
    pub fn mark_current_pages_as_previous(&mut self, target_generation: Generation) {
        for gen in 0..=target_generation.0 as usize {
            self.used_pages_previous[gen].append(&mut self.used_pages_current[gen])
        }
    }

    pub fn get_page(&mut self, generation: Generation) -> Option<Page> {
        let page = self.free_pages.pop().or_else(|| self.grow_heap())?.zero();
        self.generations.set_generation(page.base, generation);
        self.used_pages_current[generation.0 as usize].push(AllocatedPage { base: page.base });

        Some(Page::new(page))
    }

    fn grow_heap(&mut self) -> Option<AllocatedPage> {
        let page = self.next_free_base;
        let next_free = unsafe { page.byte_add(GC_PAGE_SIZE) };

        if unsafe { next_free.byte_offset_from(self.base) } as usize > self.size {
            return None;
        } else {
            self.next_free_base = next_free;
            Some(AllocatedPage { base: page })
        }
    }
}

pub(crate) struct ZeroedPage {
    base: *mut u8,
}

impl ZeroedPage {
    /// zero the provided page and provide a type witness for it
    pub unsafe fn from_zeroed_addr(base: *mut u8) -> Self {
        Self { base }
    }
    pub fn into_base(self) -> *mut u8 {
        self.base
    }
}

/// a page
pub(crate) struct AllocatedPage {
    base: *mut u8,
}
impl From<Page> for AllocatedPage {
    fn from(value: Page) -> Self {
        Self {
            base: value.get_base(),
        }
    }
}
impl AllocatedPage {
    fn zero(self) -> ZeroedPage {
        unsafe {
            let slice = core::slice::from_raw_parts_mut(self.base, GC_PAGE_SIZE);
            slice.fill(0);
            ZeroedPage::from_zeroed_addr(self.base)
        }
    }
}
