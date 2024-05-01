use std::{cell::Cell, sync::Arc};

use crate::{
    heap_page::{HeapEntry, Page},
    pointer::RawHeapGcPointer,
    Generation, GC_GEN_HIGHEST, GC_PAGE_SIZE,
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
        let page = page as usize / GC_PAGE_SIZE;
        self.inner[page].replace(generation);
    }
}
// safe because all writing operations are protected by the mutex of the heap.
unsafe impl Send for GenerationAnalyzer {}

pub struct Pagetracker {
    base: *mut u8,
    size: usize,
    next_free_base: *mut u8,
    free_pages: Vec<ZeroedPage>,
    used_pages: [Vec<AllocatedInactivePage>; GC_GEN_HIGHEST as usize],
    generations: GenerationAnalyzer,
}
unsafe impl Send for Pagetracker {}

impl Pagetracker {
    pub fn new(base: *mut u8, size: usize) -> Self {
        let num_pages = size / GC_PAGE_SIZE;
        let generations = vec![Cell::new(Generation(255)); num_pages]
            .into_boxed_slice()
            .into();
        let analyzer = GenerationAnalyzer { inner: generations };
        const EMPTYVEC: Vec<AllocatedInactivePage> = Vec::new();
        Self {
            base,
            size,
            next_free_base: base,
            free_pages: Vec::new(),
            used_pages: [EMPTYVEC; GC_GEN_HIGHEST as usize],
            generations: analyzer,
        }
    }
    pub fn get_analyzer(&self) -> &GenerationAnalyzer {
        &self.generations
    }

    pub fn get_page(&mut self, generation: Generation) -> Option<Page> {
        let page = self.free_pages.pop().or_else(|| self.grow_heap())?;
        self.generations.set_generation(page.base, generation);
        let page = Page::new(page.base);
        Some(page)
    }

    pub fn return_pages(&mut self, pages: impl Iterator<Item = ZeroedPage>) {
        self.free_pages.extend(pages)
    }
    pub fn track_used_page(&mut self, page: Page, generation: Generation) {
        self.used_pages[generation.0 as usize].push(page.into())
    }

    fn grow_heap(&mut self) -> Option<ZeroedPage> {
        let page = self.next_free_base;
        let next_free = unsafe { page.byte_add(GC_PAGE_SIZE) };

        if unsafe { next_free.byte_offset_from(self.base) } as usize > self.size {
            return None;
        } else {
            self.next_free_base = next_free;
            // all pages tracked as free must be zeroed.
            Some(ZeroedPage { base: page })
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
}

/// a page
pub(crate) struct AllocatedInactivePage {
    base: *mut u8,
}
impl From<Page> for AllocatedInactivePage {
    fn from(value: Page) -> Self {
        Self {
            base: value.get_base(),
        }
    }
}
