use std::{cell::Cell, sync::Arc};

use crate::{
    get_heap_base, heap_page::Page, init::get_global_gc, pointer::RawHeapGcPointer, Generation,
    GC_PAGE_SIZE,
};

#[derive(Clone)]
pub struct GenerationAnalyzer {
    inner: Arc<[Cell<Generation>]>,
}
impl GenerationAnalyzer {
    pub fn get_generation(&self, pointer: &RawHeapGcPointer) -> Generation {
        let page = pointer.to_heap_offset() / GC_PAGE_SIZE;
        self.inner[page].get()
    }

    fn set_generation(&self, page: *const u8, generation: Generation) {
        let page = (page as usize - get_heap_base() as usize) / GC_PAGE_SIZE;
        self.inner[page].set(generation);
    }
}

// safe because all writing operations are protected by the mutex of the heap.
unsafe impl Send for GenerationAnalyzer {}

pub struct Pagetracker {
    base: *mut u8,
    size: usize,
    next_free_base: *mut u8,
    free_pages: Vec<ZeroedPage>,
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
        Self {
            base,
            size,
            next_free_base: base,
            free_pages: Vec::new(),
            generations: analyzer,
        }
    }
    pub fn get_analyzer(&self) -> &GenerationAnalyzer {
        &self.generations
    }

    pub fn return_pages(&mut self, pages: impl Iterator<Item = AllocatedPage>) {
        self.free_pages.extend(pages.map(|page| page.zero()));
    }

    /// compute the optimal collection target according to internal metrics
    pub fn get_page(&mut self, generation: Generation) -> Option<Page> {
        let page = self.free_pages.pop().or_else(|| self.grow_heap())?;
        self.generations.set_generation(page.base, generation);

        Some(Page::new(page))
    }

    fn grow_heap(&mut self) -> Option<ZeroedPage> {
        let page = self.next_free_base;
        let next_free = unsafe { page.byte_add(GC_PAGE_SIZE) };

        if unsafe { next_free.byte_offset_from(self.base) } as usize > self.size {
            return None;
        } else {
            self.next_free_base = next_free;
            Some(
                AllocatedPage {
                    base: page,
                    _private: (),
                }
                .zero(),
            )
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

impl From<ZeroedPage> for AllocatedPage {
    fn from(value: ZeroedPage) -> Self {
        Self {
            base: value.base,
            _private: (),
        }
    }
}

/// a page
pub(crate) struct AllocatedPage {
    pub base: *mut u8,
    _private: (),
}

impl AllocatedPage {
    fn zero(mut self) -> ZeroedPage {
        unsafe {
            let base = self.base;

            self.as_byteslice().fill(0);

            // forget the page to ensure that it is not dropped, as we are converting it here...
            core::mem::forget(self);

            ZeroedPage::from_zeroed_addr(base)
        }
    }

    fn as_byteslice(&mut self) -> &mut [u8] {
        unsafe { core::slice::from_raw_parts_mut(self.base, GC_PAGE_SIZE) }
    }
}

impl Drop for AllocatedPage {
    fn drop(&mut self) {
        let base = self.base;
        self.as_byteslice().fill(0);

        let zeroed = ZeroedPage { base };

        get_global_gc().lock().unwrap().free_pages.push(zeroed);
    }
}
