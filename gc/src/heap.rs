use std::{cell::UnsafeCell, mem::MaybeUninit};

use memmap2::{MmapMut, MmapOptions};

use crate::RawGcPointer;

// 2 MB pages
const PAGE_BITS: u8 = 21;
const PAGE_SIZE: usize = 1 << PAGE_BITS;

pub struct Heap {
    map: MmapMut,
    pages: Vec<PageInfo>,
}

struct PageInfo {
    base: *mut u8,
    generation: u8,
    status: PageStatus,
}

enum PageStatus {
    Free,
    Allocated,
}

static mut HEAP: MaybeUninit<Heap> = MaybeUninit::uninit();

impl Heap {
    pub fn init(heap_size: usize) -> Self {
        assert!(heap_size % PAGE_SIZE == 0);

        let map = alloc_map(heap_size, true);

        Self { map }
    }

    pub fn base_address(&self) -> *mut u8 {
        self.map.as_ptr() as *mut u8
    }

    pub fn size(&self) -> usize {
        self.map.len()
    }

    pub fn get_pageinfo(&self, ptr: RawGcPointer) -> &PageStatus {}
}

fn alloc_map(heap_size: usize, map_huge: bool) -> MmapMut {
    let mut options = MmapOptions::new();
    options.len(heap_size);

    if map_huge {
        options.huge(Some(PAGE_BITS));
        options
            .map_anon()
            .unwrap_or_else(|_| alloc_map(heap_size, false))
    } else {
        options.map_anon().unwrap()
    }
}

pub fn init(heap_size: usize) {
    let heap = Heap::init(heap_size);
    unsafe {
        HEAP.write(heap);
    }
}
