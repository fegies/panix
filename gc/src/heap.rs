use std::{cell::UnsafeCell, io::Write, mem::MaybeUninit};

use memmap2::{MmapMut, MmapOptions};

use crate::{region::GcRegion, RawGcPointer};

// 2 MB pages
const PAGE_BITS: u8 = 21;
const PAGE_SIZE: usize = 1 << PAGE_BITS;

pub struct Heap {
    map: MmapMut,
    // free_pages: Vec<PageInfo>,
    // gen0: PageInfo,
    // gen1: Vec<PageInfo>,
    // gen2: Vec<PageInfo>,
}

struct PageInfo {
    base: *mut u8,
    status: Vec<GcRegion>,
}

static mut HEAP: MaybeUninit<Heap> = MaybeUninit::uninit();

impl Heap {
    pub fn init(heap_size: usize) -> Self {
        assert!(heap_size % PAGE_SIZE == 0);

        // 16 pages for the start...
        let initial = alloc_map(PAGE_SIZE * 16);

        Self { map: initial }
        // let initial = Box::leak(Box::new(initial)).as_mut_ptr();
        // let gen0 = alloc_map(PAGE_SIZE);

        // for base_addr in

        // Self {
        //     map,
        //     pages: Vec::new(),
        // }
    }

    pub fn base_address(&self) -> *mut u8 {
        self.map.as_ptr() as *mut u8
    }

    pub fn size(&self) -> usize {
        self.map.len()
    }
}

fn alloc_map(size: usize) -> MmapMut {
    fn inner(size: usize, map_huge: bool) -> MmapMut {
        let mut options = MmapOptions::new();
        options.len(size);
        if map_huge {
            options.huge(Some(PAGE_BITS));
            options.map_anon().unwrap_or_else(|e| inner(size, false))
        } else {
            options.map_anon().unwrap()
        }
    }

    inner(size, true)
}

pub fn init(heap_size: usize) {
    let heap = Heap::init(heap_size);
    unsafe {
        HEAP.write(heap);
    }
}
