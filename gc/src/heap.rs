use std::{cell::UnsafeCell, io::Write, mem::MaybeUninit};

use memmap2::{MmapMut, MmapOptions};

use crate::RawGcPointer;

// 2 MB pages

pub struct Heap {
    free_pages: Vec<PageInfo>,
}

struct PageInfo {
    base: *mut u8,
    status: Vec<GcRegion>,
}
