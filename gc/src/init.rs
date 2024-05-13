use std::{
    alloc::handle_alloc_error,
    sync::{Arc, Mutex},
};

use super::*;

static GLOBAL_GC: Mutex<Option<Arc<Mutex<Pagetracker>>>> = Mutex::new(None);

pub fn get_global_gc() -> Arc<Mutex<Pagetracker>> {
    let mut guard = GLOBAL_GC.lock().unwrap();
    guard
        .get_or_insert_with(|| {
            let ptr = unsafe { std::alloc::alloc(HEAP_LAYOUT) };
            if ptr == core::ptr::null_mut() {
                handle_alloc_error(HEAP_LAYOUT);
            }
            unsafe {
                HEAP_BASE = ptr;
            }
            println!("allocated heap at {ptr:?}");
            let heap = Pagetracker::new(ptr, HEAP_LAYOUT.size());

            Arc::new(Mutex::new(heap))
        })
        .clone()
}
