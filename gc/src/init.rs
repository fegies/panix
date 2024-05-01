use std::sync::{Arc, Mutex};

use super::*;

static GLOBAL_GC: Mutex<Option<Arc<Mutex<GlobalGc>>>> = Mutex::new(None);

pub fn get_global_gc() -> Arc<Mutex<GlobalGc>> {
    let mut guard = GLOBAL_GC.lock().unwrap();
    guard
        .get_or_insert_with(|| {
            let ptr = unsafe { std::alloc::alloc_zeroed(HEAP_LAYOUT) };
            unsafe {
                HEAP_BASE = ptr;
            }
            println!("allocated heap at {ptr:?}");
            let heap = Pagetracker::new(ptr, HEAP_LAYOUT.size());

            let global = GlobalGc { heap };
            Arc::new(Mutex::new(global))
        })
        .clone()
}
