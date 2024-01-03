use std::{alloc::Layout, cell::UnsafeCell, sync::Mutex};

struct LargeObjectPtr {
    ptr: *mut u8,
    layout: Layout,
}

impl Drop for LargeObjectPtr {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr, self.layout);
        }
    }
}

struct LargeObjectReference {
    object_id: u32,
}

struct LargeObjectHeapInner {
    slots: Vec<Option<LargeObjectPtr>>,
    free_list: Vec<u32>,
}

pub struct LargeObjectHeap {
    inner: Mutex<LargeObjectHeapInner>,
}

impl LargeObjectHeap {
    pub fn load(&self, reference: &LargeObjectReference) -> *const u8 {
        let guard = self.inner.lock().unwrap();
        let object = &guard.slots[reference.object_id as usize];
        object.as_ref().unwrap().ptr
    }

    pub fn free(&self, reference: &LargeObjectReference) {
        let mut guard = self.inner.lock().unwrap();
        guard.slots[reference.object_id as usize] = None;
    }

    pub fn alloc(&self, data: &[u8], align: usize) -> LargeObjectReference {
        let layout = Layout::from_size_align(data.len(), align).unwrap();
        let ptr = unsafe {
            let ptr = std::alloc::alloc(layout);
            let src = data.as_ptr();
            core::ptr::copy_nonoverlapping(src, ptr, data.len());
            LargeObjectPtr { layout, ptr }
        };
        let mut guard = self.inner.lock().unwrap();

        let used_slot = if let Some(slot) = guard.free_list.pop() {
            guard.slots[slot as usize] = Some(ptr);
            slot
        } else {
            let used_slot = guard.slots.len();
            guard.slots.push(Some(ptr));
            used_slot as u32
        };

        LargeObjectReference {
            object_id: used_slot,
        }
    }
}
