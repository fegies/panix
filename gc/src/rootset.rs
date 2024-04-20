use std::cell::Cell;
use std::{marker::PhantomData, ops::DerefMut};

use crate::{GcPointer, RawGcPointer};

pub struct RootSet {
    inner: Vec<Option<RawGcPointer>>,
    free_slots: Vec<usize>,
}

struct RootedPointer<T> {
    ptr_idx: usize,
    inner: PhantomData<T>,
}

thread_local! {
    static ROOT_SET: Cell<*mut RootSet> = const {
        Cell::new(std::ptr::null_mut())
    };
}

impl RootSet {
    fn root<T: Trace>(&mut self, ptr: GcPointer<T>) -> RootedPointer<T> {
        let raw = Some(ptr.as_raw());
        let ptr_idx = if let Some(slot) = self.free_slots.pop() {
            self.inner[slot] = raw;
            slot
        } else {
            let slot = self.inner.len();
            self.inner.push(raw);
            slot
        };
        RootedPointer {
            ptr_idx,
            inner: PhantomData,
        }
    }

    fn unroot<T>(&mut self, ptr: &RootedPointer<T>) {
        self.free_slots.push(ptr.ptr_idx);
        self.inner[ptr.ptr_idx] = None;
    }
}

impl<T: Trace> GcPointer<T> {
    pub fn root(self) -> RootedPointer<T> {
        let root_set = unsafe { &mut *ROOT_SET.get() };
        root_set.root(self)
    }
}

impl<T> Drop for RootedPointer<T> {
    fn drop(&mut self) {
        let root_set = unsafe { &mut *ROOT_SET.get() };
        root_set.unroot(self)
    }
}
