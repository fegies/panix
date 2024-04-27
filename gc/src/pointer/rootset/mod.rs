use std::cell::UnsafeCell;

mod pointerslab;
use pointerslab::Pointerslab;

use crate::{GcPointer, RawGcPointer};

pub(super) use pointerslab::Slabkey;

pub struct RootSet {
    inner: Pointerslab,
}

thread_local! {
    static ROOT_SET: UnsafeCell<RootSet> = const {
        UnsafeCell::new(RootSet {
            inner: Pointerslab::new()
        })
    };
}

impl RootSet {
    fn root_raw(&mut self, ptr: &RawGcPointer) -> RawGcPointer {
        let heapref = ptr.get_heapref();
        let rootref = self.inner.insert(heapref.content);
        super::RootsetReference { content: rootref }.into()
    }

    /// replace the pointer representation to instead be a plain heap-only pointer.
    fn unroot_raw(&mut self, ptr: &mut RawGcPointer) {
        if let Err(rootref) = ptr.decode() {
            let loaded = self.inner.remove(rootref.content);
            let loaded = super::RawHeapGcPointer { content: loaded }.into();
            unsafe { core::ptr::write(ptr as *mut RawGcPointer, loaded) }
        }
    }
}

fn with_rootset<TR>(func: impl FnOnce(&mut RootSet) -> TR) -> TR {
    ROOT_SET.with(|cell| {
        let mutref = unsafe { &mut *cell.get() };
        func(mutref)
    })
}

pub(super) fn read_rootset_entry(entry: &Slabkey) -> u32 {
    with_rootset(|set| set.inner.get(entry))
}

impl RawGcPointer {
    pub fn root(&self) -> RawGcPointer {
        with_rootset(|set| set.root_raw(self))
    }

    /// unroot the pointer.
    /// it is unsafe because a guarantee must be made that
    /// it is not used outside of the heap.
    pub(crate) unsafe fn unroot(&mut self) {
        with_rootset(|set| set.unroot_raw(self));
    }
}
impl<T> GcPointer<T> {
    pub fn root(&self) -> GcPointer<T> {
        let raw: &RawGcPointer = self.as_ref();
        let raw = raw.root();
        // SAFETY: we know that the pointer we just rooted is of the correct type
        // because we got it as a param.
        unsafe { GcPointer::from_raw_unchecked(raw) }
    }
}

impl Drop for RawGcPointer {
    fn drop(&mut self) {
        if self.is_root() {
            unsafe { self.unroot() }
        }
    }
}
