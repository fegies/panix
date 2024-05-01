use std::cell::UnsafeCell;

mod pointerslab;
use pointerslab::Pointerslab;

use crate::{GcPointer, RawGcPointer};

pub(super) use pointerslab::Slabkey;

use super::{HeapGcPointer, RawHeapGcPointer};

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

pub(crate) fn inspect_roots(func: impl FnMut(&mut RawHeapGcPointer)) {
    with_rootset(|rootset| rootset.inner.iter_mut().for_each(func))
}

impl RootSet {
    fn root_heapref(&mut self, heapref: &RawHeapGcPointer) -> RawGcPointer {
        let rootref = self.inner.insert(heapref.clone());
        super::RootsetReference { content: rootref }.into()
    }

    fn root_raw(&mut self, ptr: &RawGcPointer) -> RawGcPointer {
        self.root_heapref(&ptr.get_heapref())
    }

    /// replace the pointer representation to instead be a plain heap-only pointer.
    fn unroot_raw(&mut self, ptr: &mut RawGcPointer) {
        if let Err(rootref) = ptr.decode() {
            let loaded = self.inner.remove(rootref.content);
            unsafe { core::ptr::write(ptr as *mut RawGcPointer, loaded.into()) }
        }
    }
}

fn with_rootset<TR>(func: impl FnOnce(&mut RootSet) -> TR) -> TR {
    ROOT_SET.with(|cell| {
        let mutref = unsafe { &mut *cell.get() };
        func(mutref)
    })
}

pub(super) fn read_rootset_entry(entry: &Slabkey) -> RawHeapGcPointer {
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
        // because we got it as a param.v
        unsafe { GcPointer::from_raw_unchecked(raw) }
    }
}
impl RawHeapGcPointer {
    pub fn root(&self) -> RawGcPointer {
        with_rootset(|set| set.root_heapref(self))
    }
}

impl<T> HeapGcPointer<T> {
    pub fn root(&self) -> GcPointer<T> {
        let raw: &RawHeapGcPointer = self.as_ref();
        let raw = raw.root();
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
