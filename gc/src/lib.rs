mod heap;
pub mod loh;
mod region;

use std::{marker::PhantomData, num::NonZeroI16};

use region::*;

#[derive(Clone, Copy)]
pub struct GcPointer<TData: Trace> {
    ptr: RawGcPointer,
    data: PhantomData<TData>,
}

impl<TData: Trace> GcPointer<TData> {
    pub fn to_raw(self) -> RawGcPointer {
        self.ptr
    }
}

pub struct GcArrayPtr {
    ptr: RawGcPointer,
}

type RegionId = NonZeroI16;

// A pointer to the GC header of the next entry.
#[derive(Clone, Copy)]
pub struct RawGcPointer {
    value: RegionValuePointer,
    region: RegionId,
}

pub trait Trace {
    fn trace(&self, mark_op: &mut dyn FnMut(&mut RawGcPointer));
}

struct MemoryManager {}

impl MemoryManager {
    pub fn load_mut<TData: Trace>(&self, ptr: GcPointer<TData>) -> &mut TData {
        todo!()
    }
    pub fn load<TData: Trace>(&self, ptr: GcPointer<TData>) -> &TData {
        todo!()
    }

    pub fn alloc<TData: Trace>(&self, data: TData) -> GcPointer<TData> {
        todo!()
    }

    /// announce that referencing_pointer now has a reference to data_ptr.
    /// This gives the GC an opportunity to promote the object to that generation immediately.
    pub fn announce(
        &self,
        referencing_pointer: GcPointer<impl Trace>,
        data_ptr: &mut GcPointer<impl Trace>,
    ) {
    }

    /// mark the given object as alive
    fn mark(&self, ptr: GcPointer<impl Trace>) {}
}

pub fn init() {
    heap::init(1 << 32);
}
