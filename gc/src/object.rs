use core::any::Any;

use crate::RawGcPointer;

pub type TraceCallback<'a> = &'a mut dyn FnMut(&mut RawGcPointer);

pub(crate) type WideningAccessor = fn(*mut u8) -> *mut dyn HeapObject;

/// this is a lower-level trait compared to the trace trait.
///
/// You should only need to implement manually it if your object is
/// dynamically sized.
pub unsafe trait HeapObject: Any {
    /// A method that allows to introspect the contents of the allocation.
    ///
    /// It should invoke the callback once for every pointer internal to the object.
    fn trace(&mut self, cb: TraceCallback);

    /// the total size of the allocation, excluding the object header
    fn allocation_size(&self) -> usize;

    /// the requested alignment for the allocation.
    fn allocation_alignment(&self) -> usize;
}

/// returns a function that should turn a raw pointer to the allocation
/// into a wide pointer with the correct vtable.
///
/// Done this way to get around the ptr_metadata feature not being stable
#[inline]
pub(crate) fn widen<T: HeapObject>(raw: *mut u8) -> *mut (dyn HeapObject + 'static) {
    raw as *mut u8 as *mut T
}

unsafe impl<T> HeapObject for T
where
    T: Tracable + Sized + 'static,
{
    fn allocation_size(&self) -> usize {
        let size = core::mem::size_of::<T>();
        debug_assert!(size <= u32::MAX as usize);
        size
    }

    fn allocation_alignment(&self) -> usize {
        core::mem::align_of::<T>()
    }

    fn trace(&mut self, cb: TraceCallback) {
        self.trace(cb);
    }
}

pub trait Tracable {
    fn trace(&mut self, trace_fn: TraceCallback);
}
