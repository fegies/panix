use crate::RawGcPointer;

type TraceCallback<'a> = &'a mut dyn FnMut(&mut RawGcPointer);

pub trait HeapObject {
    /// A method that allows to introspect the contents of the allocation.
    ///
    /// It should invoke the callback once for every pointer internal to the object.
    fn trace(&mut self, cb: TraceCallback);

    /// the total size of the allocation, excluding the object header
    unsafe fn allocation_size(&self) -> u32;

    /// returns a function that should turn a raw pointer to the allocation
    /// into a wide pointer with the correct vtable.
    ///
    /// Done this way to get around the ptr_metadata feature not being stable
    fn get_widener<'a>(&self) -> fn(&'a mut u8) -> &'a mut dyn HeapObject;
}

fn widen<'r, T: HeapObject + 'static>(raw: &'r mut u8) -> &'r mut dyn HeapObject {
    let ptr = raw as *mut u8 as *mut T;
    unsafe { &mut *ptr }
}

impl<T> HeapObject for T
where
    T: Tracable + Sized + 'static,
{
    unsafe fn allocation_size(&self) -> u32 {
        let size = core::mem::size_of::<T>();
        debug_assert!(size <= u32::MAX as usize);
        size as u32
    }

    #[inline]
    fn get_widener<'a>(&self) -> fn(&'a mut u8) -> &'a mut dyn HeapObject {
        widen::<T>
    }

    fn trace(&mut self, cb: TraceCallback) {
        self.trace(cb);
    }
}

pub trait Tracable {
    fn trace(&mut self, trace_fn: TraceCallback);
}
