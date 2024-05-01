use crate::RawGcPointer;

pub type TraceCallback<'a> = &'a mut dyn FnMut(&mut RawGcPointer);

pub(crate) mod widen {
    use super::HeapObject;

    /// returns a function that should turn a raw pointer to the allocation
    /// into a wide pointer with the correct vtable.
    ///
    /// Done this way to get around the ptr_metadata feature not being stable
    #[inline]
    pub(crate) fn widen<T: HeapObject + 'static>(raw: *mut ()) -> *const (dyn HeapObject) {
        raw as *mut u8 as *mut T
    }

    pub struct WideningAccessor {
        func: fn(*mut ()) -> *const dyn HeapObject,
    }

    impl WideningAccessor {
        pub fn widen<'r>(&self, data_ref: &'r ()) -> &'r dyn HeapObject {
            let raw = (self.func)((data_ref as *const ()).cast_mut());
            unsafe { &*raw }
        }
        pub fn widen_mut<'r>(&self, data_ref: &'r mut ()) -> &'r mut dyn HeapObject {
            let raw = (self.func)(data_ref as *mut ());
            unsafe { &mut *raw.cast_mut() }
        }

        pub unsafe fn from_bitpattern(bitpattern: usize) -> Self {
            Self {
                func: unsafe { core::mem::transmute(bitpattern) },
            }
        }
        pub fn to_bitpattern(&self) -> usize {
            self.func as usize
        }
        pub fn for_type<'a, T: HeapObject + 'static>() -> Self {
            Self { func: widen::<T> }
        }
    }
}

/// this is a lower-level trait compared to the trace trait.
///
/// You should only need to implement manually it if your object is
/// dynamically sized.
pub unsafe trait HeapObject {
    /// A method that allows to introspect the contents of the allocation.
    ///
    /// It should invoke the callback once for every pointer internal to the object.
    fn trace(&mut self, cb: TraceCallback);

    /// the total size of the allocation, excluding the object header
    fn allocation_size(&self) -> usize;

    /// the requested alignment for the allocation.
    fn allocation_alignment(&self) -> usize;
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
