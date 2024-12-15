use crate::{GcPointer, RawGcPointer};

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
    T: Trace + Sized + 'static,
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

pub unsafe trait Trace {
    fn trace(&mut self, trace_fn: TraceCallback);
}

unsafe impl Trace for RawGcPointer {
    #[inline]
    fn trace(&mut self, trace_fn: TraceCallback) {
        trace_fn(self)
    }
}
unsafe impl<T> Trace for GcPointer<T> {
    #[inline]
    fn trace(&mut self, trace_fn: TraceCallback) {
        trace_fn(self.as_mut())
    }
}
unsafe impl<const N: usize, T> Trace for [T; N]
where
    T: Trace,
{
    #[inline]
    fn trace(&mut self, trace_fn: TraceCallback) {
        for entry in self {
            entry.trace(trace_fn)
        }
    }
}
macro_rules! nop_trace_impl {
    ($ty: ty) => {
        unsafe impl Trace for $ty {
            #[inline]
            fn trace(&mut self, _trace_fn: TraceCallback) {}
        }
    };
    ($first: ty, $($rest: ty),+) => {
        nop_trace_impl!($first);
        nop_trace_impl!($($rest),+);
    };
}
nop_trace_impl!(
    bool,
    char,
    f32,
    f64,
    i128,
    i16,
    i32,
    i64,
    i8,
    isize,
    u128,
    u16,
    u32,
    u64,
    u8,
    usize,
    ()
);
unsafe impl<T> Trace for Option<T>
where
    T: Trace,
{
    fn trace(&mut self, trace_fn: TraceCallback) {
        if let Some(v) = self.as_mut() {
            v.trace(trace_fn);
        }
    }
}
unsafe impl<T, E> Trace for Result<T, E>
where
    T: Trace,
    E: Trace,
{
    fn trace(&mut self, trace_fn: TraceCallback) {
        match self {
            Ok(s) => s.trace(trace_fn),
            Err(e) => e.trace(trace_fn),
        }
    }
}

unsafe impl<T1, T2> Trace for (T1, T2)
where
    T1: Trace,
    T2: Trace,
{
    fn trace(&mut self, trace_fn: TraceCallback) {
        self.0.trace(trace_fn);
        self.1.trace(trace_fn);
    }
}
