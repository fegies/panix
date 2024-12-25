use std::mem::MaybeUninit;

pub struct Stackvec<const N: usize, T> {
    inner: [MaybeUninit<T>; N],
    length: usize,
}

impl<const N: usize, T> Stackvec<N, T> {
    pub fn new() -> Self {
        unsafe {
            let inner = MaybeUninit::uninit().assume_init();
            Self { inner, length: 0 }
        }
    }

    /// push the new value onto the vec.
    /// if the vec is full, the passed value is returned.
    pub fn push(&mut self, value: T) -> Option<T> {
        if self.length < N {
            self.inner[self.length].write(value);
            self.length += 1;
            None
        } else {
            Some(value)
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.length == 0 {
            None
        } else {
            self.length -= 1;
            unsafe { Some(self.inner[self.length].assume_init_read()) }
        }
    }

    /// clears all entries in this vector.
    pub fn clear(&mut self) {
        let length = self.length;
        // set to 0 first to avoid multiple drop
        self.length = 0;

        unsafe {
            let slice = core::slice::from_raw_parts_mut(self.inner.as_mut_ptr() as *mut T, length);
            core::ptr::drop_in_place(slice);
        }
    }
}

impl<const N: usize, T> AsRef<[T]> for Stackvec<N, T> {
    fn as_ref(&self) -> &[T] {
        let start_ptr = self.inner.as_ptr() as *const T;
        unsafe { core::slice::from_raw_parts(start_ptr, self.length) }
    }
}
impl<const N: usize, T> AsMut<[T]> for Stackvec<N, T> {
    fn as_mut(&mut self) -> &mut [T] {
        let start_ptr = self.inner.as_ptr() as *mut T;
        unsafe { core::slice::from_raw_parts_mut(start_ptr, self.length) }
    }
}

impl<const N: usize, T> Drop for Stackvec<N, T> {
    fn drop(&mut self) {
        unsafe { core::ptr::drop_in_place(self.as_mut()) }
    }
}
