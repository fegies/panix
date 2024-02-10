use core::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit};

struct BufferInner<const N: usize, T> {
    inner: [MaybeUninit<T>; N],
    read_ptr: usize,
    write_ptr: usize,
}
impl<const N: usize, T> BufferInner<N, T> {
    fn new() -> Self {
        let inner = unsafe { MaybeUninit::uninit().assume_init() };
        Self {
            inner,
            read_ptr: 0,
            write_ptr: 0,
        }
    }
    /// add a new value to the buffer.
    /// returns true if there is actually space for another element
    fn push(&mut self, value: T) -> bool {
        debug_assert!(self.write_ptr < N);
        self.inner[self.write_ptr].write(value);
        self.write_ptr += 1;
        self.read_ptr = 0;

        self.write_ptr < N
    }

    fn read(&mut self) -> Option<T> {
        if self.read_ptr < self.write_ptr {
            let val = unsafe { self.inner[self.read_ptr].assume_init_read() };
            self.read_ptr += 1;
            Some(val)
        } else {
            self.write_ptr = 0;
            None
        }
    }
}

pub struct Buffer<const N: usize, T> {
    inner: UnsafeCell<BufferInner<N, T>>,
    _marker: PhantomData<*const ()>,
}

impl<const N: usize, T> Buffer<N, T> {
    pub fn new() -> Self {
        Self {
            inner: UnsafeCell::new(BufferInner::new()),
            _marker: PhantomData,
        }
    }

    pub fn push(&self, value: T) -> bool {
        let ptr = unsafe { &mut *self.inner.get() };
        ptr.push(value)
    }

    pub fn read(&self) -> Option<T> {
        let ptr = unsafe { &mut *self.inner.get() };
        ptr.read()
    }
}
