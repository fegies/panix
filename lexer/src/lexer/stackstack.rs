use core::mem::MaybeUninit;

pub struct Stack<const N: usize, T> {
    inner: [MaybeUninit<T>; N],
    top: usize,
}

impl<const N: usize, T> Stack<N, T> {
    pub fn new() -> Self {
        let inner = unsafe { MaybeUninit::uninit().assume_init() };
        Self { inner, top: 0 }
    }
    pub fn push(&mut self, value: T) -> Result<(), T> {
        if self.top < N {
            self.inner[self.top].write(value);
            self.top += 1;
            Ok(())
        } else {
            Err(value)
        }
    }
    pub fn pop(&mut self) -> Option<T> {
        if self.top > 0 {
            self.top -= 1;
            Some(unsafe { self.inner[self.top].assume_init_read() })
        } else {
            None
        }
    }
}
