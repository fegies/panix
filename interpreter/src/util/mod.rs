use std::cell::UnsafeCell;

pub struct BufferPool<T> {
    inner: InnerPool<T>,
}
struct InnerPool<T> {
    backing: UnsafeCell<Vec<Vec<T>>>,
}
pub struct RentedBuffer<'pool, T> {
    inner: Vec<T>,
    pool: &'pool InnerPool<T>,
}

impl<T> BufferPool<T> {
    pub fn get(&self) -> RentedBuffer<T> {
        let buf = self.inner.get_buf();
        RentedBuffer {
            inner: buf,
            pool: &self.inner,
        }
    }
}

impl<T> Default for BufferPool<T> {
    fn default() -> Self {
        Self {
            inner: InnerPool {
                backing: UnsafeCell::new(Vec::new()),
            },
        }
    }
}

impl<T> InnerPool<T> {
    fn with_backing<R>(&self, func: impl FnOnce(&mut Vec<Vec<T>>) -> R) -> R {
        let vec = unsafe { &mut *self.backing.get() };
        func(vec)
    }
    fn get_buf(&self) -> Vec<T> {
        unsafe { &mut *self.backing.get() }
            .pop()
            .unwrap_or_default()
    }
    fn return_buf(&self, mut buf: Vec<T>) {
        buf.clear();
        unsafe { &mut *self.backing.get() }.push(buf)
    }
}

impl<T> AsMut<Vec<T>> for RentedBuffer<'_, T> {
    fn as_mut(&mut self) -> &mut Vec<T> {
        &mut self.inner
    }
}

impl<T> Drop for RentedBuffer<'_, T> {
    fn drop(&mut self) {
        self.pool.return_buf(core::mem::take(&mut self.inner))
    }
}
