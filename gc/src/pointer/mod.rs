mod rootset;

pub use internal::*;
pub(crate) use rootset::inspect_roots;

mod internal;

impl Clone for RawGcPointer {
    fn clone(&self) -> Self {
        self.root()
    }
}
impl<T> Clone for GcPointer<T> {
    fn clone(&self) -> Self {
        self.root()
    }
}
