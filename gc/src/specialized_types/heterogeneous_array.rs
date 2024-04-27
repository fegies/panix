use crate::{object::HeapObject, RawGcPointer};

/// This is an array that contains a set of pointers to other gc objects.
pub struct HeterogeneousArray {
    length: u32,
}

unsafe impl HeapObject for HeterogeneousArray {
    fn trace(&mut self, cb: crate::object::TraceCallback) {
        // we can trace this object by simply scavenging all items it points to.
        let slice = self.as_mut_slice();
        for pointer in slice {
            cb(pointer)
        }
    }

    fn allocation_size(&self) -> usize {
        self.length as usize + core::mem::size_of_val(self)
    }
}

impl AsRef<[RawGcPointer]> for HeterogeneousArray {
    fn as_ref(&self) -> &[RawGcPointer] {
        unsafe {
            let begin_ptr = (self as *const HeterogeneousArray).add(1) as *const RawGcPointer;
            let len = self.length as usize;
            core::slice::from_raw_parts(begin_ptr, len)
        }
    }
}

impl HeterogeneousArray {
    // deliberately not implementing AsMut because that would allow anyone with a mutable reference
    // to an array pointer to mutate it
    // and as the pointer is copy, this would be unsafe.
    fn as_mut_slice(&mut self) -> &mut [RawGcPointer] {
        unsafe {
            let begin_ptr = (self as *const HeterogeneousArray).add(1) as *mut RawGcPointer;
            let len = self.length as usize;
            core::slice::from_raw_parts_mut(begin_ptr, len)
        }
    }
}
