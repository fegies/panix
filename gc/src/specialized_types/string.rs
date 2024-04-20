use std::marker::PhantomData;

use crate::{
    heap_page::Page,
    object::{HeapObject, TraceCallback},
    GcPointer, RawGcPointer,
};

pub struct SimpleGcString {
    length: u32,
    // in the on-heap layout, the string content
    // is placed immediately after the length.
}

unsafe impl HeapObject for SimpleGcString {
    fn trace(&mut self, cb: TraceCallback) {
        // a simple string does not contain any pointers to other data.
    }

    fn allocation_size(&self) -> usize {
        self.length as usize + core::mem::size_of_val(&self.length)
    }
}

impl AsRef<str> for SimpleGcString {
    fn as_ref(&self) -> &str {
        unsafe {
            let string_begin_ptr = (self as *const SimpleGcString).add(1) as *const u8;
            let slice = core::slice::from_raw_parts(string_begin_ptr, self.length as usize);
            core::str::from_utf8_unchecked(slice)
        }
    }
}

impl Page {
    pub fn alloc_string(&mut self, str: &str) -> Option<GcPointer<SimpleGcString>> {
        let len = str.len();
        let required_space = len + core::mem::size_of::<SimpleGcString>();
        let data_pointer = self.try_reserve(required_space)?;
        unsafe {
            core::ptr::write(
                data_pointer as *mut SimpleGcString,
                SimpleGcString { length: len as u32 },
            );
            let string_begin_pointer = data_pointer.add(core::mem::size_of::<SimpleGcString>());
            let dest = core::slice::from_raw_parts_mut(string_begin_pointer, len);
            dest.copy_from_slice(str.as_bytes());
            let raw_ptr = RawGcPointer::from_heap_addr(data_pointer);
            Some(GcPointer {
                ptr: raw_ptr,
                data: PhantomData,
            })
        }
    }
}
