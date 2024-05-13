use crate::{
    heap_page::{HeapEntry, Page},
    object::{HeapObject, TraceCallback},
    pointer::{HeapGcPointer, RawHeapGcPointer},
    GcError, GcHandle, GcPointer, GcResult,
};

pub struct SimpleGcString {
    length: u32,
    // in the on-heap layout, the string content
    // is placed immediately after the length.
}

unsafe impl HeapObject for SimpleGcString {
    fn trace(&mut self, _cb: TraceCallback) {
        // a simple string does not contain any pointers to other data.
        // thus, our trace is a noop
    }

    fn allocation_size(&self) -> usize {
        self.length as usize + core::mem::size_of_val(self)
    }

    fn allocation_alignment(&self) -> usize {
        core::mem::align_of_val(self)
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
    fn try_alloc_string(&self, str: &str) -> Option<HeapGcPointer<SimpleGcString>> {
        let len = str.len();
        let required_space = len + core::mem::size_of::<SimpleGcString>();
        let (header_pointer, data_pointer) =
            self.try_reserve(required_space, core::mem::align_of::<SimpleGcString>())?;
        let cast_data_pointer = data_pointer as *mut SimpleGcString;
        unsafe {
            core::ptr::write(cast_data_pointer, SimpleGcString { length: len as u32 });
            let obj = &mut *cast_data_pointer;
            core::ptr::write(header_pointer, HeapEntry::for_object(obj));
            let string_begin_pointer = cast_data_pointer.add(1) as *mut u8;
            let dest = core::slice::from_raw_parts_mut(string_begin_pointer, len);
            dest.copy_from_slice(str.as_bytes());
            let raw_ptr = RawHeapGcPointer::from_addr(header_pointer);
            Some(HeapGcPointer::from_raw_unchecked(raw_ptr))
        }
    }
}

impl GcHandle {
    pub fn alloc_string(&mut self, str: &str) -> GcResult<GcPointer<SimpleGcString>> {
        let ptr = match self.alloc_pages.pages[0].try_alloc_string(str) {
            Some(ptr) => ptr,
            None => {
                self.clear_nursery()?;
                self.alloc_pages.pages[0]
                    .try_alloc_string(str)
                    .ok_or(GcError::ObjectBiggerThanPage)?
            }
        };
        Ok(ptr.root())
    }
}
