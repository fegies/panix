use core::marker::PhantomData;
use std::mem::MaybeUninit;

use crate::{
    GenerationCounter,
    heap_page::{HeapEntry, Page},
    object::HeapObject,
    pointer::{HeapGcPointer, RawHeapGcPointer},
};

pub struct Array<T>
where
    T: Sized,
{
    length: u32,
    // in the on-heap layout, the content is placed on the next higher offset that
    // fits the requested alignment
    content: PhantomData<T>,
}

unsafe impl<T> HeapObject for Array<T>
where
    T: HeapObject + Sized,
{
    fn trace(&mut self, cb: crate::TraceCallback) {
        self.as_mut().iter_mut().for_each(|r| r.trace(cb))
    }

    fn allocation_size(&self) -> usize {
        Self::get_size_for_len(self.length as usize)
    }

    fn allocation_alignment(&self) -> usize {
        Self::get_alignment()
    }
}

impl<T: Sized> Array<T> {
    fn get_alignment() -> usize {
        core::mem::align_of::<Array<T>>().max(core::mem::align_of::<T>())
    }
    const fn get_size_for_len(len: usize) -> usize {
        core::mem::size_of::<Self>() + Self::get_header_padding() + len * core::mem::size_of::<T>()
    }

    const fn get_header_padding() -> usize {
        assert!(core::mem::size_of::<T>() >= core::mem::align_of::<T>());
        core::mem::align_of::<T>().saturating_sub(core::mem::size_of::<Array<T>>())
    }

    fn get_data_ptr(&self) -> *const T {
        unsafe {
            (self as *const Array<T>)
                .add(1)
                .byte_add(Self::get_header_padding()) as *const T
        }
    }
    fn get_data_ptr_mut(&mut self) -> *mut T {
        unsafe {
            (self as *mut Array<T>)
                .add(1)
                .byte_add(Self::get_header_padding()) as *mut T
        }
    }
}

impl<T> AsRef<[T]> for Array<T>
where
    T: Sized,
{
    fn as_ref(&self) -> &[T] {
        let ptr = self.get_data_ptr();
        let len = self.length as usize;
        unsafe { core::slice::from_raw_parts(ptr, len) }
    }
}
impl<T> AsMut<[T]> for Array<T>
where
    T: Sized,
{
    fn as_mut(&mut self) -> &mut [T] {
        let ptr = self.get_data_ptr_mut();
        let len = self.length as usize;
        unsafe { core::slice::from_raw_parts_mut(ptr, len) }
    }
}

impl Page {
    /// Allocate the provided slice in-place.
    /// the init function must exactly initialize all entries in the slice passed to it.
    unsafe fn try_alloc_slice_inplace<T>(
        &self,
        num_items: u32,
        init: impl FnOnce(&mut [MaybeUninit<T>]),
        counter: &GenerationCounter,
    ) -> Option<HeapGcPointer<Array<T>>>
    where
        T: HeapObject + Sized + 'static,
    {
        let required_space = Array::<T>::get_size_for_len(num_items as usize);
        let (header_pointer, data_pointer) =
            self.try_reserve(required_space, Array::<T>::get_alignment(), counter)?;
        let cast_data_pointer = data_pointer as *mut Array<T>;

        unsafe {
            core::ptr::write(
                cast_data_pointer,
                Array {
                    length: num_items,
                    content: PhantomData,
                },
            );
            let obj = &mut *cast_data_pointer;
            let data_content_ptr = obj.get_data_ptr_mut() as *mut MaybeUninit<T>;
            let maybe_uninit_slice =
                core::slice::from_raw_parts_mut(data_content_ptr, num_items as usize);
            init(maybe_uninit_slice);

            // and unroot all entries
            for item in maybe_uninit_slice {
                item.assume_init_mut().trace(&mut |ptr| ptr.unroot());
            }

            core::ptr::write(header_pointer, HeapEntry::for_object(obj));

            let raw_ptr = RawHeapGcPointer::from_addr(header_pointer);

            Some(HeapGcPointer::from_raw_unchecked(raw_ptr))
        }
    }

    pub fn try_alloc_slice<T>(
        &self,
        slice: &[T],
        counter: &mut GenerationCounter,
    ) -> Option<HeapGcPointer<Array<T>>>
    where
        T: HeapObject + Sized + 'static + Clone,
    {
        unsafe {
            self.try_alloc_slice_inplace(
                slice.len() as u32,
                |dest| {
                    for (dest, src) in dest.iter_mut().zip(slice) {
                        dest.write(src.clone());
                    }
                },
                counter,
            )
        }
    }

    pub fn try_alloc_vec<T>(
        &self,
        vec: &mut Vec<T>,
        counter: &GenerationCounter,
    ) -> Option<HeapGcPointer<Array<T>>>
    where
        T: HeapObject + Sized + 'static,
    {
        unsafe {
            self.try_alloc_slice_inplace(
                vec.len() as u32,
                |dest| {
                    for (dest, src) in dest.iter_mut().zip(vec.drain(..)) {
                        dest.write(src);
                    }
                },
                counter,
            )
        }
    }
}
