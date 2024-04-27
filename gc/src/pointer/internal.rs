use std::marker::PhantomData;

use crate::{get_heap_base, heap_page::HeapEntry};

use super::rootset::{read_rootset_entry, Slabkey};

/// a pointer that may either be rooted or a direct on-heap reference to something.
pub struct GcPointer<TData> {
    ptr: RawGcPointer,
    data: PhantomData<TData>,
}

/// representation:
/// bits 0..30: real pointer data
/// bit 31:
///     1 -> This is a root reference
///     0 -> This is a heap reference
pub struct RawGcPointer {
    content: u32,
}

/// representation:
/// the lower 32 bits of the heap addr, shifted right by the min alignment of
/// the heap header
pub(crate) struct RawHeapGcPointer {
    pub(super) content: u32,
}
pub(crate) struct HeapGcPointer<T> {
    raw: RawHeapGcPointer,
    data: PhantomData<T>,
}
impl<T> HeapGcPointer<T> {
    pub(crate) unsafe fn from_raw_unchecked(raw: RawHeapGcPointer) -> Self {
        Self {
            raw,
            data: PhantomData,
        }
    }
}

impl<T> Into<RawHeapGcPointer> for HeapGcPointer<T> {
    fn into(self) -> RawHeapGcPointer {
        self.raw
    }
}

impl RawHeapGcPointer {
    /// Create a new instance of this pointer.
    /// This function is only valid if the heap address
    /// is properly allocated on the gc heap and
    /// valid.
    pub(crate) unsafe fn from_addr(raw_ptr: *mut HeapEntry) -> Self {
        let value = (raw_ptr as usize >> (32 - HEAP_ENTRY_SHIFT)) as u32;
        Self { content: value }
    }

    pub(crate) unsafe fn from_bits(bitrep: u32) -> Self {
        Self { content: bitrep }
    }
    /// get the bitpattern representing this pointer.
    pub(crate) fn to_bits(&self) -> u32 {
        self.content
    }
}

pub(crate) struct RootsetReference {
    pub(super) content: Slabkey,
}

impl<TData> GcPointer<TData> {
    pub(crate) unsafe fn from_raw_unchecked(ptr: RawGcPointer) -> Self {
        Self {
            ptr,
            data: PhantomData,
        }
    }
    pub(crate) fn as_raw_mut(&mut self) -> &mut RawGcPointer {
        &mut self.ptr
    }
}

impl<TData> Into<RawGcPointer> for GcPointer<TData> {
    fn into(self) -> RawGcPointer {
        self.ptr
    }
}
impl<TData> AsRef<RawGcPointer> for GcPointer<TData> {
    fn as_ref(&self) -> &RawGcPointer {
        &self.ptr
    }
}
impl<TData> AsMut<RawGcPointer> for GcPointer<TData> {
    fn as_mut(&mut self) -> &mut RawGcPointer {
        &mut self.ptr
    }
}

const ROOT_REF_BIT: u32 = 1 << 31;

const HEAP_ENTRY_SHIFT: usize = core::mem::align_of::<HeapEntry>().ilog2() as usize;

impl RootsetReference {
    pub fn resolve(&mut self) -> &mut HeapEntry {
        let short = self.get_heapref().resolve() as *mut HeapEntry;
        unsafe { &mut *short }
    }

    #[inline]
    pub fn get_heapref(&self) -> RawHeapGcPointer {
        let entry = read_rootset_entry(&self.content);
        RawHeapGcPointer { content: entry }
    }
}
impl RawHeapGcPointer {
    pub fn resolve(&mut self) -> &mut HeapEntry {
        let gcptr = self.content;
        // it is not a root entry, and the top bit is 0.
        // that means we can just zero extend it and add the heap base to arrive
        // at the address
        let heap_base = get_heap_base();
        let addr = ((gcptr as usize) << HEAP_ENTRY_SHIFT) + heap_base as usize;
        let ptr = addr as *mut HeapEntry;
        unsafe { &mut *ptr }
    }
}

impl From<RootsetReference> for RawGcPointer {
    #[inline]
    fn from(value: RootsetReference) -> Self {
        Self {
            content: value.content.0 | ROOT_REF_BIT,
        }
    }
}
impl From<RawHeapGcPointer> for RawGcPointer {
    #[inline]
    fn from(value: RawHeapGcPointer) -> Self {
        Self {
            content: value.content,
        }
    }
}

impl RawGcPointer {
    #[inline]
    fn get_rootref(&self) -> u32 {
        self.content & !ROOT_REF_BIT
    }

    #[inline]
    pub(super) fn is_root(&self) -> bool {
        self.content & ROOT_REF_BIT > 0
    }

    #[inline]
    pub(crate) fn decode(&self) -> Result<RawHeapGcPointer, RootsetReference> {
        if self.is_root() {
            Err(RootsetReference {
                content: Slabkey(self.get_rootref()),
            })
        } else {
            Ok(RawHeapGcPointer {
                content: self.content,
            })
        }
    }

    #[inline]
    pub(crate) fn get_heapref(&self) -> RawHeapGcPointer {
        self.decode().unwrap_or_else(|e| e.get_heapref())
    }

    /// gets the heap ref repr without checking the root bit.
    /// only safe if the pointer is known to be a heap pointer (like if it is found on the heap)
    #[inline]
    pub(crate) unsafe fn get_heapref_unchecked(&self) -> RawHeapGcPointer {
        RawHeapGcPointer {
            content: self.content,
        }
    }

    /// resolve the pointer to the final heap address it is pointing to.
    pub(crate) fn resolve(&mut self) -> &mut HeapEntry {
        self.decode().map_or_else(
            |mut i| {
                let short = i.resolve() as *mut HeapEntry;
                unsafe { &mut *short }
            },
            |mut i| {
                let short = i.resolve() as *mut HeapEntry;
                unsafe { &mut *short }
            },
        )
    }
}
