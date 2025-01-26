use std::marker::PhantomData;

use crate::{get_heap_base, heap_page::HeapEntry};

use super::rootset::{read_rootset_entry, Slabkey};

/// a pointer that may either be rooted or a direct on-heap reference to something.
pub struct GcPointer<TData> {
    ptr: RawGcPointer,
    data: PhantomData<TData>,
}

impl<TData> core::fmt::Debug for GcPointer<TData> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GcPointer")
            .field("type", &core::any::type_name_of_val(self))
            .field("ptr", &self.ptr)
            .finish()
    }
}

/// representation:
/// bits 0..30: real pointer data
/// bit 31:
///     1 -> This is a root reference
///     0 -> This is a heap reference
pub struct RawGcPointer {
    content: u32,
    _not_send: PhantomData<*const ()>,
}

impl core::fmt::Debug for RawGcPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("RawGcPointer");
        match self.decode() {
            Ok(raw) => debug.field("raw_value", &raw),
            Err(root) => debug.field("root_reference", &root),
        };
        debug.finish()
    }
}

/// representation:
/// the lower 32 bits of the heap addr, shifted right by the min alignment of
/// the heap header
#[derive(Clone)]
pub(crate) struct RawHeapGcPointer {
    pub(super) content: u32,
}
impl core::fmt::Debug for RawHeapGcPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:#x}", self.content))
    }
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
impl<T> AsRef<RawHeapGcPointer> for HeapGcPointer<T> {
    fn as_ref(&self) -> &RawHeapGcPointer {
        &self.raw
    }
}

impl RawHeapGcPointer {
    /// Create a new instance of this pointer.
    /// This function is only valid if the heap address
    /// is properly allocated on the gc heap and
    /// valid.
    pub(crate) unsafe fn from_addr(raw_ptr: *mut HeapEntry) -> Self {
        let mask: usize = 0xff_ff_ff_ff;
        let value = ((raw_ptr as usize & mask) >> HEAP_ENTRY_SHIFT) as u32;
        Self { content: value }
    }

    pub(crate) const unsafe fn from_bits(bitrep: u32) -> Self {
        Self { content: bitrep }
    }
    /// get the bitpattern representing this pointer.
    pub(crate) fn to_bits(&self) -> u32 {
        self.content
    }

    pub(crate) fn to_heap_offset(&self) -> usize {
        (self.content as usize) << HEAP_ENTRY_SHIFT
    }
}

pub(crate) struct RootsetReference {
    pub(super) content: Slabkey,
}
impl core::fmt::Debug for RootsetReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RootsetReference")
            .field("id", &self.content.0)
            .field("object", &self.get_heapref())
            .finish()
    }
}

impl<TData> GcPointer<TData> {
    pub(crate) unsafe fn from_raw_unchecked(ptr: RawGcPointer) -> Self {
        Self {
            ptr,
            data: PhantomData,
        }
    }
    pub fn as_raw(&self) -> &RawGcPointer {
        self.as_ref()
    }
}
impl<TData> From<HeapGcPointer<TData>> for GcPointer<TData> {
    fn from(value: HeapGcPointer<TData>) -> Self {
        Self {
            ptr: value.raw.into(),
            data: PhantomData,
        }
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

pub(crate) const HEAP_ENTRY_SHIFT: usize = core::mem::align_of::<HeapEntry>().ilog2() as usize;

impl RootsetReference {
    #[inline]
    pub fn get_heapref(&self) -> RawHeapGcPointer {
        read_rootset_entry(&self.content)
    }
}
impl RawHeapGcPointer {
    fn resolve_untyped(&self) -> usize {
        let gcptr = self.content;
        // it is not a root entry, and the top bit is 0.
        // that means we can just zero extend it and add the heap base to arrive
        // at the address
        let heap_base = get_heap_base();
        ((gcptr as usize) << HEAP_ENTRY_SHIFT) + heap_base as usize
    }

    pub fn resolve(&self) -> &'static HeapEntry {
        let ptr = self.resolve_untyped() as *const HeapEntry;
        unsafe { &*ptr }
    }

    pub fn resolve_mut(&mut self) -> &mut HeapEntry {
        let ptr = self.resolve_untyped() as *mut HeapEntry;
        unsafe { &mut *ptr }
    }
}

impl From<RootsetReference> for RawGcPointer {
    #[inline]
    fn from(value: RootsetReference) -> Self {
        Self {
            content: value.content.0 | ROOT_REF_BIT,
            _not_send: PhantomData,
        }
    }
}
impl From<RawHeapGcPointer> for RawGcPointer {
    #[inline]
    fn from(value: RawHeapGcPointer) -> Self {
        Self {
            content: value.content,
            _not_send: PhantomData,
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
        debug_assert!(!self.is_root());

        RawHeapGcPointer {
            content: self.content,
        }
    }
}
