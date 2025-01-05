use crate::{
    heap::{AllocatedPage, ZeroedPage},
    object::HeapObject,
    pointer::{HeapGcPointer, RawHeapGcPointer},
    CollectionHandle, GcError, Generation, GenerationCounter, PageSource, GC_PAGE_SIZE,
};
use std::cell::Cell;

pub(crate) use header::HeapEntry;

mod header {
    use crate::object::{widen::WideningAccessor, HeapObject};

    use super::*;
    pub(crate) struct HeapEntry {
        widening_function: usize,
    }

    pub struct ForwardingPointer {
        pub pointer: RawHeapGcPointer,
        // only set during garbage collection.
        pub fully_resolved: bool,
    }
    impl ForwardingPointer {
        /// follow the chain of forwardigs to the end.
        /// returns a pointer that is guaranteed to not be a forwarding entry
        pub(crate) fn follow_to_end(mut self) -> (RawHeapGcPointer, bool) {
            if self.fully_resolved {
                (self.pointer, true)
            } else {
                loop {
                    match self.pointer.resolve_mut().decode_mut() {
                        Ok(_) => return (self.pointer, false),
                        Err(forward) => {
                            if forward.fully_resolved {
                                return (forward.pointer, true);
                            }
                            self = forward;
                        }
                    }
                }
            }
        }
    }

    impl HeapEntry {
        #[inline]
        pub(crate) fn for_object<T: HeapObject + 'static>(_object: &T) -> Self {
            Self {
                widening_function: WideningAccessor::for_type::<T>().to_bitpattern(),
            }
        }

        fn decode_forward(bitrep: usize) -> ForwardingPointer {
            let pointer = unsafe { RawHeapGcPointer::from_bits((bitrep >> 32) as u32) };
            let fully_resolved = bitrep & 2 > 0;
            ForwardingPointer {
                pointer,
                fully_resolved,
            }
        }

        unsafe fn get_widener(&self) -> WideningAccessor {
            WideningAccessor::from_bitpattern(self.widening_function)
        }

        pub fn decode_mut(&mut self) -> Result<&mut dyn HeapObject, ForwardingPointer> {
            if self.widening_function as usize & 1 == 0 {
                let loaded = unsafe { self.get_widener() }.widen_mut(self.get_dataref_mut());
                Ok(loaded)
            } else {
                // this is actually a forwarding pointer.
                let bitrep = self.widening_function as usize;
                Err(Self::decode_forward(bitrep))
            }
        }
        fn decode(&self) -> Result<&dyn HeapObject, ForwardingPointer> {
            if self.widening_function as usize & 1 == 0 {
                let loaded = unsafe { self.get_widener().widen(self.get_dataref_unchecked()) };
                Ok(loaded)
            } else {
                // this is actually a forwarding pointer.
                let bitrep = self.widening_function as usize;
                Err(Self::decode_forward(bitrep))
            }
        }

        pub fn forward_to(&mut self, other: RawHeapGcPointer) {
            let repr = ((other.to_bits() as usize) << 32) | 1;
            self.widening_function = repr;
        }
        /// mark the entry as being a forwarding to another heap object.
        /// this method may only be called during garbage collection, and
        /// will indicate that the final destination is known, and that
        /// the page this object is on will soon be recycled.
        pub(super) fn forward_to_final(&mut self, other: &RawHeapGcPointer) {
            let repr = ((other.to_bits() as usize) << 32) | 3;
            self.widening_function = repr;
        }

        fn get_dataref_mut(&mut self) -> &mut () {
            unsafe { &mut *((self as *mut HeapEntry).add(1) as *mut ()) }
        }

        pub fn get_dataref(&self) -> &'static () {
            if self.widening_function as usize & 1 == 0 {
                unsafe { self.get_dataref_unchecked() }
            } else {
                let forward = Self::decode_forward(self.widening_function);
                forward.pointer.resolve().get_dataref()
            }
        }

        unsafe fn get_dataref_unchecked(&self) -> &'static () {
            &*((self as *const HeapEntry).add(1) as *const ())
        }

        #[inline]
        pub(crate) fn load(&self) -> &dyn HeapObject {
            self.decode().unwrap_or_else(|forward| {
                let resolved = forward.pointer.resolve() as *const HeapEntry;
                unsafe { &*resolved }.load()
            })
        }

        #[inline]
        pub(crate) fn load_mut(&mut self) -> &mut dyn HeapObject {
            self.decode_mut().unwrap_or_else(|mut forward| unsafe {
                let resolved = forward.pointer.resolve_mut() as *mut HeapEntry;
                (*resolved).load_mut()
            })
        }
    }
}

pub(crate) struct Page {
    /// the lowest valid address that belongs to this page
    inner: AllocatedPage,
    /// the latest non-free byte of this page.
    /// (e.g. a value 1 past the free range)
    free_top: Cell<*mut HeapEntry>,

    // a past-the-end pointer to the highest non-scavengable memory address
    scavenge_end_addr: Cell<usize>,
}

impl Page {
    pub(crate) fn new(page: ZeroedPage) -> Self {
        let page: AllocatedPage = page.into();
        debug_assert!(page.base as usize % GC_PAGE_SIZE == 0);

        let free_top = unsafe { page.base.add(GC_PAGE_SIZE) } as *mut HeapEntry;
        Self {
            inner: page,
            free_top: Cell::new(free_top),
            scavenge_end_addr: Cell::new(free_top as usize),
        }
    }

    pub fn into_allocated(self) -> AllocatedPage {
        self.inner
    }

    pub(crate) fn scavenge_content(
        &self,
        gc_handle: &mut CollectionHandle,
        gc_max_generation: Generation,
    ) -> Result<(), GcError> {
        // a past-the-end pointer to the highest non-scavengable memory address
        let mut scavenge_end_addr = self.scavenge_end_addr.get();
        // a pointer to the place we began to scavenge.
        let mut scavenge_start = self.free_top.get();

        while (scavenge_start as usize) < scavenge_end_addr {
            let mut scavenge_current = scavenge_start as *mut HeapEntry;
            while (scavenge_current as usize) < scavenge_end_addr {
                // scavenge pointed-to object.
                let object = unsafe {
                    // the pointed_to object is actually a hole in the heap caused by alignment.
                    // instead, search for the next value by searching for the next properly-aligned heap entry
                    if core::ptr::read(scavenge_current as *const usize) == 0 {
                        scavenge_current = scavenge_current.add(1);
                        continue;
                    }
                    (&mut *scavenge_current).load_mut()
                };

                object.trace(&mut |gc_pointer| {
                    let mut heap_ptr = unsafe { gc_pointer.get_heapref_unchecked() };
                    scavenge_heap_pointer(gc_handle, &mut heap_ptr, gc_max_generation);
                    unsafe { core::ptr::write(gc_pointer, heap_ptr.into()) }
                });
                scavenge_current = advance_entrypointer(scavenge_current, object.allocation_size());
            }
            // set the end to where we started as everything up to here has been scavenged
            scavenge_end_addr = scavenge_start as usize;
            // and the start to the allocation pointer, as there may have been additional objects added that need to be scavenged too
            scavenge_start = self.free_top.get();
        }
        self.scavenge_end_addr.set(scavenge_end_addr);

        fn advance_entrypointer(ptr: *mut HeapEntry, allocation_size: usize) -> *mut HeapEntry {
            let mut addr = ptr as usize;
            // the header itself
            addr += core::mem::size_of::<HeapEntry>();
            // the allocated data
            addr += allocation_size;

            // ensure the correct alignment to account for padding.
            const ALIGN: usize = core::mem::align_of::<HeapEntry>();
            let align = addr % ALIGN;
            if align > 0 {
                addr += ALIGN - align;
            }

            addr as *mut HeapEntry
        }

        Ok(())
    }

    /// attempt to move the provided object into this heap page.
    /// Will return the object on failure.
    pub fn try_alloc<T: HeapObject + 'static>(
        &self,
        object: T,
        counter: &mut GenerationCounter,
    ) -> Result<HeapGcPointer<T>, T> {
        let alloc_size = object.allocation_size();

        if let Some((header_ptr, data_ptr)) =
            self.try_reserve(alloc_size, core::mem::align_of_val(&object), counter)
        {
            let cast_data_ptr = data_ptr as *mut T;
            unsafe {
                core::ptr::write(cast_data_ptr, object);
                core::ptr::write(header_ptr, HeapEntry::for_object(&*cast_data_ptr));

                // unroot all data pointers in there to ensure we don't get leaks
                (*cast_data_ptr).trace(&mut |ptr| ptr.unroot());

                let gc_ptr = RawHeapGcPointer::from_addr(header_ptr);
                Ok(HeapGcPointer::from_raw_unchecked(gc_ptr))
            }
        } else {
            Err(object)
        }
    }

    /// attempt to reserve enough space for the requested allocation.
    /// The requested alignment must be a power of 2.
    ///
    /// The returned data pointer will always point to the beginning of the allocation.
    pub fn try_reserve(
        &self,
        requested_space: usize,
        requested_alignment: usize,
        counter: &GenerationCounter,
    ) -> Option<(*mut HeapEntry, *mut u8)> {
        debug_assert!(requested_alignment.is_power_of_two());

        let free_top = self.free_top.get() as *mut u8;

        #[cold]
        fn align_down_general(ptr: *mut u8, alignment: usize) -> *mut u8 {
            let val = (ptr as usize / alignment) * alignment;
            val as *mut u8
        }

        // ensure that we align to at least the alignment of our heap items.
        // if we align to more, it would still be fine for the header.
        let data_alignment = core::mem::align_of::<HeapEntry>().max(requested_alignment);

        // compute the pointer to our data object.
        // because we ensured the alignment, it has also legal alignment for the object header.
        let data_ptr = {
            // first requirement: enough space in the allocation.
            let ptr = unsafe { free_top.sub(requested_space) };
            // second requirement: enough alignment
            const HEAP_ENTRY_ALIGN: usize = core::mem::align_of::<HeapEntry>();
            if requested_alignment <= HEAP_ENTRY_ALIGN {
                // align to the minimum object header.
                ((ptr as usize / HEAP_ENTRY_ALIGN) * HEAP_ENTRY_ALIGN) as *mut u8
            } else {
                // align to the requested alignment, as it exceeds the minimum.
                // we do this in an extra function to not pay the cost for the div in the common path
                align_down_general(ptr, data_alignment)
            }
        };

        // this is legal because we ensured that the data pointer is at least aligned
        // as much as the object header needs.
        let header_ptr = unsafe { (data_ptr as *mut HeapEntry).sub(1) };

        if (self.inner.base as usize) < header_ptr as usize {
            // our allocation is legal, so perform it by writing the free top.
            self.free_top.set(header_ptr);

            counter.record_allocation(requested_space);

            // and return the pointer to the allocation
            Some((header_ptr, data_ptr))
        } else {
            // performing this allocation would result in us going off the page.
            None
        }
    }
}

/// scavenge the provided heap pointer, ensuring that it points to a location
/// that is valid after the current collection is complete.
pub(crate) fn scavenge_heap_pointer(
    gc_handle: &mut CollectionHandle,
    heap_ptr: &mut RawHeapGcPointer,
    gc_max_generation: Generation,
) {
    let generation = gc_handle.get_generation(&heap_ptr);
    if generation > gc_max_generation {
        // we do not need to scavenge this particular pointer.
        return;
    }
    let header = heap_ptr.resolve_mut();
    let new_gc_pointer = match header.decode_mut() {
        Ok(obj) => {
            // copy the value.
            let copy = copy_object_to_new_allocation(obj, gc_handle, generation.next_higher());
            header.forward_to_final(&copy);
            copy
        }
        Err(forward) if forward.fully_resolved => forward.pointer,
        Err(forward) => {
            // the pointer was already forwarded.
            // since there may have been a chain of forwardings, follow them until the final heap entry
            let (mut resolved, fully_resolved) = forward.follow_to_end();
            let new_gc_pointer = if fully_resolved {
                // at some point in the chain, we already scavenged whatever this pointer was pointing to.
                // so we can just use that value.
                resolved
            } else {
                let generation = gc_handle.get_generation(&resolved);
                if generation > gc_max_generation {
                    // the pointer was pointing to some value in a generation we are not collecting yet.
                    resolved
                } else {
                    // we _do_ have to scavenge the value it is pointing to.
                    let final_header = resolved.resolve_mut();
                    let new_pointer = copy_object_to_new_allocation(
                        final_header.load_mut(),
                        gc_handle,
                        generation.next_higher(),
                    );
                    // ensure that the header of the final object is updated too, to ensure it is not
                    // duplicated if there is another reference to it later.
                    final_header.forward_to_final(&new_pointer);
                    new_pointer
                }
            };
            header.forward_to_final(&new_gc_pointer);
            new_gc_pointer
        }
    };
    *heap_ptr = new_gc_pointer;
}

pub(crate) fn promote_object(
    gc_handle: &mut impl PageSource,
    heap_ptr: &mut RawHeapGcPointer,
    target_generation: Generation,
) -> RawHeapGcPointer {
    let header = heap_ptr.resolve_mut();

    let new_heap_ptr = match header.decode_mut() {
        Ok(obj) => copy_object_to_new_allocation(obj, gc_handle, target_generation),
        Err(forward) => {
            let (mut resolved, _) = forward.follow_to_end();
            if gc_handle.get_generation(&resolved).0 >= target_generation.0 {
                // the object it was pointing to was already a member of the target generation.
                resolved
            } else {
                // we need to copy the object.
                let header = resolved.resolve_mut();
                let mut new_value =
                    copy_object_to_new_allocation(header.load_mut(), gc_handle, target_generation);

                // ensure that all the indirectly referenced values are also promoted.
                new_value.resolve_mut().load_mut().trace(&mut |ptr| {
                    let mut heap_ptr = unsafe { ptr.get_heapref_unchecked() };
                    if gc_handle.get_generation(&heap_ptr) >= target_generation {
                        return;
                    }
                    let new_value = promote_object(gc_handle, &mut heap_ptr, target_generation);
                    unsafe { core::ptr::write(ptr, new_value.into()) }
                });

                header.forward_to(new_value.clone());
                new_value
            }
        }
    };

    header.forward_to(new_heap_ptr.clone());
    new_heap_ptr
}

fn copy_object_to_new_allocation(
    obj: &mut dyn HeapObject,
    gc_handle: &mut impl PageSource,
    target_generation: Generation,
) -> RawHeapGcPointer {
    let alloc_size = obj.allocation_size();
    let alloc_align = obj.allocation_alignment();

    let header = unsafe { (obj as *const dyn HeapObject as *const HeapEntry).offset(-1) };
    loop {
        let (allocation_page, counter) = gc_handle.get_allocation_page(target_generation);
        if let Some((destination_header, _destination_data)) =
            allocation_page.try_reserve(alloc_size, alloc_align, counter)
        {
            let total_copy_size = alloc_size + core::mem::size_of::<HeapEntry>();
            unsafe {
                let source_bytes =
                    core::slice::from_raw_parts(header as *const u8, total_copy_size);
                let dest_bytes =
                    core::slice::from_raw_parts_mut(destination_header as *mut u8, total_copy_size);
                dest_bytes.copy_from_slice(source_bytes);

                break RawHeapGcPointer::from_addr(destination_header);
            }
        }
        gc_handle.finish_allocation_page(target_generation);
    }
}
