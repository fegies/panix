
# gc design notes

Similar to the haskell gc, we will split the heap into multiple segments
and multiple generations.

Each generation has a dynamic number of pages in it.

Further, the gc will use the fact that once thunks are evaluated, everything that they
point to can immediately be promoted into the generation the thunk lives in
if it is a higher gen.

That way, during collection, no pointers into higher generations need to be considered.

Also, because we are managing pointers, it should be possible to limit the pointer size to 32 bits
for managed pointers, while still packing some extra metadata in.

## Metadata to track:
- the object type
- is this a heap object or a stack reference?
- allocation size and alignment

# Heap object header
- Entry type (Forwarding Pointer or inline data)
- has the object been forwarded to its final destination?

## Pointer layout and types

`GcPointer<T>`: A wrapper around the RawGcPointer,
identical in layout, only adding type information to the compiler

`RawGcPointer`: The only 'exposed' pointer type.
This pointer may either represent a stack reference or
a position onto the heap.

These values are discriminated by the value of the highest
bit.

Valid bitpatterns are 

1 [31 bits of rootset index]

0 [31 bits of heap pointer]

`RootsetReference`: A 'gc root' reference. These objects form
the entry point for garbage collection and are indices into
a thread-local array of `RawHeapGcPointer`s.

Layout: 

[1 reserved bit, must be 0] [31 bits of index]

`RawHeapGcPointer`:

This is a comrpessed reference to a heap entry.

Layout:

[1 reserved bit, must be 0] [31 bits of object id]
where the object id is derived from its address on the heap in the following manner:
- subtract the heap base address to arrive at the relative offset from the heap base.
- shift the address right by the log2 of the alignment of the heap entry, as those bits must be all zero anyway.
- (implicit) zero out the top bit

To decode, widen to 64 bits, shift left to get the bits to the same position and add the heap base.

## Heap entries

A heap entry is data that is actually stored on the allocated garbage collected heap, with 
an additional object header.

Due to the object header, the minimum alignment of all allocations is that of the header.
Inside of the page, data is stored in a compact manner with minimum gaps between allocations.

The allocation size and alignment is stored implicitly, by storing a function
that converts a pointer to the allocated data to a trait object which provides access to that information.

The lowest 2 bits store data on if the current entry should be interpreted as a forwarding pointer
and if all resolution has been performed.

