
# gc design notes

Similar to the haskell gc, we will split the heap into multiple segments
and multiple generations.

Each generation has a dynamic number of segments in it.

Further, the gc will use the fact that once thunks are evaluated, everything that they
point to can immediately be promoted into the generation the thunk lives in
if it is a higher gen.

That way, during collection, no pointers into higher generations need to be considered.

Also, because we are managing pointers, it should be possible to limit the pointer size to 32 bits
for managed pointers, while still packing some extra metadata in.

## Metadata to track:
- tagged pointer?
- allocation size

# Heap object header
- Either object type or trace pointer
- Entry type (Forwarding Pointer or inline data)
- obje

## Pointer layout

[24 bits virtual addr] [8 bits metadata]

