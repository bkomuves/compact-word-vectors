
compact-word-vectors
====================

This a (GHC) Haskell library, which implements a data structure designed
to store vectors of small integers with minimal memory overhead.

For vectors of non-negative integers, see the module `WordVec`; for signed 
integers (which is a bit slower) see `IntVec`. The underlying raw binary storage
is implemented in `Blob`.

For example the list `[1..14] :: [Int]` consumes 560 bytes (70 words) on 
a 64 bit machine, while the corresponding `WordVec` takes only
16 bytes (2 words), and the one corresponding to `[101..115]` still only 
24 bytes (3 words).

Unboxed arrays (`UArray`) or unboxed vectors (`Data.Vector.Unboxed`)
are better than lists, as they only have a constant overhead, but those 
constants are big: 13 words (104 bytes on 64 bit) for unboxed arrays, 
and 6 words (48 bytes) for unboxed vectors. And you still have to select 
the number of bits per element in advance.
