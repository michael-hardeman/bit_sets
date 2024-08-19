Bit_Sets
========

This library implements packed boolean arrays in a way that allows preformant useage.

if you declare an array of boolean the normal way:

    type Boolean_Array is array (Positive range <>) of Boolean;

Each array element will be a Byte in memory due to memory alignment. Unfortunately this
wastes a tremendous amount of memory. Sometimes for code exercises or particular algorithms
you must allocate huge boolean arrays (for example the Sieve of Eratosthenes) and doing so
normally means you use 8x the amount of memory.

This library will represent the boolean array as an array of unsigned words (matching your
system word size). Each bit is one of the booleans. This reduces memory usage by 8x.
