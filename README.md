Bit_Sets
========

This library implements packed boolean arrays in a way that allows performant usage.

If you declare an array of boolean the normal way:

    type Boolean_Array is array (Positive range <>) of Boolean;

Each array element will be a Byte in memory due to memory alignment. Unfortunately this
wastes a tremendous amount of memory. Sometimes for code exercises or particular algorithms
you must allocate huge boolean arrays (for example the Sieve of Eratosthenes) and doing so
normally means you use 8x the amount of memory.

This library will represent the boolean array as an array of 64 bit
unsigned words. Each bit is one of the booleans. This reduces memory usage by 8x.

Usage
-----

Instantiate `Bit_Sets.Generic_Sets` with the index type you need. Sets live on the heap,
so billions of bits are fine, and are freed automatically when they go out of scope.

    with Bit_Sets.Generic_Sets;

    package Flags is new Bit_Sets.Generic_Sets (Natural);
    use Flags;

    S : Bit_Set := Create (0, 1_000_000_000);

    S.Include (42);           --  Single indexes
    S.Exclude (42);
    S.Include (100, 200);     --  Ranges, written a whole word at a time
    S.Contains (150);         --  True
    S.Count;                  --  101, using the hardware pop count

    for I of S loop           --  Visits only the members, skipping empty words
       ...
    end loop;

Sets of the same bounds combine in place with `Union`, `Intersection`, `Difference` and
`Symmetric_Difference`, or into a new set with `or`, `and`, `-`, `xor` and `not`. `Bit_Set` is
limited so a large set is never copied by accident; use `Copy` or `Assign` when you mean to.

`Shift_Left` and `Shift_Right` move every member up or down by some amount, a whole word at a
time. They make bitset dynamic programming fast, for example the sums reachable from a list of
weights:

    Reachable : Bit_Set := Create (0, Max_Sum);
    Reachable.Include (0);
    for W of Weights loop
       Union_Shifted (Reachable, Reachable, W);  --  Reachable := Reachable or Shift_Left (Reachable, W)
    end loop;

`Union_Shifted` does the shift and the union in one pass without allocating, and its source
may be its target.

`S'Image` lists the members with runs as ranges, e.g. `{-5 .. 2, 10, 58, 59, 100 .. 130}`.

Out of range indexes are caught by preconditions, compile with `-gnata` to check them.

See [example](./example) for a Sieve of Eratosthenes comparing a `Boolean` array to a `Bit_Set`.

Building
--------

This project is built with [Alire](https://alire.ada.dev). With alire installed:

    alr build

Without alire, copy [share/bit_sets/bit_sets_config.gpr](./share/bit_sets/bit_sets_config.gpr)
to `config/bit_sets_config.gpr` and build with:

    gprbuild bit_sets.gpr

The [example](./example) and [tests](./tests) crates pin the library from the repository root.
