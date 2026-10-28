--  Copyright (C) 2024 Michael Hardeman
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--  You should have received a copy of the GNU Library General Public
--  License along with this library; if not, write to the
--  Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
--  Boston, MA  02110-1301, USA.

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

------------
-- Blocks --
------------
--  Operations on the 64 bit blocks a Bit_Set is stored in. Blocks are a
--  fixed 64 bits on every platform. 32 bit targets split each operation in
--  two, which is still cheap, and the behavior never depends on the target.
private package Bit_Sets.Blocks with Pure is

   Block_Size : constant := 64;

   type Block_Type is new Unsigned_64;

   type Bit_Offset is range 0 .. Block_Size - 1;
   type Bit_Count  is range 0 .. Block_Size;

   ALL_ZEROS : constant Block_Type := Block_Type'First;
   ALL_ONES  : constant Block_Type := Block_Type'Last;

   --------------
   -- Bit_Mask --
   --------------
   --  A mask where only the bit at Offset is set
   function Bit_Mask (Offset : Bit_Offset) return Block_Type
     is (Shift_Left (1, Natural (Offset))) with Inline_Always;

   --------------
   -- Low_Mask --
   --------------
   --  A mask where the lowest Count bits are set
   --
   --  Examples:
   --     Low_Mask (0)  = 2#0000#
   --     Low_Mask (1)  = 2#0001#
   --     Low_Mask (3)  = 2#0111#
   --     Low_Mask (64) = ALL_ONES
   function Low_Mask (Count : Bit_Count) return Block_Type
     is (if Count = Block_Size then ALL_ONES else Shift_Left (1, Natural (Count)) - 1) with Inline_Always;

   ---------------
   -- High_Mask --
   ---------------
   --  A mask where every bit at Offset and above is set
   function High_Mask (Offset : Bit_Offset) return Block_Type
     is (not Low_Mask (Bit_Count (Offset))) with Inline_Always;

   ---------------
   -- Pop_Count --
   ---------------
   --  The number of bits set in the block
   function Pop_Count (Block : Block_Type) return Natural with Inline_Always;

   --------------------------
   -- Count_Trailing_Zeros --
   --------------------------
   --  The offset of the lowest set bit
   function Count_Trailing_Zeros (Block : Block_Type) return Bit_Offset
     with Pre => Block /= ALL_ZEROS, Inline_Always;

private

   function Builtin_Pop_Count (X : unsigned_long_long) return int
     with Import, Convention => Intrinsic, External_Name => "__builtin_popcountll";

   function Builtin_Count_Trailing_Zeros (X : unsigned_long_long) return int
     with Import, Convention => Intrinsic, External_Name => "__builtin_ctzll";

   function Pop_Count (Block : Block_Type) return Natural
     is (Natural (Builtin_Pop_Count (unsigned_long_long (Block))));

   function Count_Trailing_Zeros (Block : Block_Type) return Bit_Offset
     is (Bit_Offset (Builtin_Count_Trailing_Zeros (unsigned_long_long (Block))));

end Bit_Sets.Blocks;
