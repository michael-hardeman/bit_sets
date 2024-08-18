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

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Bit_Sets is
   pragma Elaborate_Body (Bit_Sets);

   type Bit_Offset is mod Standard'Word_Size;
   type Bit_Index is range 1 .. Standard'Word_Size;
   type Bit_Count is range 0 .. Standard'Word_Size;
   type Block_Type is mod 2 ** Standard'Word_Size;

   ALL_ZEROS        : constant Block_Type := Block_Type'First;
   ALL_ONES         : constant Block_Type := Block_Type'Last;
   LOW_ALTERNATING  : Block_Type;
   HIGH_ALTERNATING : Block_Type;

   type Block_Array is array (Long_Long_Integer range <>) of Block_Type;

   -------------
   -- Bit_Set --
   -------------
   --  The internals are exposed in the private section
   --  Essentially we store a list of full blocks and the
   --  final jagged block separate.
   type Bit_Set is new Ada.Finalization.Controlled with private;

   ----------------
   -- Initialize --
   ----------------
   --  You must call initialize to allocate the needed blocks. The compiler
   --  will automtically finalize and release the memory following the
   --  controlled type rules.
   procedure Initialize (Bits        : in out Bit_Set;          -- The Bits to be initialized
                         Size        : Long_Long_Integer;       -- The number of bits you need
                         First_Index : Long_Long_Integer := 1); -- The first index to start counting from when iterating

   function Size_In_Bits     (Bits : Bit_Set)                     return Long_Long_Integer;
   function Test_All_Equal   (Bits : Bit_Set; Value : Block_Type) return Boolean;
   function Test_All_Set     (Bits : Bit_Set)                     return Boolean;
   function Test_All_Unset   (Bits : Bit_Set)                     return Boolean;
   function Count_Population (Bits : Bit_Set)                     return Long_Long_Integer;

   ---------------
   -- To_Offset --
   ---------------
   --  Converts an Offset to an Index
   function To_Offset (Index : Bit_Index)
                       return Bit_Offset is (Bit_Offset (Bit_Count (Index) - 1)) with Inline_Always;

   --------------
   -- To_Index --
   --------------
   --  Converts an Index to an Offset
   function To_Index (Offset : Bit_Offset)
                      return Bit_Index is (Bit_Index (Bit_Count (Offset) + 1)) with Inline_Always;

   --------------
   -- Bit_Mask --
   --------------
   --  returns a mask where the requested bit is set
   function Bit_Mask (Offset : Bit_Offset)
                      return Block_Type is (2 ** Natural (Offset)) with Inline_Always;

   function Bit_Mask (Index : Bit_Index)
                      return Block_Type is (Bit_Mask (To_Offset (Index))) with Inline_Always;

   -----------------------
   -- Leading_Bits_Mask --
   -----------------------
   --  returns a mask where the indicated number of leading bits are all 1s
   --
   --  Examples for 8 bit system:
   --     Leading_Bits_Mask (0)  = 2#00000000#
   --     Leading_Bits_Mask (1)  = 2#00000001#
   --     Leading_Bits_Mask (2)  = 2#00000011#
   --     Leading_Bits_Mask (3)  = 2#00000111#
   --     Leading_Bits_Mask (4)  = 2#00001111#
   --     Leading_Bits_Mask (5)  = 2#00011111#
   --     Leading_Bits_Mask (6)  = 2#00111111#
   --     Leading_Bits_Mask (7)  = 2#01111111#
   --     Leading_Bits_Mask (9)  = 2#11111111#
   function Leading_Bits_Mask (Count : Bit_Count)
                               return Block_Type is (
                                 if Count = 0 then 0
                                 else ((Bit_Mask (Bit_Offset (Count - 1)) - 1) * 2) + 1);

   --------------
   -- Test_Bit --
   --------------
   --  Returns true if the indicated bit is 1, false if 0
   function Test_Bit (Block    : Block_Type;
                      Bit_Mask : Block_Type)
                      return Boolean is (0 /= (Block and Bit_Mask)) with Inline_Always;

   function Test_Bit (Block  : Block_Type;
                      Offset : Bit_Offset)
                      return Boolean is (Test_Bit (Block, Bit_Mask (Offset))) with Inline_Always;

   function Test_Bit (Block : Block_Type;
                      Index : Bit_Index)
                      return Boolean is (Test_Bit (Block, Bit_Mask (Index))) with Inline_Always;

   -------------
   -- Set_Bit --
   -------------
   --  Sets the indicated bit to 1
   procedure Set_Bit (Block : in out Block_Type; Offset : Bit_Offset) with Inline_Always;
   procedure Set_Bit (Block : in out Block_Type; Index  : Bit_Index)  with Inline_Always;

   ---------------
   -- Unset_Bit --
   ---------------
   --  Sets the indicated bit to 0
   procedure Unset_Bit (Block : in out Block_Type; Offset : Bit_Offset) with Inline_Always;
   procedure Unset_Bit (Block : in out Block_Type; Index  : Bit_Index)  with Inline_Always;

   ---------------
   -- Pop_Count --
   ---------------
   --  Gives the number of bits set to 1 in the block
   function Pop_Count (Block : Block_Type) return Natural;

   --------------------
   -- Iterate_Blocks --
   --------------------
   --  Used to iterate through the blocks and modify/inspect each one
   procedure Iterate_Blocks (Bits : in out Bit_Set;

                             --  Run on all full blocks of Word'Size (64 or 32 or whatever) bits
                             Block_Callback : access procedure (
                                --  the block to inspect/modify
                                Block : in out Block_Type;
                                --  The index of the first bit in the full bit_set
                                First_Index : Long_Long_Integer);

                             --  Run on the final partially filled block of left over bits
                             --  not divisible by Word'Size (64 or 32 or whatever)
                             Remaining_Callback : access procedure (
                                --  the block to inspect/modify
                                Block : in out Block_Type;
                                --  The index of the first bit in the full bit_set
                                First_Index : Long_Long_Integer;
                                --  How many bits are in the remaining block
                                Remaining_Bits : Bit_Count;
                                --  Allows you to easily tell which bits are used/ignored)
                                Remaining_Mask : Block_Type));

   ------------------
   -- Iterate_Bits --
   ------------------
   --  Used to iterate through each individual bit and modify/inspect each one
   procedure Iterate_Bits (Bits : in out Bit_Set;

                           --  Run on each bit
                           Bit_Callback : access procedure (
                              --  the block to inspect/modify
                              Block : in out Block_Type;
                              --  The index of this bit relative to the entire set
                              Index : Long_Long_Integer;
                              --  The offset of this bit within the block
                              Offset : Bit_Offset));
                              --  I was going to provide a Mask but then I realized the user
                              --  of the function could just generate one with Bit_Mask (Offset)
                              --  and if they didn't use the mask it was an extra overhead
                              --  operation every single bit.
                              --  A bit mask with 1 set only on the bit matching the offset
                              --  Mask : Block_Type);

   -------------------
   -- Reduce_Blocks --
   -------------------
   --  Used to iterate through the blocks and generate some result about the entire group
   generic
      type Result_Type is private;
   procedure Reduce_Blocks (Result : in out Result_Type;
                            Bits   : Bit_Set;

                            --  Run on all full blocks of Word'Size (64 or 32 or whatever) bits
                            Reduce_Block_Callback : access procedure (
                               --  The result of the block reduction
                               Result : in out Result_Type;
                               --  the block to inspect
                               Block : Block_Type;
                               --  The index of the first bit in the full bit_set
                               First_Index : Long_Long_Integer);

                            --  Run on the final partially filled block of left over bits
                            --  not divisible by Word'Size (64 or 32 or whatever)
                            Reduce_Remaining_Callback : access procedure (
                               --  The result of the block reduction
                               Result : in out Result_Type;
                               --  the block to inspect
                               Block : Block_Type;
                               --  The index of the first bit in the full bit_set
                               First_Index : Long_Long_Integer;
                               --  How many bits are in the remaining block
                               Remaining_Bits : Bit_Count;
                               --  Allows you to easily tell which bits are used/ignored);
                               Remaining_Mask : Block_Type));

   -----------------
   -- Reduce_Bits --
   -----------------
   --  Used to iterate through each individual bit and generate some result about the entire group
   generic
      type Result_Type is private;
   procedure Reduce_Bits (Result : in out Result_Type;
                          Bits   : Bit_Set;

                            -- Run on each bit
                            Reduce_Bit_Callback : access procedure (
                              --  The result of the bit reduction
                              Result : in out Result_Type;
                              --  the block to inspect
                              Block : Block_Type;
                              --  The index of this bit relative to the entire set
                              Index : Long_Long_Integer;
                              --  The offset of this bit within the block
                              Offset : Bit_Offset));

-------
private
-------

   type Block_Array_Ptr is access all Block_Array;

   procedure Free is new Ada.Unchecked_Deallocation (Block_Array, Block_Array_Ptr);

   type Bit_Set is new Ada.Finalization.Controlled with record
      Blocks          : Block_Array_Ptr   := null; -- Whole blocks of 64 bits
      First_Index     : Long_Long_Integer := 1;    -- Where to start counting indexes from when iterating
      Remaining_Block : Block_Type        := 0;    -- Final block of left over bits
      Remaining_Bits  : Bit_Count         := 0;    -- Count of left over bits
      Remaining_Mask  : Block_Type        := 0;    -- Mask of used left over bits
   end record;

   overriding procedure Finalize (Bits : in out Bit_Set);

end Bit_Sets;
