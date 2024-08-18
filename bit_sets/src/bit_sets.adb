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

with Interfaces.C; use Interfaces.C;

package body Bit_Sets is

   --------------------------------------
   -- Generate_Alternating_Bit_Pattern --
   --------------------------------------
   function Generate_Alternating_Bit_Pattern return Block_Type is
      Output : Block_Type := 0;
   begin
      for I in Bit_Offset'Range loop
         if I mod 2 = 0 then
            Output := Output or Bit_Mask (I);
         end if;
      end loop;
      return Output;
   end Generate_Alternating_Bit_Pattern;

   -------------
   -- Set_Bit --
   -------------
   procedure Set_Bit (Block : in out Block_Type; Offset : Bit_Offset) is begin
      Block := Block or Bit_Mask (Offset);
   end Set_Bit;
   procedure Set_Bit (Block : in out Block_Type; Index  : Bit_Index) is begin
      Set_Bit (Block, To_Offset (Index));
   end Set_Bit;

   ---------------
   -- Unset_Bit --
   ---------------
   procedure Unset_Bit (Block : in out Block_Type; Offset : Bit_Offset) is begin
      Block := Block xor Bit_Mask (Offset);
   end Unset_Bit;
   procedure Unset_Bit (Block : in out Block_Type; Index  : Bit_Index) is begin
      Unset_Bit (Block, To_Offset (Index));
   end Unset_Bit;

   ---------------
   -- Pop_Count --
   ---------------
   --  unsigned (32)
   function Builtin_Pop_Count (X : unsigned) return int
      with Import, Convention => Intrinsic, External_Name => "__builtin_popcount";
   function Pop_Count_Wrapper (Block : Block_Type) return Natural
      is (Natural (Builtin_Pop_Count (unsigned (Block))));
   --  unsigned long (64)
   function Builtin_Pop_Count_Long (X : unsigned_long) return int
      with Import, Convention => Intrinsic, External_Name => "__builtin_popcountl";
   function Pop_Count_Long_Wrapper (Block : Block_Type) return Natural
      is (Natural (Builtin_Pop_Count_Long (unsigned_long (Block))));
   --  unsigned long long (also 64 I tested)
   function Builtin_Pop_Count_Long_Long (X : unsigned_long_long) return int
      with Import, Convention => Intrinsic, External_Name => "__builtin_popcountll";
   function Pop_Count_Long_Long_Wrapper (Block : Block_Type) return Natural
      is (Natural (Builtin_Pop_Count_Long_Long (unsigned_long_long (Block))));
   --  anything else
   function Pop_Count_Slow (Block : Block_Type) return Natural is
      Pop : Natural := 0;
   begin
      for I in Bit_Offset'First .. Bit_Offset'Last loop
         if Test_Bit (Block, I) then
            Pop := Pop + 1;
         end if;
      end loop;
      return Pop;
   end Pop_Count_Slow;

   --  Precompute the correct path and cache it
   type Pop_Count_Implementation is access function (Block : Block_Type) return Natural;
   POP_COUNT_IMPL : constant Pop_Count_Implementation := (
      if    Block_Type'Size = unsigned'Size           then Pop_Count_Wrapper'Access
      elsif Block_Type'Size = unsigned_long'Size      then Pop_Count_Long_Wrapper'Access
      elsif Block_Type'Size = unsigned_long_long'Size then Pop_Count_Long_Long_Wrapper'Access
      else Pop_Count_Slow'Access);

   function Pop_Count (Block : Block_Type) return Natural is (POP_COUNT_IMPL.all (Block));

   --------------------
   -- Iterate_Blocks --
   --------------------
   procedure Iterate_Blocks (Bits : in out Bit_Set;
                             Block_Callback : access procedure (
                                Block       : in out Block_Type;
                                First_Index : Long_Long_Integer);
                             Remaining_Callback : access procedure (
                                Block          : in out Block_Type;
                                First_Index    : Long_Long_Integer;
                                Remaining_Bits : Bit_Count;
                                Remaining_Mask : Block_Type))
   is
      First_Index : Long_Long_Integer := Bits.First_Index;
   begin
      if Bits.Blocks = null then
         return;
      end if;

      for I in Bits.Blocks.all'Range loop
         Block_Callback.all (Bits.Blocks.all (I), First_Index);
         First_Index := First_Index + Block_Type'Size;
      end loop;

      if Bits.Remaining_Bits /= 0 then
         Remaining_Callback.all (Bits.Remaining_Block, First_Index, Bits.Remaining_Bits, Bits.Remaining_Mask);
      end if;
   end Iterate_Blocks;

   ------------------
   -- Iterate_Bits --
   ------------------
   --  Used to iterate through each individual bit and modify/inspect each one
   procedure Iterate_Bits (Bits : in out Bit_Set;

                           --  Run on each bit
                           Bit_Callback : access procedure (
                              --  the block to inspect/modify
                              Block : in out Block_Type;
                              --  The index of this bit relative to the entire array
                              Index : Long_Long_Integer;
                              --  The offset of this bit within the block
                              Offset : Bit_Offset))
   is
      procedure Iterate_Block_Bits_Callback (Block       : in out Block_Type;
                                             First_Index : Long_Long_Integer)
      is begin
         for I in Bit_Offset'Range loop
            Bit_Callback.all (Block, First_Index + Long_Long_Integer (I), I);
         end loop;
      end Iterate_Block_Bits_Callback;

      procedure Iterate_Block_Bits_Remaining_Callback (Block          : in out Block_Type;
                                                       First_Index    : Long_Long_Integer;
                                                       Remaining_Bits : Bit_Count;
                                                       Remaining_Mask : Block_Type)
      is
         pragma Unreferenced (Remaining_Mask);
      begin
         for I in 0 .. Remaining_Bits - 1 loop
            Bit_Callback.all (Block, First_Index + Long_Long_Integer (I), Bit_Offset (I));
         end loop;
      end Iterate_Block_Bits_Remaining_Callback;
   begin
      Iterate_Blocks (Bits, Iterate_Block_Bits_Callback'Access, Iterate_Block_Bits_Remaining_Callback'Access);
   end Iterate_Bits;

   -------------------
   -- Reduce_Blocks --
   -------------------
   procedure Reduce_Blocks (Result : in out Result_Type;
                            Bits : Bit_Set;
                            Reduce_Block_Callback : access procedure (
                               Result      : in out Result_Type;
                               Block       : Block_Type;
                               First_Index : Long_Long_Integer);
                            Reduce_Remaining_Callback : access procedure (
                               Result         : in out Result_Type;
                               Block          : Block_Type;
                               First_Index    : Long_Long_Integer;
                               Remaining_Bits : Bit_Count;
                               Remaining_Mask : Block_Type))
   is
      First_Index : Long_Long_Integer := Bits.First_Index;
   begin
      if Bits.Blocks = null then
         return;
      end if;

      for I in Bits.Blocks.all'Range loop
         Reduce_Block_Callback.all (Result, Bits.Blocks.all (I), First_Index);
         First_Index := First_Index + Block_Type'Size;
      end loop;

      if Bits.Remaining_Bits /= 0 then
         Reduce_Remaining_Callback.all (Result,
                                        Bits.Remaining_Block,
                                        First_Index,
                                        Bits.Remaining_Bits,
                                        Bits.Remaining_Mask);
      end if;
   end Reduce_Blocks;

   -----------------
   -- Reduce_Bits --
   -----------------
   procedure Reduce_Bits (Result : in out Result_Type;
                          Bits   : Bit_Set;
                          Reduce_Bit_Callback : access procedure (
                             Result : in out Result_Type;
                             Block  : Block_Type;
                             Index  : Long_Long_Integer;
                             Offset : Bit_Offset))
   is
      procedure Reduce_Block_Callback (Result      : in out Result_Type;
                                       Block       : Block_Type;
                                       First_Index : Long_Long_Integer)
      is begin
         for I in Bit_Offset'Range loop
            Reduce_Bit_Callback.all (Result, Block, First_Index + Long_Long_Integer (I), I);
         end loop;
      end Reduce_Block_Callback;

      procedure Reduce_Remaining_Callback (Result         : in out Result_Type;
                                           Block          : Block_Type;
                                           First_Index    : Long_Long_Integer;
                                           Remaining_Bits : Bit_Count;
                                           Remaining_Mask : Block_Type)
      is
         pragma Unreferenced (Remaining_Mask);
      begin
         for I in 0 .. Remaining_Bits - 1 loop
            Reduce_Bit_Callback.all (Result, Block, First_Index + Long_Long_Integer (I), Bit_Offset (I));
         end loop;
      end Reduce_Remaining_Callback;

      procedure Reduce_Blocks_To_Result is new Reduce_Blocks (Result_Type);
   begin
      Reduce_Blocks_To_Result (Result, Bits, Reduce_Block_Callback'Access, Reduce_Remaining_Callback'Access);
   end Reduce_Bits;

   ------------------
   -- Size_In_Bits --
   ------------------
   function Size_In_Bits (Bits : Bit_Set) return Long_Long_Integer is (Bits.Blocks.all'Size + Bit_Set'Size);

   --------------------
   -- Test_All_Equal --
   --------------------
   function Test_All_Equal (Bits : Bit_Set; Value : Block_Type) return Boolean is
      procedure Reduce_Blocks_To_Boolean is new Reduce_Blocks (Result_Type => Boolean);

      procedure Block_Callback (Result      : in out Boolean;
                                Block       : Block_Type;
                                First_Index : Long_Long_Integer)
      is
         pragma Unreferenced (First_Index);
      begin
         Result := Result and then Value = Block;
      end Block_Callback;

      procedure Remaining_Callback (Result         : in out Boolean;
                                    Block          : Block_Type;
                                    First_Index    : Long_Long_Integer;
                                    Remaining_Bits : Bit_Count;
                                    Remaining_Mask : Block_Type)
      is
         pragma Unreferenced (First_Index);
         pragma Unreferenced (Remaining_Bits);
      begin
         Result := Result and then (Value and Remaining_Mask) = (Block and Remaining_Mask);
      end Remaining_Callback;

      All_Equal : Boolean := True;
   begin
      if Bits.Blocks = null then
         return False;
      end if;

      Reduce_Blocks_To_Boolean (All_Equal,
                                Bits,
                                Block_Callback'Access,
                                Remaining_Callback'Access);
      return All_Equal;
   end Test_All_Equal;

   ------------------
   -- Test_All_Set --
   ------------------
   function Test_All_Set (Bits : Bit_Set) return Boolean is begin
      return Test_All_Equal (Bits, ALL_ONES);
   end Test_All_Set;

   --------------------
   -- Test_All_Unset --
   --------------------
   function Test_All_Unset (Bits : Bit_Set) return Boolean is begin
      return Test_All_Equal (Bits, ALL_ZEROS);
   end Test_All_Unset;

   ----------------------
   -- Count_Population --
   ----------------------
   function Count_Population (Bits : Bit_Set) return Long_Long_Integer is
      procedure Reduce_Blocks_To_Long_Long_Integer is new Reduce_Blocks (Result_Type => Long_Long_Integer);

      procedure Block_Callback (Result      : in out Long_Long_Integer;
                                Block       : Block_Type;
                                First_Index : Long_Long_Integer)
      is
         pragma Unreferenced (First_Index);
      begin
         Result := Result + Long_Long_Integer (Pop_Count (Block));
      end Block_Callback;

      procedure Remaining_Callback (Result         : in out Long_Long_Integer;
                                    Block          : Block_Type;
                                    First_Index    : Long_Long_Integer;
                                    Remaining_Bits : Bit_Count;
                                    Remaining_Mask : Block_Type)
      is
         pragma Unreferenced (First_Index);
         pragma Unreferenced (Remaining_Bits);
      begin
         Result := Result + Long_Long_Integer (Pop_Count (Block and Remaining_Mask));
      end Remaining_Callback;

      Population : Long_Long_Integer := 0;
   begin
      if Bits.Blocks = null then
         return 0;
      end if;

      Reduce_Blocks_To_Long_Long_Integer (Population,
                                          Bits,
                                          Block_Callback'Access,
                                          Remaining_Callback'Access);
      return Population;
   end Count_Population;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Bits        : in out Bit_Set;
                         Size        : Long_Long_Integer;
                         First_Index : Long_Long_Integer := 1)
   is
      Padded_Size : constant Long_Long_Integer := (Size / Block_Type'Size);
      First       : constant Long_Long_Integer := Long_Long_Integer'First;
      Last        : constant Long_Long_Integer := Long_Long_Integer'First + Padded_Size - 1;
   begin
      if Bits.Blocks /= null then
         Free (Bits.Blocks);
      end if;
      Bits.Blocks          := new Block_Array (First .. Last);
      Bits.First_Index     := First_Index;
      Bits.Remaining_Block := 0;
      Bits.Remaining_Bits  := Bit_Count (Size mod Block_Type'Size);
      Bits.Remaining_Mask  := Leading_Bits_Mask (Bits.Remaining_Bits);
   end Initialize;

   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (Bits : in out Bit_Set) is begin
      if Bits.Blocks /= null then
         Free (Bits.Blocks);
      end if;
      Bits.First_Index     := 1;
      Bits.Remaining_Block := 0;
      Bits.Remaining_Bits  := 0;
      Bits.Remaining_Mask  := 0;
   end Finalize;

begin
   LOW_ALTERNATING  := Generate_Alternating_Bit_Pattern;
   HIGH_ALTERNATING := (LOW_ALTERNATING * 2);
end Bit_Sets;