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

with Ada.Unchecked_Deallocation;

package body Bit_Sets.Generic_Sets is

   procedure Free is new Ada.Unchecked_Deallocation (Block_Array, Block_Array_Access);

   -------------
   -- Helpers --
   -------------

   --  Number of blocks needed to hold Length bits
   function Block_Count (Length : Count_Type) return Count_Type
     is ((Length + Block_Size - 1) / Block_Size);

   --  Position of a bit counted from the first bit of the set. It is unsigned so
   --  "/" and "mod" by Block_Size compile to a shift and a mask. With a signed type
   --  the compiler must also handle negative values, and when range checks are
   --  suppressed nothing tells it the value is never negative.
   type Bit_Position is mod 2 ** 64;

   function Offset_Of (S : Bit_Set; Index : Index_Type) return Bit_Position
     is (Bit_Position (Long_Long_Integer (Index) - Long_Long_Integer (S.First)));

   function Index_At (S : Bit_Set; Offset : Count_Type) return Index_Type
     is (Index_Type (Long_Long_Integer (S.First) + Long_Long_Integer (Offset)));

   function Block_Of (Offset : Bit_Position) return Count_Type is (Count_Type (Offset / Block_Size));
   function Bit_Of   (Offset : Bit_Position) return Bit_Offset is (Bit_Offset (Offset mod Block_Size));

   --  Number of bits used in the final block, 0 when the final block is full
   function Tail_Bits (S : Bit_Set) return Bit_Count is (Bit_Count (S.Length mod Block_Size));

   --  The value block B has when every index it holds is in the set
   function Full_Block (S : Bit_Set; B : Count_Type) return Block_Type
     is (if B = S.Blocks'Last and then Tail_Bits (S) /= 0 then Low_Mask (Tail_Bits (S)) else ALL_ONES);

   ----------------
   -- Clear_Tail --
   ----------------
   --  Restores the invariant that the unused bits of the final block are 0
   procedure Clear_Tail (S : in out Bit_Set) is
   begin
      if Tail_Bits (S) /= 0 then
         S.Blocks (S.Blocks'Last) := S.Blocks (S.Blocks'Last) and Low_Mask (Tail_Bits (S));
      end if;
   end Clear_Tail;

   ------------
   -- Create --
   ------------
   function Create (First, Last : Index_Type; Value : Boolean := False) return Bit_Set is
      Length : constant Count_Type :=
        (if Last < First then 0 else Count_Type (Long_Long_Integer (Last) - Long_Long_Integer (First) + 1));
   begin
      return S : Bit_Set do
         S.First  := First;
         S.Last   := Last;
         S.Length := Length;
         S.Blocks := new Block_Array (0 .. Block_Count (Length) - 1);
         if Value then
            Fill (S);
         else
            Clear (S);
         end if;
      end return;
   end Create;

   ----------
   -- Copy --
   ----------
   function Copy (Source : Bit_Set) return Bit_Set is
   begin
      return S : Bit_Set do
         S.First       := Source.First;
         S.Last        := Source.Last;
         S.Length      := Source.Length;
         S.Blocks      := new Block_Array (Source.Blocks'Range);
         S.Blocks.all  := Source.Blocks.all;
      end return;
   end Copy;

   ------------
   -- Assign --
   ------------
   procedure Assign (Target : in out Bit_Set; Source : Bit_Set) is
   begin
      Target.Blocks.all := Source.Blocks.all;
   end Assign;

   -------------
   -- Queries --
   -------------
   function First_Index (S : Bit_Set) return Index_Type is (S.First);
   function Last_Index  (S : Bit_Set) return Index_Type is (S.Last);
   function Length      (S : Bit_Set) return Count_Type is (S.Length);

   function Storage_Size (S : Bit_Set) return Count_Type is (S.Blocks'Length * Block_Size);

   --------------
   -- Contains --
   --------------
   function Contains (S : Bit_Set; Index : Index_Type) return Boolean is
      Offset : constant Bit_Position := Offset_Of (S, Index);
   begin
      return (S.Blocks (Block_Of (Offset)) and Bit_Mask (Bit_Of (Offset))) /= ALL_ZEROS;
   end Contains;

   -------------
   -- Include --
   -------------
   procedure Include (S : in out Bit_Set; Index : Index_Type) is
      Offset : constant Bit_Position := Offset_Of (S, Index);
      Block  : Block_Type renames S.Blocks (Block_Of (Offset));
   begin
      Block := Block or Bit_Mask (Bit_Of (Offset));
   end Include;

   -------------
   -- Exclude --
   -------------
   procedure Exclude (S : in out Bit_Set; Index : Index_Type) is
      Offset : constant Bit_Position := Offset_Of (S, Index);
      Block  : Block_Type renames S.Blocks (Block_Of (Offset));
   begin
      Block := Block and not Bit_Mask (Bit_Of (Offset));
   end Exclude;

   ----------
   -- Flip --
   ----------
   procedure Flip (S : in out Bit_Set; Index : Index_Type) is
      Offset : constant Bit_Position := Offset_Of (S, Index);
      Block  : Block_Type renames S.Blocks (Block_Of (Offset));
   begin
      Block := Block xor Bit_Mask (Bit_Of (Offset));
   end Flip;

   -----------------
   -- Apply_Range --
   -----------------
   --  Calls Apply with a mask of the bits From .. To for each block they cover
   generic
      with procedure Apply (Block : in out Block_Type; Mask : Block_Type);
   procedure Apply_Range (S : in out Bit_Set; From, To : Index_Type);

   procedure Apply_Range (S : in out Bit_Set; From, To : Index_Type) is
   begin
      if To < From then
         return;
      end if;

      declare
         First_Offset : constant Bit_Position := Offset_Of (S, From);
         Last_Offset  : constant Bit_Position := Offset_Of (S, To);
         First_Block  : constant Count_Type   := Block_Of (First_Offset);
         Last_Block   : constant Count_Type   := Block_Of (Last_Offset);
         First_Mask   : constant Block_Type   := High_Mask (Bit_Of (First_Offset));
         Last_Mask    : constant Block_Type   := Low_Mask (Bit_Count (Bit_Of (Last_Offset)) + 1);
      begin
         if First_Block = Last_Block then
            Apply (S.Blocks (First_Block), First_Mask and Last_Mask);
         else
            Apply (S.Blocks (First_Block), First_Mask);
            for B in First_Block + 1 .. Last_Block - 1 loop
               Apply (S.Blocks (B), ALL_ONES);
            end loop;
            Apply (S.Blocks (Last_Block), Last_Mask);
         end if;
      end;
   end Apply_Range;

   procedure Set_Bits (Block : in out Block_Type; Mask : Block_Type) with Inline;
   procedure Set_Bits (Block : in out Block_Type; Mask : Block_Type) is
   begin
      Block := Block or Mask;
   end Set_Bits;

   procedure Clear_Bits (Block : in out Block_Type; Mask : Block_Type) with Inline;
   procedure Clear_Bits (Block : in out Block_Type; Mask : Block_Type) is
   begin
      Block := Block and not Mask;
   end Clear_Bits;

   procedure Include_Range is new Apply_Range (Set_Bits);
   procedure Exclude_Range is new Apply_Range (Clear_Bits);

   procedure Include (S : in out Bit_Set; From, To : Index_Type) renames Include_Range;
   procedure Exclude (S : in out Bit_Set; From, To : Index_Type) renames Exclude_Range;

   -----------
   -- Clear --
   -----------
   procedure Clear (S : in out Bit_Set) is
   begin
      for Block of S.Blocks.all loop
         Block := ALL_ZEROS;
      end loop;
   end Clear;

   ----------
   -- Fill --
   ----------
   procedure Fill (S : in out Bit_Set) is
   begin
      for Block of S.Blocks.all loop
         Block := ALL_ONES;
      end loop;
      Clear_Tail (S);
   end Fill;

   ----------
   -- Flip --
   ----------
   procedure Flip (S : in out Bit_Set) is
   begin
      for Block of S.Blocks.all loop
         Block := not Block;
      end loop;
      Clear_Tail (S);
   end Flip;

   -----------
   -- Count --
   -----------
   function Count (S : Bit_Set) return Count_Type is
      Result : Count_Type := 0;
   begin
      for Block of S.Blocks.all loop
         Result := Result + Count_Type (Pop_Count (Block));
      end loop;
      return Result;
   end Count;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (S : Bit_Set) return Boolean is
     (for all Block of S.Blocks.all => Block = ALL_ZEROS);

   -------------
   -- Is_Full --
   -------------
   function Is_Full (S : Bit_Set) return Boolean is
     (for all B in S.Blocks'Range => S.Blocks (B) = Full_Block (S, B));

   ------------------
   -- Combine_Into --
   ------------------
   --  Target := Combine (Target, Source) one block at a time
   generic
      with function Combine (Left, Right : Block_Type) return Block_Type;
   procedure Combine_Into (Target : in out Bit_Set; Source : Bit_Set);

   procedure Combine_Into (Target : in out Bit_Set; Source : Bit_Set) is
   begin
      for B in Target.Blocks'Range loop
         Target.Blocks (B) := Combine (Target.Blocks (B), Source.Blocks (B));
      end loop;
   end Combine_Into;

   function And_Not (Left, Right : Block_Type) return Block_Type is (Left and not Right) with Inline;

   procedure Union_Into                is new Combine_Into ("or");
   procedure Intersection_Into         is new Combine_Into ("and");
   procedure Difference_Into           is new Combine_Into (And_Not);
   procedure Symmetric_Difference_Into is new Combine_Into ("xor");

   procedure Union (Target : in out Bit_Set; Source : Bit_Set) renames Union_Into;
   procedure Intersection (Target : in out Bit_Set; Source : Bit_Set) renames Intersection_Into;
   procedure Difference (Target : in out Bit_Set; Source : Bit_Set) renames Difference_Into;
   procedure Symmetric_Difference (Target : in out Bit_Set; Source : Bit_Set) renames Symmetric_Difference_Into;

   -------------------
   -- Set Operators --
   -------------------
   function "or" (Left, Right : Bit_Set) return Bit_Set is
   begin
      return S : Bit_Set := Copy (Left) do
         Union (S, Right);
      end return;
   end "or";

   function "and" (Left, Right : Bit_Set) return Bit_Set is
   begin
      return S : Bit_Set := Copy (Left) do
         Intersection (S, Right);
      end return;
   end "and";

   function "xor" (Left, Right : Bit_Set) return Bit_Set is
   begin
      return S : Bit_Set := Copy (Left) do
         Symmetric_Difference (S, Right);
      end return;
   end "xor";

   function "-" (Left, Right : Bit_Set) return Bit_Set is
   begin
      return S : Bit_Set := Copy (Left) do
         Difference (S, Right);
      end return;
   end "-";

   function "not" (S : Bit_Set) return Bit_Set is
   begin
      return Result : Bit_Set := Copy (S) do
         Flip (Result);
      end return;
   end "not";

   -----------------
   -- Comparisons --
   -----------------
   function "=" (Left, Right : Bit_Set) return Boolean is
     (Same_Bounds (Left, Right) and then Left.Blocks.all = Right.Blocks.all);

   function Is_Subset (Subset, Of_Set : Bit_Set) return Boolean is
     (for all B in Subset.Blocks'Range => (Subset.Blocks (B) and not Of_Set.Blocks (B)) = ALL_ZEROS);

   function Overlaps (Left, Right : Bit_Set) return Boolean is
     (for some B in Left.Blocks'Range => (Left.Blocks (B) and Right.Blocks (B)) /= ALL_ZEROS);

   ----------
   -- Find --
   ----------
   function Find (S : Bit_Set; From : Index_Type) return Cursor is
      Offset : constant Bit_Position := Offset_Of (S, From);
      B      : Count_Type            := Block_Of (Offset);
      Block  : Block_Type            := S.Blocks (B) and High_Mask (Bit_Of (Offset));
   begin
      loop
         if Block /= ALL_ZEROS then
            return (Valid => True,
                    Index => Index_At (S, B * Block_Size + Count_Type (Count_Trailing_Zeros (Block))));
         end if;
         exit when B = S.Blocks'Last;
         B     := B + 1;
         Block := S.Blocks (B);
      end loop;
      return No_Element;
   end Find;

   ---------------
   -- Iteration --
   ---------------
   function First (S : Bit_Set) return Cursor is
     (if S.Length = 0 then No_Element else Find (S, S.First));

   function Next (S : Bit_Set; Position : Cursor) return Cursor is
     (if not Position.Valid or else Position.Index = S.Last then No_Element
      else Find (S, Index_Type'Succ (Position.Index)));

   function Has_Element (S : Bit_Set; Position : Cursor) return Boolean is
      pragma Unreferenced (S);
   begin
      return Position.Valid;
   end Has_Element;

   function Element (S : Bit_Set; Position : Cursor) return Index_Type is
      pragma Unreferenced (S);
   begin
      return Position.Index;
   end Element;

   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (S : in out Bit_Set) is
   begin
      Free (S.Blocks);
   end Finalize;

end Bit_Sets.Generic_Sets;
