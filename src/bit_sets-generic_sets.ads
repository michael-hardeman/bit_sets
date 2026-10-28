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

private with Bit_Sets.Blocks;

------------------
-- Generic_Sets --
------------------
--  A fixed size set of the indexes First .. Last, stored one bit per index on
--  the heap so very large sets (billions of bits) do not overflow the stack.
--
--  Bit_Set is limited: it cannot be copied by accident. Use Copy or Assign
--  when a copy is really wanted.
--
--     package Flags is new Bit_Sets.Generic_Sets (Natural);
--     use Flags;
--
--     S : Bit_Set := Create (0, 99);
--     S.Include (3);
--     S.Include (10, 20);
--     for I of S loop  --  Visits only the indexes in the set, in order
--        ...
--     end loop;
generic
   type Index_Type is range <>;
package Bit_Sets.Generic_Sets is

   type Bit_Set (<>) is new Ada.Finalization.Limited_Controlled with private
     with Iterable => (First       => First,
                       Next        => Next,
                       Has_Element => Has_Element,
                       Element     => Element);

   ------------
   -- Cursor --
   ------------
   --  Marks one index that is in the set, or No_Element
   type Cursor is private;
   No_Element : constant Cursor;

   ------------
   -- Create --
   ------------
   --  A set of the indexes First .. Last, all excluded (or all included when
   --  Value is True). Last < First makes an empty set.
   function Create (First, Last : Index_Type; Value : Boolean := False) return Bit_Set;

   ----------
   -- Copy --
   ----------
   function Copy (Source : Bit_Set) return Bit_Set;

   ------------
   -- Assign --
   ------------
   --  Copies the contents of Source into Target
   procedure Assign (Target : in out Bit_Set; Source : Bit_Set)
     with Pre => Same_Bounds (Target, Source);

   -------------
   -- Queries --
   -------------
   function First_Index (S : Bit_Set) return Index_Type;
   function Last_Index  (S : Bit_Set) return Index_Type;
   function Length      (S : Bit_Set) return Count_Type;

   function In_Range (S : Bit_Set; Index : Index_Type) return Boolean
     is (Index in First_Index (S) .. Last_Index (S));

   function Same_Bounds (Left, Right : Bit_Set) return Boolean
     is (First_Index (Left) = First_Index (Right) and then Last_Index (Left) = Last_Index (Right));

   --  The number of bits of heap storage used, Length rounded up to whole blocks
   function Storage_Size (S : Bit_Set) return Count_Type;

   -------------------
   -- Single Indexes --
   -------------------
   function Contains (S : Bit_Set; Index : Index_Type) return Boolean
     with Pre => In_Range (S, Index);

   procedure Include (S : in out Bit_Set; Index : Index_Type)
     with Pre => In_Range (S, Index), Post => Contains (S, Index);

   procedure Exclude (S : in out Bit_Set; Index : Index_Type)
     with Pre => In_Range (S, Index), Post => not Contains (S, Index);

   procedure Flip (S : in out Bit_Set; Index : Index_Type)
     with Pre => In_Range (S, Index);

   ------------
   -- Ranges --
   ------------
   --  Include or exclude every index in From .. To. Whole blocks are written at
   --  once. To < From does nothing.
   procedure Include (S : in out Bit_Set; From, To : Index_Type)
     with Pre => To < From or else (In_Range (S, From) and then In_Range (S, To));

   procedure Exclude (S : in out Bit_Set; From, To : Index_Type)
     with Pre => To < From or else (In_Range (S, From) and then In_Range (S, To));

   -----------------
   -- Whole Sets --
   -----------------
   procedure Clear (S : in out Bit_Set) with Post => Is_Empty (S);
   procedure Fill  (S : in out Bit_Set) with Post => Is_Full (S);
   procedure Flip  (S : in out Bit_Set);

   function Count    (S : Bit_Set) return Count_Type;
   function Is_Empty (S : Bit_Set) return Boolean;
   function Is_Full  (S : Bit_Set) return Boolean;

   ----------------------------
   -- In Place Set Operations --
   ----------------------------
   --  These change Target and allocate nothing, prefer them for large sets
   procedure Union                (Target : in out Bit_Set; Source : Bit_Set)
     with Pre => Same_Bounds (Target, Source);
   procedure Intersection         (Target : in out Bit_Set; Source : Bit_Set)
     with Pre => Same_Bounds (Target, Source);
   procedure Difference           (Target : in out Bit_Set; Source : Bit_Set)
     with Pre => Same_Bounds (Target, Source);
   procedure Symmetric_Difference (Target : in out Bit_Set; Source : Bit_Set)
     with Pre => Same_Bounds (Target, Source);

   -------------------
   -- Set Operators --
   -------------------
   --  Each returns a newly allocated set
   function "or"  (Left, Right : Bit_Set) return Bit_Set with Pre => Same_Bounds (Left, Right);
   function "and" (Left, Right : Bit_Set) return Bit_Set with Pre => Same_Bounds (Left, Right);
   function "xor" (Left, Right : Bit_Set) return Bit_Set with Pre => Same_Bounds (Left, Right);
   function "-"   (Left, Right : Bit_Set) return Bit_Set with Pre => Same_Bounds (Left, Right);
   function "not" (S : Bit_Set) return Bit_Set;

   -----------------
   -- Comparisons --
   -----------------
   --  Sets with different bounds are never equal
   function "=" (Left, Right : Bit_Set) return Boolean;

   function Is_Subset (Subset, Of_Set : Bit_Set) return Boolean
     with Pre => Same_Bounds (Subset, Of_Set);
   function "<=" (Left, Right : Bit_Set) return Boolean renames Is_Subset;

   --  True when the sets have at least one index in common
   function Overlaps (Left, Right : Bit_Set) return Boolean
     with Pre => Same_Bounds (Left, Right);

   ---------------
   -- Iteration --
   ---------------
   --  Iteration skips whole empty blocks, so sparse sets iterate quickly
   function First       (S : Bit_Set) return Cursor;
   function Next        (S : Bit_Set; Position : Cursor) return Cursor;
   function Has_Element (S : Bit_Set; Position : Cursor) return Boolean;
   function Element     (S : Bit_Set; Position : Cursor) return Index_Type
     with Pre => Has_Element (S, Position);

   --  The first index at or after From that is in the set
   function Find (S : Bit_Set; From : Index_Type) return Cursor
     with Pre => In_Range (S, From);

private

   use Bit_Sets.Blocks;

   type Block_Array is array (Count_Type range <>) of Block_Type;
   type Block_Array_Access is access Block_Array;

   --  Invariant: the unused bits past Last in the final block are always 0.
   --  Every whole block operation relies on it so the final block never needs
   --  special handling, only Fill and Flip must re-clear it.
   type Bit_Set is new Ada.Finalization.Limited_Controlled with record
      First  : Index_Type         := Index_Type'First;
      Last   : Index_Type         := Index_Type'First;
      Length : Count_Type         := 0;
      Blocks : Block_Array_Access := null;
   end record;

   overriding procedure Finalize (S : in out Bit_Set);

   type Cursor is record
      Valid : Boolean    := False;
      Index : Index_Type := Index_Type'First;
   end record;

   No_Element : constant Cursor := (Valid => False, Index => Index_Type'First);

end Bit_Sets.Generic_Sets;
