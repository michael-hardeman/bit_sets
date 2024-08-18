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

with AUnit.Assertions; use AUnit.Assertions;

package body Bit_Sets.Bit_Set_Tests is

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Initialize'Access,       "Initialize");
      Register_Routine (Test, Test_Size_In_Bits'Access,     "Size_In_Bits");
      Register_Routine (Test, Test_Test_All_Equal'Access,   "Test_All_Equal");
      Register_Routine (Test, Test_Test_All_Set'Access,     "Test_All_Set");
      Register_Routine (Test, Test_Test_All_Unset'Access,   "Test_All_Unset");
      Register_Routine (Test, Test_Count_Population'Access, "Count_Population");
      Register_Routine (Test, Test_Iterate_Blocks'Access,   "Iterate_Blocks");
      Register_Routine (Test, Test_Iterate_Bits'Access,     "Iterate_Bits");
      Register_Routine (Test, Test_Reduce_Blocks'Access,    "Reduce_Blocks");
      Register_Routine (Test, Test_Reduce_Bits'Access,      "Reduce_Bits");
   end Register_Tests;

   ------------------
   -- Test_Initialize --
   ------------------
   procedure Test_Initialize (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2, 1337);
      Assert (null /= Bits.Blocks,            "Blocks are null");
      Assert (2     = Bits.Blocks.all'Length, "2 blocks we not allocated");
      Assert (1337  = Bits.First_Index,       "First_Index was not respected and cached");
      Assert (0     = Bits.Remaining_Block,   "Remaining_Blocks should always be 0 initially");
      Assert (2     = Bits.Remaining_Bits,    "Remaining_Bits should be 1 given the initial size");
      Assert (2#11# = Bits.Remaining_Mask,    "Remaining_Mask should have the first two bits set");
   end Test_Initialize;

   -----------------------
   -- Test_Size_In_Bits --
   -----------------------
   procedure Test_Size_In_Bits (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2);
      Assert ((Block_Type'Size * 2 + Bits'Size) = Size_In_Bits (Bits), "Size not correctly computed");
   end Test_Size_In_Bits;

   -------------------------
   -- Test_Test_All_Equal --
   -------------------------
   procedure Test_Test_All_Equal (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2);
      Bits.Blocks.all (Bits.Blocks.all'First) := 0;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 0;
      Bits.Remaining_Block := 0;
      Assert (False = Test_All_Equal (Bits, 42069), "Blocks should not be 42069");
      Bits.Blocks.all (Bits.Blocks.all'First) := 42069;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 42069;
      Bits.Remaining_Block := 42069;
      Assert (True = Test_All_Equal (Bits, 42069), "Blocks should be 42069");
   end Test_Test_All_Equal;

   -----------------------
   -- Test_Test_All_Set --
   -----------------------
   procedure Test_Test_All_Set (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2);
      Bits.Blocks.all (Bits.Blocks.all'First) := LOW_ALTERNATING;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := LOW_ALTERNATING;
      Bits.Remaining_Block := LOW_ALTERNATING;
      Assert (False = Test_All_Set (Bits), "Bits should not be all 1");
      Bits.Blocks.all (Bits.Blocks.all'First) := Block_Type'Last;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := Block_Type'Last;
      Bits.Remaining_Block := Block_Type'Last;
      Assert (True = Test_All_Set (Bits), "Bits should all be 1");
   end Test_Test_All_Set;

   -------------------------
   -- Test_Test_All_Unset --
   -------------------------
   procedure Test_Test_All_Unset (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2);
      Bits.Blocks.all (Bits.Blocks.all'First) := LOW_ALTERNATING;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := LOW_ALTERNATING;
      Bits.Remaining_Block := LOW_ALTERNATING;
      Assert (False = Test_All_Unset (Bits), "Bits should not be all 0");
      Bits.Blocks.all (Bits.Blocks.all'First) := 0;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 0;
      Bits.Remaining_Block := 0;
      Assert (True = Test_All_Unset (Bits), "Bits should all be 0");
   end Test_Test_All_Unset;

   ---------------------------
   -- Test_Count_Population --
   ---------------------------
   procedure Test_Count_Population (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2);
      Bits.Blocks.all (Bits.Blocks.all'First) := 0;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 0;
      Bits.Remaining_Block := 0;
      Assert (0 = Count_Population (Bits), "Bits should all be 0");
      Bits.Blocks.all (Bits.Blocks.all'First) := LOW_ALTERNATING;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := LOW_ALTERNATING;
      Bits.Remaining_Block := LOW_ALTERNATING;
      Assert ((Block_Type'Size + 1) = Count_Population (Bits), "Half the bits should be 1");
   end Test_Count_Population;

   -------------------------
   -- Test_Iterate_Blocks --
   -------------------------
   procedure Test_Iterate_Blocks (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      Block_Callback_Called     : Natural := 0;
      Remaining_Callback_Called : Natural := 0;

      procedure Block_Callback (Block       : in out Block_Type;
                                First_Index : Long_Long_Integer)
      is begin
         Assert (2 > Block_Callback_Called, "There should only be 2 calls to this callback. 1 for each block");
         if 0 = Block_Callback_Called then
            Assert (1 = Block,       "First Block should be 1");
            Assert (0 = First_Index, "First Index should be 0");
         elsif 1 = Block_Callback_Called then
            Assert (2 = Block,                     "First Block should be 1");
            Assert (Block_Type'Size = First_Index, "First should be incremented by Block_Type'Size");
         end if;
         Block := 42069;
         Block_Callback_Called := Block_Callback_Called + 1;
      end Block_Callback;

      procedure Remaining_Callback (Block          : in out Block_Type;
                                    First_Index    : Long_Long_Integer;
                                    Remaining_Bits : Bit_Count;
                                    Remaining_Mask : Block_Type)
      is begin
         Assert (0 = Remaining_Callback_Called,     "There should only ever by 1 call to this callback");
         Assert (3 = Block,                         "First Block should be 1");
         Assert (Block_Type'Size * 2 = First_Index, "First Index should be Block_Type'Size * 2");
         Assert (2 = Remaining_Bits,                "There should be 2 remaining bits");
         Assert (2#11# = Remaining_Mask,            "The mask should indicate 2 remaining bits");
         Block := 42069;
         Remaining_Callback_Called := Remaining_Callback_Called + 1;
      end Remaining_Callback;

      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2, 0);
      Bits.Blocks.all (Bits.Blocks.all'First) := 1;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 2;
      Bits.Remaining_Block := 3;
      Iterate_Blocks (Bits, Block_Callback'Access, Remaining_Callback'Access);
      Assert (2 = Block_Callback_Called, "Block Callback should have been called twice");
      Assert (1 = Remaining_Callback_Called, "Remaining Callback should have been called once");
      Assert (42069 = Bits.Blocks.all (Bits.Blocks.all'First), "First block should be 42069");
      Assert (42069 = Bits.Blocks.all (Bits.Blocks.all'Last), "Second block should be 42069");
      Assert (42069 = Bits.Remaining_Block, "Remaining block should be 42069");
   end Test_Iterate_Blocks;

   -----------------------
   -- Test_Iterate_Bits --
   -----------------------
   procedure Test_Iterate_Bits (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      procedure Bit_Callback (Block  : in out Block_Type;
                              Index  : Long_Long_Integer;
                              Offset : Bit_Offset)
      is begin
         Assert (2 * Block_Type'Size + 3 > Index, "There should never be more calls than 2 * Block_Type'Size + 3");
         if Offset mod 2 = 0 then
            Assert (0 = (Block and Bit_Mask (Offset)), "Odd bits should be off");
            Set_Bit (Block, Offset);
         else
            Assert (0 /= (Block and Bit_Mask (Offset)), "Even bits should be on");
            Unset_Bit (Block, Offset);
         end if;
      end Bit_Callback;

      Bits : Bit_Set;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2, 0);
      Bits.Blocks.all (Bits.Blocks.all'First) := HIGH_ALTERNATING;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := HIGH_ALTERNATING;
      Bits.Remaining_Block := HIGH_ALTERNATING;
      Iterate_Bits (Bits, Bit_Callback'Access);
      Assert (LOW_ALTERNATING = Bits.Blocks.all (Bits.Blocks.all'First), "First block should now be 0101...");
      Assert (LOW_ALTERNATING = Bits.Blocks.all (Bits.Blocks.all'Last), "Second block should now be 0101...");
      Assert (2#01# = (Bits.Remaining_Block and Bits.Remaining_Mask), "Remaining block should now be #01#");
   end Test_Iterate_Bits;

   ------------------------
   -- Test_Reduce_Blocks --
   ------------------------
   procedure Test_Reduce_Blocks (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      Block_Callback_Called : Natural := 0;
      Remaining_Callback_Called : Natural := 0;
      procedure Reduce_Blocks_To_Boolean is new Reduce_Blocks (Result_Type => Boolean);

      procedure Test_Are_42069_Block_Callback (Result      : in out Boolean;
                                               Block       : Block_Type;
                                               First_Index : Long_Long_Integer)
      is begin
         Assert (2 > Block_Callback_Called, "There should only be 2 calls to this callback. 1 for each block");
         if 0 = Block_Callback_Called then
            Assert (0 = First_Index, "First_Index should be 0");
         else
            Assert (Block_Type'Size = First_Index, "First_Index should be incremented by Block_Type'Size");
         end if;
         Result := Result and then 42069 = Block;
         Block_Callback_Called := Block_Callback_Called + 1;
      end Test_Are_42069_Block_Callback;

      procedure Test_Are_42069_Remaining_Callback (Result         : in out Boolean;
                                                   Block          : Block_Type;
                                                   First_Index    : Long_Long_Integer;
                                                   Remaining_Bits : Bit_Count;
                                                   Remaining_Mask : Block_Type)
      is begin
         Assert (0 = Remaining_Callback_Called, "There should only ever by 1 call to this callback");
         Assert (Block_Type'Size * 2 = First_Index, "First_Index should be 2 * Block_Type'Size");
         Assert (2 = Remaining_Bits, "Remaining bits should be 2");
         Assert (2#11# = Remaining_Mask, "Remaining Mask should be 2#11#");
         Result := Result and then ((42069 and Remaining_Mask) = (Block and Remaining_Mask));
         Remaining_Callback_Called := Remaining_Callback_Called + 1;
      end Test_Are_42069_Remaining_Callback;

      Bits : Bit_Set;
      Result : Boolean := True;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2, 0);
      Bits.Blocks.all (Bits.Blocks.all'First) := 0;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 0;
      Bits.Remaining_Block := 0;
      Reduce_Blocks_To_Boolean (Result,
                                Bits,
                                Test_Are_42069_Block_Callback'Access,
                                Test_Are_42069_Remaining_Callback'Access);
      Assert (False = Result, "Result should be False");
      Assert (2 = Block_Callback_Called, "Block Callback should have been called twice");
      Assert (1 = Remaining_Callback_Called, "Remaining Callback should have been called once");
      Bits.Blocks.all (Bits.Blocks.all'First) := 42069;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := 42069;
      Bits.Remaining_Block := 42069;
      Result := True;
      Block_Callback_Called := 0;
      Remaining_Callback_Called := 0;
      Reduce_Blocks_To_Boolean (Result,
                                Bits,
                                Test_Are_42069_Block_Callback'Access,
                                Test_Are_42069_Remaining_Callback'Access);
      Assert (True = Result, "Result should be True");
      Assert (2 = Block_Callback_Called, "Block Callback should have been called twice");
      Assert (1 = Remaining_Callback_Called, "Remaining Callback should have been called once");
   end Test_Reduce_Blocks;

   ----------------------
   -- Test_Reduce_Bits --
   ----------------------
   procedure Test_Reduce_Bits (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      procedure Reduce_Bits_To_Boolean is new Reduce_Bits (Result_Type => Boolean);

      procedure Test_Is_Alternating_Bit_Callback (Result : in out Boolean;
                                                  Block  : Block_Type;
                                                  Index  : Long_Long_Integer;
                                                  Offset : Bit_Offset)
      is begin
         Assert (2 * Block_Type'Size + 3 > Index, "There should never be more calls than 2 * Block_Type'Size + 3");

         if Offset mod 2 = 0 then
            Result := Result and then Test_Bit (Block, Offset);
         else
            Result := Result and then not Test_Bit (Block, Offset);
         end if;
      end Test_Is_Alternating_Bit_Callback;

      Bits : Bit_Set;
      Result : Boolean := True;
   begin
      Initialize (Bits, (Block_Type'Size * 2) + 2, 0);
      Bits.Blocks.all (Bits.Blocks.all'First) := LOW_ALTERNATING;
      Bits.Blocks.all (Bits.Blocks.all'Last)  := LOW_ALTERNATING;
      Bits.Remaining_Block := LOW_ALTERNATING;
      Reduce_Bits_To_Boolean (Result,
                              Bits,
                              Test_Is_Alternating_Bit_Callback'Access);
      Assert (True = Result, "Result should be True");
   end Test_Reduce_Bits;

end Bit_Sets.Bit_Set_Tests;