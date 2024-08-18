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

package body Bit_Sets.Block_Tests is

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_To_Offset'Access,         "To_Offset");
      Register_Routine (Test, Test_To_Index'Access,          "To_Index");
      Register_Routine (Test, Test_Bit_Mask'Access,          "Bit_Mask");
      Register_Routine (Test, Test_Leading_Bits_Mask'Access, "Leading_Bits_Mask");
      Register_Routine (Test, Test_Test_Bit'Access,          "Test_Bit");
      Register_Routine (Test, Test_Set_Bit'Access,           "Set_Bit");
      Register_Routine (Test, Test_Unset_Bit'Access,         "Unset_Bit");
      Register_Routine (Test, Test_Pop_Count'Access,         "Pop_Count");
   end Register_Tests;

   --------------------
   -- Test_To_Offset --
   --------------------
   procedure Test_To_Offset (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (Bit_Offset'First = To_Offset (Bit_Index'First), "Incorrect index for Bit_Index'First");
      Assert (Bit_Offset'Last  = To_Offset (Bit_Index'Last),  "Incorrect index for Bit_Index'Last");
   end Test_To_Offset;

   -------------------
   -- Test_To_Index --
   -------------------
   procedure Test_To_Index (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (Bit_Index'First = To_Index (Bit_Offset'First), "Incorrect index for Bit_Offset'First");
      Assert (Bit_Index'Last  = To_Index (Bit_Offset'Last),  "Incorrect index for Bit_Offset'Last");
   end Test_To_Index;

   -------------------
   -- Test_Bit_Mask --
   -------------------
   procedure Test_Bit_Mask (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (2 ** Natural (Bit_Offset'First) = Bit_Mask (Bit_Offset'First), "Incorrect result for Offset'First");
      Assert (2 ** Natural (Bit_Offset'Last)  = Bit_Mask (Bit_Offset'Last),  "Incorrect result for Offset'Last");
      Assert (2 ** Natural (Bit_Offset'First) = Bit_Mask (Bit_Index'First),  "Incorrect result for Index'First");
      Assert (2 ** Natural (Bit_Offset'Last)  = Bit_Mask (Bit_Index'Last),   "Incorrect result for Index'Last");
   end Test_Bit_Mask;

   ----------------------------
   -- Test_Leading_Bits_Mask --
   ----------------------------
   procedure Test_Leading_Bits_Mask (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (2#0000# = Leading_Bits_Mask (0), "Incorrect result for 0");
      Assert (2#0001# = Leading_Bits_Mask (1), "Incorrect result for 1");
      Assert (2#0011# = Leading_Bits_Mask (2), "Incorrect result for 2");
      Assert (2#0111# = Leading_Bits_Mask (3), "Incorrect result for 3");
      Assert (2#1111# = Leading_Bits_Mask (4), "Incorrect result for 4");
   end Test_Leading_Bits_Mask;

   -------------------
   -- Test_Test_Bit --
   -------------------
   procedure Test_Test_Bit (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (False = Test_Bit (2#0000#, Bit_Offset (0)), "Incorrect bit read 0");
      Assert (True  = Test_Bit (2#0001#, Bit_Offset (0)), "Incorrect bit read 1");
      Assert (False = Test_Bit (2#0001#, Bit_Offset (1)), "Incorrect bit read 2");
      Assert (True  = Test_Bit (2#0010#, Bit_Offset (1)), "Incorrect bit read 3");
      Assert (False = Test_Bit (2#0010#, Bit_Offset (2)), "Incorrect bit read 4");
      Assert (True  = Test_Bit (2#0100#, Bit_Offset (2)), "Incorrect bit read 5");
      Assert (False = Test_Bit (2#0100#, Bit_Offset (3)), "Incorrect bit read 6");
      Assert (True  = Test_Bit (2#1000#, Bit_Offset (3)), "Incorrect bit read 7");
   end Test_Test_Bit;

   ------------------
   -- Test_Set_Bit --
   ------------------
   procedure Test_Set_Bit (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Input : Block_Type := 0;
   begin
      Set_Bit (Input, Bit_Offset (0));
      Assert (2#0001# = Input, "Bit not set 0");

      Set_Bit (Input, Bit_Offset (1));
      Assert (2#0011# = Input, "Bit not set 1");

      Set_Bit (Input, Bit_Offset (2));
      Assert (2#0111# = Input, "Bit not set 2");

      Set_Bit (Input, Bit_Offset (3));
      Assert (2#1111# = Input, "Bit not set 3");
   end Test_Set_Bit;

   --------------------
   -- Test_Unset_Bit --
   --------------------
   procedure Test_Unset_Bit (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Input : Block_Type := 2#1111#;
   begin
      Unset_Bit (Input, Bit_Offset (0));
      Assert (2#1110# = Input, "Bit not unset 0");

      Unset_Bit (Input, Bit_Offset (1));
      Assert (2#1100# = Input, "Bit not set 1");

      Unset_Bit (Input, Bit_Offset (2));
      Assert (2#1000# = Input, "Bit not set 2");

      Unset_Bit (Input, Bit_Offset (3));
      Assert (2#0000# = Input, "Bit not set 3");
   end Test_Unset_Bit;

   --------------------
   -- Test_Pop_Count --
   --------------------
   procedure Test_Pop_Count (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (0 = Pop_Count (2#0000#), "Incorrect pop count of 0");
      Assert (1 = Pop_Count (2#1000#), "Incorrect pop count of 1");
      Assert (2 = Pop_Count (2#0110#), "Incorrect pop count of 2");
      Assert (3 = Pop_Count (2#1011#), "Incorrect pop count of 3");
   end Test_Pop_Count;

end Bit_Sets.Block_Tests;