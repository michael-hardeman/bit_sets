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

with Bit_Sets.Blocks; use Bit_Sets.Blocks;

package body Bit_Sets.Block_Tests is

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Bit_Mask'Access,             "Bit_Mask");
      Register_Routine (Test, Test_Low_Mask'Access,             "Low_Mask");
      Register_Routine (Test, Test_High_Mask'Access,            "High_Mask");
      Register_Routine (Test, Test_Pop_Count'Access,            "Pop_Count");
      Register_Routine (Test, Test_Count_Trailing_Zeros'Access, "Count_Trailing_Zeros");
   end Register_Tests;

   -------------------
   -- Test_Bit_Mask --
   -------------------
   procedure Test_Bit_Mask (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (2#0001# = Bit_Mask (0),  "Incorrect mask for 0");
      Assert (2#0100# = Bit_Mask (2),  "Incorrect mask for 2");
      Assert (2 ** 63 = Bit_Mask (63), "Incorrect mask for 63");
   end Test_Bit_Mask;

   -------------------
   -- Test_Low_Mask --
   -------------------
   procedure Test_Low_Mask (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (2#0000#     = Low_Mask (0),  "Incorrect result for 0");
      Assert (2#0001#     = Low_Mask (1),  "Incorrect result for 1");
      Assert (2#0111#     = Low_Mask (3),  "Incorrect result for 3");
      Assert (2 ** 63 - 1 = Low_Mask (63), "Incorrect result for 63");
      Assert (ALL_ONES    = Low_Mask (64), "Incorrect result for 64");
   end Test_Low_Mask;

   --------------------
   -- Test_High_Mask --
   --------------------
   procedure Test_High_Mask (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (ALL_ONES         = High_Mask (0),  "Incorrect result for 0");
      Assert ((not 2#0111#)    = High_Mask (3),  "Incorrect result for 3");
      Assert (2 ** 63          = High_Mask (63), "Incorrect result for 63");
   end Test_High_Mask;

   --------------------
   -- Test_Pop_Count --
   --------------------
   procedure Test_Pop_Count (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (0  = Pop_Count (2#0000#), "Incorrect pop count of 0");
      Assert (1  = Pop_Count (2#1000#), "Incorrect pop count of 1");
      Assert (3  = Pop_Count (2#1011#), "Incorrect pop count of 3");
      Assert (1  = Pop_Count (2 ** 63), "Incorrect pop count of the high bit");
      Assert (64 = Pop_Count (ALL_ONES), "Incorrect pop count of all ones");
   end Test_Pop_Count;

   -------------------------------
   -- Test_Count_Trailing_Zeros --
   -------------------------------
   procedure Test_Count_Trailing_Zeros (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
   begin
      Assert (0  = Count_Trailing_Zeros (2#0001#), "Incorrect count for 2#0001#");
      Assert (3  = Count_Trailing_Zeros (2#1000#), "Incorrect count for 2#1000#");
      Assert (1  = Count_Trailing_Zeros (2#0110#), "Incorrect count for 2#0110#");
      Assert (63 = Count_Trailing_Zeros (2 ** 63), "Incorrect count for the high bit");
   end Test_Count_Trailing_Zeros;

end Bit_Sets.Block_Tests;
