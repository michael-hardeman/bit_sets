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

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Bit_Sets.Bit_Set_Tests is

   type Integer_Array is array (Positive range <>) of Integer;

   type Test_Case is new Test_Cases.Test_Case with null record;

   overriding function Name (Test : Test_Case) return Message_String is (AUnit.Format ("Bit_Sets.Bit_Set_Tests"));

   overriding procedure Register_Tests (Test : in out Test_Case);

   procedure Test_Create          (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Empty_Set       (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Single_Indexes  (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Ranges          (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Whole_Sets      (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Operations  (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Comparisons     (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Copy_And_Assign (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Iteration       (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Shifts          (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Union_Shifted   (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Image           (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Preconditions   (Test : in out Test_Cases.Test_Case'Class);
end Bit_Sets.Bit_Set_Tests;
