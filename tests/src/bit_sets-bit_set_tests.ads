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

   type Test_Case is new Test_Cases.Test_Case with null record;

   overriding function Name (Test : Test_Case) return Message_String is (AUnit.Format ("Bit_Sets.Bit_Set_Tests"));

   overriding procedure Register_Tests (Test : in out Test_Case);

   procedure Test_Initialize       (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Size_In_Bits     (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Test_All_Equal   (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Test_All_Set     (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Test_All_Unset   (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Count_Population (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Iterate_Blocks   (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Iterate_Bits     (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Reduce_Blocks    (Test : in out Test_Cases.Test_Case'Class);
   procedure Test_Reduce_Bits      (Test : in out Test_Cases.Test_Case'Class);
end Bit_Sets.Bit_Set_Tests;