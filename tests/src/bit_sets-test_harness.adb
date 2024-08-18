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

with AUnit.Run;
with AUnit.Reporter.Text;

with Bit_Sets.Block_Test_Suite;
with Bit_Sets.Bit_Set_Test_Suite;

procedure Bit_Sets.Test_Harness is
   procedure Run_Block_Tests   is new AUnit.Run.Test_Runner (Block_Test_Suite.Suite);
   procedure Run_Bit_Set_Tests is new AUnit.Run.Test_Runner (Bit_Set_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Run_Block_Tests   (Reporter);
   Run_Bit_Set_Tests (Reporter);
end Bit_Sets.Test_Harness;
