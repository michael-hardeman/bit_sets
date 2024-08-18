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

with Ada.Text_IO; use Ada.Text_IO;

package body Bit_Set_Examples is

   procedure Put_Header (Header : String) is begin
      for I in 1 .. Header'Length + 6 loop
         Put ("-");
      end loop;
      New_Line;
      Put_Line ("-- " & Header & " --");
      for I in 1 .. Header'Length + 6 loop
         Put ("-");
      end loop;
      New_Line;
   end Put_Header;

   procedure Put (Example : Example_State'Class; Header : String) is begin
      Put_Header (Header);
      Put_Line ("Memory Size: " & Example.Size_In_Bits'Image & " bits");
      Put_Line ("Time to set alternating bit pattern: " & Example.Time_To_Set_Bit_Pattern'Image & "s");
      Put_Line ("Time to count population: " & Example.Time_To_Count_Pop_Pattern'Image & "s");
      Put_Line ("Population: " & Example.Pop_Count'Image);
      Put_Line ("Time to test all bits: " & Example.Time_To_Test_Bit_Pattern'Image & "s");
      Put_Line ("Is_Alternating: " & Example.Is_Alternating'Image);
      New_Line;
   end Put;

end Bit_Set_Examples;