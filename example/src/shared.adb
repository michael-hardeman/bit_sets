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

package body Shared is

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
      Put_Line ("Memory Size: " & Example.Memory_In_Bits'Image & " bits");
      Put_Line ("Time to sieve: " & Example.Time_To_Sieve'Image & "s");
      Put_Line ("Time to count primes: " & Example.Time_To_Count'Image & "s");
      Put_Line ("Primes found: " & Example.Prime_Count'Image);
      New_Line;
   end Put;

end Shared;
