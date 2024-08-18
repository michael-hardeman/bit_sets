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

with Ada.Calendar; use Ada.Calendar;

package body Boolean_Array_Example is
   procedure Run (Example : in out Boolean_Array_Example_State; Size : Long_Long_Integer) is
      First : constant Long_Long_Integer := Long_Long_Integer'First;
      Last  : constant Long_Long_Integer := Long_Long_Integer'First + Size - 1;
      Bits : Boolean_Array_Ptr := new Boolean_Array (First .. Last);
      Start_Time : Time;
   begin
      Example.Size_In_Bits := Bits.all'Size;

      Start_Time := Clock;
      for I in 0 .. (Size - 1) loop
         Bits.all (First + I) := I mod 2 = 0;
      end loop;
      Example.Time_To_Set_Bit_Pattern := Clock - Start_Time;

      Start_Time := Clock;
      for I in 0 .. (Size - 1) loop
         if Bits.all (First + I) then
            Example.Pop_Count := Example.Pop_Count + 1;
         end if;
      end loop;
      Example.Time_To_Count_Pop_Pattern := Clock - Start_Time;

      Example.Is_Alternating := True;
      Start_Time := Clock;
      for I in 0 .. (Size - 1) loop
         if I mod 2 = 0 then
            Example.Is_Alternating := Example.Is_Alternating and then Bits.all (First + I);
         else
            Example.Is_Alternating := Example.Is_Alternating and then not Bits.all (First + I);
         end if;
      end loop;
      Example.Time_To_Test_Bit_Pattern := Clock - Start_Time;

      Free (Bits);
   end Run;
end Boolean_Array_Example;