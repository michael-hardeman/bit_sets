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
   procedure Run (Example : in out Boolean_Array_Example_State; Limit : Natural) is
      Is_Prime   : Boolean_Array_Ptr := new Boolean_Array'(0 .. Limit => True);
      Prime      : Natural := 2;
      Multiple   : Natural;
      Start_Time : Time;
   begin
      Example.Memory_In_Bits := Is_Prime.all'Size;

      Start_Time := Clock;
      Is_Prime (0) := False;
      Is_Prime (1) := False;
      while Prime <= Limit / Prime loop
         if Is_Prime (Prime) then
            Multiple := Prime * Prime;
            while Multiple <= Limit loop
               Is_Prime (Multiple) := False;
               Multiple := Multiple + Prime;
            end loop;
         end if;
         Prime := Prime + 1;
      end loop;
      Example.Time_To_Sieve := Clock - Start_Time;

      Start_Time := Clock;
      for Value of Is_Prime.all loop
         if Value then
            Example.Prime_Count := Example.Prime_Count + 1;
         end if;
      end loop;
      Example.Time_To_Count := Clock - Start_Time;

      Free (Is_Prime);
   end Run;
end Boolean_Array_Example;
