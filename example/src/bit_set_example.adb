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

with Bit_Sets.Generic_Sets;

package body Bit_Set_Example is

   package Natural_Sets is new Bit_Sets.Generic_Sets (Natural);
   use Natural_Sets;

   procedure Run (Example : in out Bit_Set_Example_State; Limit : Natural) is
      --  Every number starts out as a prime candidate
      Primes     : Bit_Set := Create (0, Limit, Value => True);
      Prime      : Natural := 2;
      Multiple   : Natural;
      Start_Time : Time;
   begin
      Example.Memory_In_Bits := Long_Long_Integer (Primes.Storage_Size);

      Start_Time := Clock;
      Primes.Exclude (0, 1);
      while Prime <= Limit / Prime loop
         if Primes.Contains (Prime) then
            Multiple := Prime * Prime;
            while Multiple <= Limit loop
               Primes.Exclude (Multiple);
               Multiple := Multiple + Prime;
            end loop;
         end if;
         Prime := Prime + 1;
      end loop;
      Example.Time_To_Sieve := Clock - Start_Time;

      --  Count uses the hardware pop count on 64 bits at a time
      Start_Time := Clock;
      Example.Prime_Count := Long_Long_Integer (Primes.Count);
      Example.Time_To_Count := Clock - Start_Time;
   end Run;

end Bit_Set_Example;
