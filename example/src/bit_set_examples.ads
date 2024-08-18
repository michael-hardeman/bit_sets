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

package Bit_Set_Examples is

   type Example_State is tagged record
      Size_In_Bits : Long_Long_Integer := 0;
      Time_To_Set_Bit_Pattern : Duration;
      Time_To_Count_Pop_Pattern : Duration;
      Pop_Count : Long_Long_Integer := 0;
      Time_To_Test_Bit_Pattern : Duration;
      Is_Alternating : Boolean := True;
   end record;

   procedure Put (Example : Example_State'Class; Header : String);

end Bit_Set_Examples;