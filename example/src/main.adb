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

with Shared;                use Shared;
with Boolean_Array_Example; use Boolean_Array_Example;
with Bit_Set_Example;       use Bit_Set_Example;

procedure Main is
   LIMIT : constant Natural := 100_000_000;
   Bool_Array_Example : Boolean_Array_Example_State;
   Bits_Example       : Bit_Set_Example_State;
begin
   Run (Bool_Array_Example, LIMIT);
   Put (Bool_Array_Example, Boolean_Array_Example.HEADER);

   Run (Bits_Example, LIMIT);
   Put (Bits_Example, Bit_Set_Example.HEADER);
end Main;
