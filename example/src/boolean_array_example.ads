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

with Ada.Unchecked_Deallocation;

with Shared;

package Boolean_Array_Example is

   --  This is how you would normally make a boolean array. Each boolean
   --  is allocated as a byte so this takes 8x more memory to store.
   --  It is quite performant to iterate as CPUs are great at loading
   --  contiguous arrays and doing an operation on each item
   --
   --  Unfortunately that means it's very easy to exceed the stack size
   --  or explode memory usage when doing an algo like the Sieve of Eratosthenes
   type Boolean_Array is array (Natural range <>) of Boolean;

   --  We have to allocate on the heap or else any size larger than about
   --  1_000_000 explodes the default stack
   type Boolean_Array_Ptr is access all Boolean_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Boolean_Array, Boolean_Array_Ptr);

   HEADER : constant String := "Boolean Array Example";

   type Boolean_Array_Example_State is new Shared.Example_State with null record;

   procedure Run (Example : in out Boolean_Array_Example_State; Limit : Natural);
end Boolean_Array_Example;
