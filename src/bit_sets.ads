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

-------------
-- Bit_Sets --
-------------
--  Packed sets of bits. Each element costs one bit of memory instead of the
--  byte (or more) a plain array of Boolean uses.
--
--  Instantiate Bit_Sets.Generic_Sets with the index type you need:
--
--     package Flags is new Bit_Sets.Generic_Sets (Natural);
--     Sieve : Flags.Bit_Set := Flags.Create (0, 1_000_000_000, Value => True);
package Bit_Sets with Pure is

   --  Number of bits in a set
   type Count_Type is range 0 .. 2 ** 62;

end Bit_Sets;
