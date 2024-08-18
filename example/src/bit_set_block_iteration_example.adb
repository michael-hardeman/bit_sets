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

with Bit_Sets; use Bit_Sets;

package body Bit_Set_Block_Iteration_Example is

   ---------------------------------
   -- Set_Alternating_Bit_Pattern --
   ---------------------------------
   procedure Set_Alternating_Bit_Pattern (Example : in out Bit_Set_Block_Iteration_Example_State;
                                          Bits    : in out Bit_Set)
   is

      procedure Alternate_Bits_Block_Callback (Block : in out Block_Type; First_Index : Long_Long_Integer) is
         pragma Unreferenced (First_Index);
      begin
         Block := LOW_ALTERNATING;
      end Alternate_Bits_Block_Callback;

      procedure Alternate_Bits_Remaining_Callback  (Block          : in out Block_Type;
                                                    First_Index    : Long_Long_Integer;
                                                    Remaining_Bits : Bit_Count;
                                                    Remaining_Mask : Block_Type)
      is
         pragma Unreferenced (First_Index);
         pragma Unreferenced (Remaining_Bits);
      begin
         Block := LOW_ALTERNATING and Remaining_Mask;
      end Alternate_Bits_Remaining_Callback;

      Start_Time : Time;
   begin
      Start_Time := Clock;
      Iterate_Blocks (Bits,
                      Alternate_Bits_Block_Callback'Access,
                      Alternate_Bits_Remaining_Callback'Access);
      Example.Time_To_Set_Bit_Pattern := Clock - Start_Time;
   end Set_Alternating_Bit_Pattern;

   ----------------------
   -- Count_Population --
   ----------------------
   procedure Count_Population (Example : in out Bit_Set_Block_Iteration_Example_State;
                               Bits    : Bit_Set)
   is
      Start_Time : Time;
   begin
      Start_Time := Clock;
      Example.Pop_Count := Count_Population (Bits);
      Example.Time_To_Count_Pop_Pattern := Clock - Start_Time;
   end Count_Population;

   -------------------------
   -- Test_Is_Alternating --
   -------------------------
   procedure Test_Is_Alternating (Example : in out Bit_Set_Block_Iteration_Example_State;
                                  Bits    : Bit_Set)
   is
      Start_Time : Time;
   begin
      Start_Time := Clock;
      Example.Is_Alternating := Test_All_Equal (Bits, LOW_ALTERNATING);
      Example.Time_To_Test_Bit_Pattern := Clock - Start_Time;
   end Test_Is_Alternating;

   ---------
   -- Run --
   ---------
   procedure Run (Example : in out Bit_Set_Block_Iteration_Example_State;
                  Size    : Long_Long_Integer)
   is
      Bits : Bit_Set;
   begin
      Initialize (Bits, Size);

      Example.Size_In_Bits := Size_In_Bits (Bits);

      Set_Alternating_Bit_Pattern (Example, Bits);
      Count_Population (Example, Bits);
      Test_Is_Alternating (Example, Bits);
   end Run;

end Bit_Set_Block_Iteration_Example;