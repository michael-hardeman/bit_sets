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

package body Bit_Set_Bit_Iteration_Example is

   ---------------------------------
   -- Set_Alternating_Bit_Pattern --
   ---------------------------------
   procedure Set_Alternating_Bit_Pattern (Example : in out Bit_Set_Bit_Iteration_Example_State;
                                          Bits    : in out Bit_Set)
   is
      procedure Bits_Callback (Block : in out Block_Type;
                               Index : Long_Long_Integer;
                               Offset : Bit_Offset)
      is begin
         if Index mod 2 = 1 then
            Block := Block or Bit_Mask (Offset);
         end if;
      end Bits_Callback;

      Start_Time : Time;
   begin
      Start_Time := Clock;
      Iterate_Bits (Bits, Bits_Callback'Access);
      Example.Time_To_Set_Bit_Pattern := Clock - Start_Time;
   end Set_Alternating_Bit_Pattern;

   ----------------------
   -- Count_Population --
   ----------------------
   procedure Count_Population (Example : in out Bit_Set_Bit_Iteration_Example_State;
                               Bits    : Bit_Set)
   is
      procedure Reduce_Bits_To_Long_Long_Integer is new Reduce_Bits (Result_Type => Long_Long_Integer);

      procedure Pop_Count_Callback (Result : in out Long_Long_Integer;
                                    Block  : Block_Type;
                                    Index  : Long_Long_Integer;
                                    Offset : Bit_Offset)
      is
         pragma Unreferenced (Index);
      begin
         if Test_Bit (Block, Offset) then
            Result := Result + 1;
         end if;
      end Pop_Count_Callback;

      Start_Time : Time;
   begin
      Start_Time := Clock;
      Reduce_Bits_To_Long_Long_Integer (Example.Pop_Count,
                                        Bits,
                                        Pop_Count_Callback'Access);
      Example.Time_To_Count_Pop_Pattern := Clock - Start_Time;
   end Count_Population;

   -------------------------
   -- Test_Is_Alternating --
   -------------------------
   procedure Test_Is_Alternating (Example : in out Bit_Set_Bit_Iteration_Example_State;
                                  Bits    : Bit_Set)
   is
      procedure Reduce_Bits_To_Boolean is new Reduce_Bits (Result_Type => Boolean);

      procedure Test_Bits_Alternate_Callback (Result : in out Boolean;
                                              Block  : Block_Type;
                                              Index  : Long_Long_Integer;
                                              Offset : Bit_Offset)
      is begin
         if Index mod 2 = 1 then
            Result := Result and then Test_Bit (Block, Offset);
         else
            Result := Result and then not Test_Bit (Block, Offset);
         end if;
      end Test_Bits_Alternate_Callback;

      Start_Time : Time;
   begin
      Start_Time := Clock;
      Reduce_Bits_To_Boolean (Example.Is_Alternating,
                              Bits,
                              Test_Bits_Alternate_Callback'Access);
      Example.Time_To_Test_Bit_Pattern := Clock - Start_Time;
   end Test_Is_Alternating;

   ---------
   -- Run --
   ---------
   procedure Run (Example : in out Bit_Set_Bit_Iteration_Example_State; Size : Long_Long_Integer) is
      Bits : Bit_Set;
   begin
      Initialize (Bits, Size);

      Example.Size_In_Bits := Size_In_Bits (Bits);

      Set_Alternating_Bit_Pattern (Example, Bits);
      Count_Population (Example, Bits);
      Test_Is_Alternating (Example, Bits);
   end Run;

end Bit_Set_Bit_Iteration_Example;