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

with Ada.Assertions;

with AUnit.Assertions; use AUnit.Assertions;

with Bit_Sets.Generic_Sets;

package body Bit_Sets.Bit_Set_Tests is

   package Sets is new Bit_Sets.Generic_Sets (Integer);
   use Sets;

   --  Bounds that are not block aligned: 136 bits, 3 blocks, 8 bits in the last
   FIRST  : constant := -5;
   LAST   : constant := 130;
   LENGTH : constant := LAST - FIRST + 1;

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Create'Access,          "Create");
      Register_Routine (Test, Test_Empty_Set'Access,       "Empty_Set");
      Register_Routine (Test, Test_Single_Indexes'Access,  "Single_Indexes");
      Register_Routine (Test, Test_Ranges'Access,          "Ranges");
      Register_Routine (Test, Test_Whole_Sets'Access,      "Whole_Sets");
      Register_Routine (Test, Test_Set_Operations'Access,  "Set_Operations");
      Register_Routine (Test, Test_Comparisons'Access,     "Comparisons");
      Register_Routine (Test, Test_Copy_And_Assign'Access, "Copy_And_Assign");
      Register_Routine (Test, Test_Iteration'Access,       "Iteration");
      Register_Routine (Test, Test_Shifts'Access,          "Shifts");
      Register_Routine (Test, Test_Union_Shifted'Access,   "Union_Shifted");
      Register_Routine (Test, Test_Image'Access,           "Image");
      Register_Routine (Test, Test_Preconditions'Access,   "Preconditions");
   end Register_Tests;

   -----------------
   -- Test_Create --
   -----------------
   procedure Test_Create (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Empty : constant Bit_Set := Create (FIRST, LAST);
      Full  : constant Bit_Set := Create (FIRST, LAST, Value => True);
   begin
      Assert (FIRST  = Empty.First_Index,  "First_Index was not respected");
      Assert (LAST   = Empty.Last_Index,   "Last_Index was not respected");
      Assert (LENGTH = Empty.Length,       "Length is wrong");
      Assert (3 * 64 = Empty.Storage_Size, "Storage should be 3 whole blocks");
      Assert (Empty.Is_Empty,              "Default set should be empty");
      Assert (0      = Empty.Count,        "Default set should have no members");
      Assert (Full.Is_Full,                "Set created with True should be full");
      Assert (LENGTH = Full.Count,         "Full set should count every index, but none of the unused bits");
   end Test_Create;

   --------------------
   -- Test_Empty_Set --
   --------------------
   procedure Test_Empty_Set (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S : Bit_Set := Create (1, 0);
   begin
      Assert (0 = S.Length,                    "Length should be 0");
      Assert (0 = S.Storage_Size,              "No storage should be used");
      Assert (S.Is_Empty,                      "An empty set is empty");
      Assert (S.Is_Full,                       "An empty set is also full");
      Assert (not Has_Element (S, S.First),    "There is nothing to iterate");
      S.Fill;
      S.Flip;
      S.Include (1, 0);
      Assert (0 = S.Count,                     "Whole set operations should do nothing");
   end Test_Empty_Set;

   -------------------------
   -- Test_Single_Indexes --
   -------------------------
   procedure Test_Single_Indexes (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S : Bit_Set := Create (FIRST, LAST);
   begin
      --  First and last index, and both sides of the first block boundary
      for I of Integer_Array'[FIRST, 58, 59, LAST] loop
         S.Include (I);
         Assert (S.Contains (I), "Index" & I'Image & " should be included");
      end loop;
      Assert (not S.Contains (0), "Index 0 should not be included");
      Assert (4 = S.Count,        "There should be 4 members");

      --  Excluding twice must leave the bit clear, not toggle it back on
      S.Exclude (58);
      S.Exclude (58);
      Assert (not S.Contains (58), "Index 58 should be excluded");
      Assert (3 = S.Count,         "There should be 3 members");

      S.Flip (0);
      Assert (S.Contains (0),     "Flip should include index 0");
      S.Flip (0);
      Assert (not S.Contains (0), "Flip should exclude index 0");
   end Test_Single_Indexes;

   -----------------
   -- Test_Ranges --
   -----------------
   procedure Test_Ranges (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S : Bit_Set := Create (FIRST, LAST);
   begin
      --  Within one block
      S.Include (10, 20);
      Assert (11 = S.Count,         "Range within a block should include 11 indexes");
      Assert (not S.Contains (9),   "Index before the range should not be included");
      Assert (S.Contains (10),      "First index of the range should be included");
      Assert (S.Contains (20),      "Last index of the range should be included");
      Assert (not S.Contains (21),  "Index after the range should not be included");

      --  Across all three blocks
      S.Include (FIRST, LAST);
      Assert (S.Is_Full,            "Including every index should fill the set");
      S.Exclude (0, 100);
      Assert (LENGTH - 101 = S.Count, "Range across blocks should exclude 101 indexes");
      Assert (S.Contains (-1),      "Index before the range should still be included");
      Assert (not S.Contains (0),   "First index of the range should be excluded");
      Assert (not S.Contains (100), "Last index of the range should be excluded");
      Assert (S.Contains (101),     "Index after the range should still be included");

      --  Reversed ranges are empty
      S.Exclude (50, 49);
      Assert (LENGTH - 101 = S.Count, "A reversed range should change nothing");
   end Test_Ranges;

   ---------------------
   -- Test_Whole_Sets --
   ---------------------
   procedure Test_Whole_Sets (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S : Bit_Set := Create (FIRST, LAST);
   begin
      S.Flip;
      Assert (S.Is_Full,         "Flipping an empty set should fill it");
      Assert (LENGTH = S.Count,  "Flip must not set the unused bits of the last block");
      S.Flip;
      Assert (S.Is_Empty,        "Flipping a full set should empty it");
      S.Fill;
      Assert (LENGTH = S.Count,  "Fill must not set the unused bits of the last block");
      S.Exclude (LAST);
      Assert (not S.Is_Full,     "A set missing its last index is not full");
      S.Clear;
      Assert (S.Is_Empty,        "Clear should empty the set");
   end Test_Whole_Sets;

   -------------------------
   -- Test_Set_Operations --
   -------------------------
   procedure Test_Set_Operations (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      A : Bit_Set := Create (FIRST, LAST);
      B : Bit_Set := Create (FIRST, LAST);
   begin
      A.Include (0, 69);   --  70 members, crosses a block boundary
      B.Include (60, 89);  --  30 members, 10 shared with A

      Assert (90  = Count (A or B),  """or"" should be the union");
      Assert (10  = Count (A and B), """and"" should be the intersection");
      Assert (60  = Count (A - B),   """-"" should be the difference");
      Assert (80  = Count (A xor B), """xor"" should be the symmetric difference");
      Assert (LENGTH - 70 = Count (not A), """not"" should be the complement");

      declare
         Target : Bit_Set := Copy (A);
      begin
         Target.Union (B);
         Assert (Target = (A or B),  "Union should match ""or""");
         Target.Assign (A);
         Target.Intersection (B);
         Assert (Target = (A and B), "Intersection should match ""and""");
         Target.Assign (A);
         Target.Difference (B);
         Assert (Target = A - B,     "Difference should match ""-""");
         Target.Assign (A);
         Target.Symmetric_Difference (B);
         Assert (Target = (A xor B), "Symmetric_Difference should match ""xor""");
      end;
   end Test_Set_Operations;

   ----------------------
   -- Test_Comparisons --
   ----------------------
   procedure Test_Comparisons (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Small   : Bit_Set := Create (FIRST, LAST);
      Large   : Bit_Set := Create (FIRST, LAST);
      Other   : Bit_Set := Create (FIRST, LAST);
      Shifted : constant Bit_Set := Create (FIRST + 1, LAST + 1);
   begin
      Small.Include (10, 20);
      Large.Include (0, 100);
      Other.Include (110, 120);

      Assert (Small = Small,             "A set equals itself");
      Assert (Small /= Large,            "Sets with different members are not equal");
      Assert (Create (FIRST, LAST) /= Shifted, "Sets with different bounds are not equal");
      Assert (Small <= Large,            "Small is a subset of Large");
      Assert (not (Large <= Small),      "Large is not a subset of Small");
      Assert (Small.Overlaps (Large),    "Small and Large overlap");
      Assert (not Small.Overlaps (Other), "Small and Other do not overlap");
   end Test_Comparisons;

   --------------------------
   -- Test_Copy_And_Assign --
   --------------------------
   procedure Test_Copy_And_Assign (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Original : Bit_Set := Create (FIRST, LAST);
   begin
      Original.Include (42);
      declare
         Duplicate : Bit_Set := Copy (Original);
      begin
         Assert (Duplicate = Original,    "A copy should equal the original");
         Duplicate.Include (43);
         Assert (not Original.Contains (43), "Changing a copy must not change the original");
         Duplicate.Assign (Original);
         Assert (Duplicate = Original,    "Assign should copy the contents");
      end;
      Assert (Original.Contains (42),     "The original should survive its copy being finalized");
   end Test_Copy_And_Assign;

   --------------------
   -- Test_Iteration --
   --------------------
   procedure Test_Iteration (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Expected : constant Integer_Array := [FIRST, 58, 59, 100, LAST];
      S        : Bit_Set := Create (FIRST, LAST);
      Visited  : Natural := 0;
   begin
      for I of Expected loop
         S.Include (I);
      end loop;

      for I of S loop
         Visited := Visited + 1;
         Assert (Visited <= Expected'Length, "Visited more indexes than were included");
         Assert (Expected (Visited) = I,     "Expected" & Expected (Visited)'Image & " but got" & I'Image);
      end loop;
      Assert (Expected'Length = Visited, "Every included index should be visited");

      Assert (100 = Element (S, S.Find (60)), "Find should skip to the next member");
      Assert (LAST = Element (S, S.Find (LAST)), "Find should include its starting index");
      S.Exclude (LAST);
      Assert (not Has_Element (S, S.Find (101)), "Find past the last member should give No_Element");
   end Test_Iteration;

   -----------------
   -- Test_Shifts --
   -----------------
   procedure Test_Shifts (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      --  Sizes around every block boundary, word aligned and not, and past the end
      AMOUNTS : constant array (Positive range <>) of Count_Type :=
        [0, 1, 7, 63, 64, 65, 70, 127, 128, 129, LENGTH - 1, LENGTH, LENGTH + 50];

      Original : Bit_Set := Create (FIRST, LAST);

      --  The expected result, built one index at a time
      function Reference (Amount : Count_Type; Left : Boolean) return Bit_Set is
      begin
         return Expected : Bit_Set := Create (FIRST, LAST) do
            for I of Original loop
               declare
                  Moved : constant Long_Long_Integer :=
                    Long_Long_Integer (I) + (if Left then 1 else -1) * Long_Long_Integer (Amount);
               begin
                  if Moved in FIRST .. LAST then
                     Expected.Include (Integer (Moved));
                  end if;
               end;
            end loop;
         end return;
      end Reference;
   begin
      for I of Integer_Array'[FIRST, FIRST + 1, 0, 58, 59, 63, 64, 100, LAST - 1, LAST] loop
         Original.Include (I);
      end loop;

      for Amount of AMOUNTS loop
         Assert (Shift_Left (Original, Amount) = Reference (Amount, Left => True),
                 "Shift_Left by" & Amount'Image & " gave " & Shift_Left (Original, Amount)'Image
                 & " expected " & Reference (Amount, Left => True)'Image);
         Assert (Shift_Right (Original, Amount) = Reference (Amount, Left => False),
                 "Shift_Right by" & Amount'Image & " gave " & Shift_Right (Original, Amount)'Image
                 & " expected " & Reference (Amount, Left => False)'Image);
      end loop;

      --  Shifting a full set left must not set the unused bits of the last block
      declare
         Full : Bit_Set := Create (FIRST, LAST, Value => True);
      begin
         Full.Shift_Left (3);
         Assert (LENGTH - 3 = Full.Count, "Shift_Left must drop members shifted past Last");
         Assert (not Full.Contains (FIRST + 2) and then Full.Contains (FIRST + 3),
                 "Shift_Left must exclude the vacated indexes");
         Full.Shift_Right (10);
         Assert (LENGTH - 10 = Full.Count, "Shift_Right must drop members shifted past First");
         Assert (Full.Contains (LAST - 10) and then not Full.Contains (LAST - 9),
                 "Shift_Right must exclude the vacated indexes");
      end;
   end Test_Shifts;

   ------------------------
   -- Test_Union_Shifted --
   ------------------------
   procedure Test_Union_Shifted (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);

      AMOUNTS : constant array (Positive range <>) of Count_Type :=
        [0, 1, 7, 63, 64, 65, 70, 127, 128, 129, LENGTH - 1, LENGTH, LENGTH + 50];

      Target : Bit_Set := Create (FIRST, LAST);
      Source : Bit_Set := Create (FIRST, LAST);
   begin
      Target.Include (-3, 4);
      Target.Include (90);

      --  The block boundaries and ends, plus a dense irregular pattern so carries
      --  between blocks happen at every amount. An aliased shift done in the wrong
      --  order only shows up when bits near the top of a block move into the next.
      for I of Integer_Array'[FIRST, 0, 58, 59, 63, 64, 100, LAST] loop
         Source.Include (I);
      end loop;
      for I in FIRST .. LAST loop
         if (I * 37) mod 11 < 4 then
            Source.Include (I);
         end if;
      end loop;

      for Amount of AMOUNTS loop
         --  Separate sets
         declare
            Expected : constant Bit_Set := Target or Shift_Left (Source, Amount);
            Actual   : Bit_Set := Copy (Target);
         begin
            Actual.Union_Shifted (Source, Amount);
            Assert (Actual = Expected,
                    "Union_Shifted by" & Amount'Image & " gave " & Actual'Image & " expected " & Expected'Image);
         end;

         --  Source is Target
         declare
            Expected : constant Bit_Set := Source or Shift_Left (Source, Amount);
            Actual   : Bit_Set := Copy (Source);
         begin
            Union_Shifted (Actual, Actual, Amount);
            Assert (Actual = Expected,
                    "Aliased Union_Shifted by" & Amount'Image & " gave " & Actual'Image
                    & " expected " & Expected'Image);
         end;
      end loop;

      --  Subset sums of 3, 5 and 7 up to 20
      declare
         Reachable : Bit_Set := Create (0, 20);
      begin
         Reachable.Include (0);
         for W of Integer_Array'[3, 5, 7] loop
            Union_Shifted (Reachable, Reachable, Count_Type (W));
         end loop;
         Assert ("{0, 3, 5, 7, 8, 10, 12, 15}" = Reachable'Image, "Subset sums were " & Reachable'Image);
      end;
   end Test_Union_Shifted;

   ----------------
   -- Test_Image --
   ----------------
   procedure Test_Image (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S : Bit_Set := Create (FIRST, LAST);
   begin
      Assert ("{}" = S'Image, "Empty set image was " & S'Image);

      S.Include (10);
      Assert ("{10}" = S'Image, "Single member image was " & S'Image);

      S.Include (FIRST, 2);
      S.Include (58, 59);
      S.Include (100, LAST);
      Assert ("{-5 .. 2, 10, 58, 59, 100 .. 130}" = S'Image, "Runs image was " & S'Image);

      S.Include (60, 99);
      Assert ("{-5 .. 2, 10, 58 .. 130}" = S'Image, "Run across blocks image was " & S'Image);

      S.Fill;
      Assert ("{-5 .. 130}" = S'Image, "Full set image was " & S'Image);
      Assert ("{}" = Create (1, 0)'Image, "Zero length set image was " & Create (1, 0)'Image);
   end Test_Image;

   ------------------------
   -- Test_Preconditions --
   ------------------------
   procedure Test_Preconditions (Test : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      S     : Bit_Set := Create (FIRST, LAST);
      Other : constant Bit_Set := Create (FIRST, LAST + 1);
   begin
      begin
         S.Include (LAST + 1);
         Assert (False, "Including an index out of range should fail");
      exception
         when Ada.Assertions.Assertion_Error => null;
      end;

      begin
         S.Union (Other);
         Assert (False, "Combining sets with different bounds should fail");
      exception
         when Ada.Assertions.Assertion_Error => null;
      end;
   end Test_Preconditions;

end Bit_Sets.Bit_Set_Tests;
