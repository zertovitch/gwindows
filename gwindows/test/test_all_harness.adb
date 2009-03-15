--  Abstract :
--
--  Run all tests of GWindows stuff.
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with GWindows.Combo_Boxes.Test;
with GWindows.Common_Controls.Date_Time_Picker_Test;
with GWindows.Edit_Boxes.Test;
with GWindows.Static_Controls.Test;
with GWindows.GControls.Duration.Edit.Test;
with Test_Duration;
with Test_Edit_Boxes_Generic_Integer;
with Test_Packing_Boxes;
with Test_Packing_Boxes_Autosize;
procedure Test_All_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   Result : AUnit.Test_Results.Result;

begin
   --  Alphabetical by test package name

   Add_Test
     (Suite,
      new GWindows.Combo_Boxes.Test.Test_Case
        (Debug_Level => 0));

   Add_Test
     (Suite,
      new GWindows.Common_Controls.Date_Time_Picker_Test.Test_Case
        (Debug_Level => 0));

   Add_Test
     (Suite,
      new GWindows.Edit_Boxes.Test.Test_Case
        (Debug_Level => 0));

   Add_Test
     (Suite,
      new GWindows.Static_Controls.Test.Test_Case
        (Debug_Level => 0));

   Add_Test
     (Suite,
      new GWindows.GControls.Duration.Edit.Test.Test_Case (Debug_Level => 0));

   Add_Test (Suite, new Test_Duration.Test_Case (Debug_Level => 0));

   Add_Test
     (Suite,
      new Test_Edit_Boxes_Generic_Integer.Test_Case
        (Debug_Level => 0));

   Add_Test (Suite, new Test_Packing_Boxes.Test_Case (Debug_Level => 0));
   Add_Test
     (Suite, new Test_Packing_Boxes_Autosize.Test_Case (Debug_Level => 0));

   Run (Suite.all, Result);
   AUnit.Test_Results.Text_Reporter.Report (Result);
end Test_All_Harness;
