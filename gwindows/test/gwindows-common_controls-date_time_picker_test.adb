--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with AUnit.Utilities; use AUnit.Utilities;
with GWindows.Testing.Generic_Task;
with GWindows.Types.AUnit;
with GWindows.Windows;
with Test_Common_Controls_Aux;
package body GWindows.Common_Controls.Date_Time_Picker_Test is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is
      use Test_Common_Controls_Aux;
      Common_Window    : Top_Window_Type renames Top_Window_Type (Window.all);
      Noon_27_Feb_2005 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
        (Year    => 2005,
         Month   => 2,
         Day     => 27,
         Seconds => 43_200.0);
   begin
      Create
        (Common_Window,
         Title  => "test_duration_picker");

      Common_Window.Control_1 := new Date_Time_Picker_Type;

      Create
        (Date_Time_Picker_Type (Common_Window.Control_1.all),
         Parent => Common_Window,
         Format => Long_Format);

      Date_Time (Date_Time_Picker_Type (Common_Window.Control_1.all),
                 Noon_27_Feb_2005);

      Common_Window.Control_2 := new Date_Time_Picker_Type;

      Create
        (Date_Time_Picker_Type (Common_Window.Control_2.all),
         Parent => Common_Window,
         Format => Time_Format);

      Date_Time (Date_Time_Picker_Type (Common_Window.Control_2.all),
                 Noon_27_Feb_2005);

      Size (Common_Window, Recommended_Size (Common_Window));
   end Create;

   package Background is new GWindows.Testing.Generic_Task (Create);

   Main_Window : aliased Test_Common_Controls_Aux.Top_Window_Type;

   procedure Start;
   procedure Start
   is begin
      Background.Background_Task.Create_Window (Main_Window'Access);

      Background.Background_Task.Run;

      Background.Test_Delay; --  let user see window when Debug_Level > 0.
   end Start;

   procedure Finish;
   procedure Finish
   is begin
      case Background.Debug_Level is
      when 0 | 1 =>
         Background.Close;

      when others =>
         --  wait for user to close window; let them experiment.
         null;
      end case;

      Background.Background_Task.Wait_Shutdown;

   end Finish;

   ----------
   --  Test procedures

   procedure Test_Format_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Format_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Types.AUnit;
   begin
      Check
        ("Long_Format",
         Computed => Largest_Formatted_String ("dddd, MMMM d, yyyy"),
         Expected => "Wednesday, December 30, 2000");

      Check
        ("Short_Format",
         Computed => Largest_Formatted_String ("M/d/yy"),
         Expected => "12/30/00");

      Check
        ("Time_Format",
         Computed => Largest_Formatted_String ("h:m:s tt"),
         Expected => "12:59:59 AM");

   end Test_Format_Size;

   procedure Test_Recommended_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Recommended_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Types.AUnit;
   begin
      Start;

      Check
        ("long date",
         Computed => Size (Main_Window.Control_1.all),
         Expected => (Width => 182, Height => 19));

      Check
        ("time",
         Computed => Size (Main_Window.Control_2.all),
         Expected => (Width => 86, Height => 19));

      --  We can't set the font on the common controls.

      Finish;
   exception
   when others =>
      Finish;
      raise;
   end Test_Recommended_Size;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("GWindows.Common_Controls.Date_Time_Picker_Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      case T.Debug_Level is
         when 1 =>
            Register_Routine
              (T, Test_Recommended_Size'Access, "Test_Recommended_Size");
         when others =>
            Register_Routine
              (T, Test_Format_Size'Access, "Test_Format_Size");
            Register_Routine
              (T, Test_Recommended_Size'Access, "Test_Recommended_Size");
      end case;
   end Register_Tests;

   procedure Set_Up_Case (Test : in out Test_Case)
   is begin
      Background.Debug_Level := Test.Debug_Level;

      case Test.Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Background.Test_Delay_Time := 1.0;

      when others =>
         null;
      end case;

      Background.Background_Task.Init;

   end Set_Up_Case;

end GWindows.Common_Controls.Date_Time_Picker_Test;
