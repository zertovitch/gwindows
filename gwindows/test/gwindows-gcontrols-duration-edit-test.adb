--  Abstract:
--
--  see spec.
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

with AUnit.Test_Cases.Registration;
with AUnit.Utilities; use AUnit.Utilities;
with GWindows.Colors.AUnit;
with GWindows.Drawing_Objects;
with GWindows.GControls.Duration.Edit.AUnit;
with GWindows.Key_States;
with GWindows.Testing.Events;
with GWindows.Testing.Generic_Task;
with GWindows.Types.AUnit;
with GWindows.Windows;
with Test_Duration_Edit_Aux;
package body GWindows.GControls.Duration.Edit.Test is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is begin
      Test_Duration_Edit_Aux.Create
        (Test_Duration_Edit_Aux.Top_Window_Type (Window.all),
         Top    => 20,
         Left   => 0,
         Width  => 190,
         Height => 190,
         Title  => "test_duration_edit");
      GWindows.Windows.Show (Window.all);
   end Create;

   package Background is new GWindows.Testing.Generic_Task (Create);

   procedure Start;
   Main_Window : aliased Test_Duration_Edit_Aux.Top_Window_Type;

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

   procedure Test_1 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);

   procedure Test_1 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Background;
      use GWindows.Colors.AUnit;
      use GWindows.Colors;
      use GWindows.Drawing;
      use GWindows.GControls.Duration.Edit.AUnit;
      use GWindows.Key_States;
      use GWindows.Testing.Events;
      Background_Highlight : constant Color_Type :=
        System_Color (COLOR_HIGHLIGHT);
   begin

      Start;

      --  On_Create event handler callback sets data; test
      Check
        ("On_Create",
         Main_Window.Duration_Edit.Data,
         60.0);

      --  Test field selection with mouse
      Move_To ((15, 15)); -- hour background
      Click_Left;
      Check
        ("click hour",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Hour);
      declare
         Canvas : Canvas_Type;
      begin
         Test_Duration_Edit_Aux.Get_Canvas (Main_Window, Canvas);
         --  Check background colors; too hard to get right pixel for
         --  text colors.
         --
         --  WORKAROUND: for some reason, colors on my laptop can be off by 1.
         Check
           ("hour highlight hour background",
            Point (Canvas, 15, 15),
            Background_Highlight,
            Tolerance => 1);
         Check
           ("hour highlight minute background",
            Point (Canvas, 30, 15),
            White);
      end;

      Move_To ((30, 10)); -- minute
      Click_Left;
      Check
        ("click minute",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Minute);
      declare
         Canvas : Canvas_Type;
      begin
         Test_Duration_Edit_Aux.Get_Canvas (Main_Window, Canvas);
         Check
           ("minute highlight hour background",
            Point (Canvas, 15, 15),
            White);
         Check
           ("minute highlight minute background",
            Point (Canvas, 30, 15),
            Background_Highlight,
            Tolerance  => 1);
      end;

      Move_To ((30, 30)); -- out of the way

      --  Test field selection with tab
      Key_Event (VK_SHIFT, Key_Up => False);
      Key_Stroke (VK_TAB); -- hour
      Key_Event (VK_SHIFT, Key_Up => True);
      Check
        ("shift tab to hour",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Hour);

      Key_Stroke (VK_TAB); -- minute
      Check
        ("tab to minute",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Minute);

      --  Test field selection with arrow keys.
      Key_Stroke (VK_LEFT);  -- hour
      Check
        ("left to hour",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Hour);

      Key_Stroke (VK_LEFT);  -- no motion
      Check
        ("left no motion",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Hour);

      Key_Stroke (VK_RIGHT); -- minute
      Check
        ("right to minute",
         Main_Window.Duration_Edit.Current_Field,
         Expected => Minute);

      --  Test field increment/decrement with arrow keys.
      Key_Stroke (VK_UP);  -- minute
      Check ("minute up", Main_Window.Duration_Edit.Data, 120.0);

      Key_Stroke (VK_DOWN);
      Key_Stroke (VK_DOWN);
      Check ("minute down", Main_Window.Duration_Edit.Data, 0.0);
      Key_Stroke (VK_DOWN); -- down past 0:0
      Check ("minute down past 0", Main_Window.Duration_Edit.Data, 0.0);
      Key_Stroke (VK_LEFT);  -- hour
      Key_Stroke (VK_UP);
      Check ("hour up", Main_Window.Duration_Edit.Data, 3600.0);
      Key_Stroke (VK_DOWN);
      Check ("hour down", Main_Window.Duration_Edit.Data, 0.0);
      Key_Stroke (VK_RIGHT);  -- minute
      Key_Stroke (VK_UP);
      Check ("minute up 2", Main_Window.Duration_Edit.Data, 60.0);
      Key_Stroke (VK_LEFT);  -- hour
      Key_Stroke (VK_DOWN);
      Check ("hour down past 0", Main_Window.Duration_Edit.Data, 60.0);

      Finish;

   exception
   when others =>
      Finish;
      raise;
   end Test_1;

   procedure Test_Recommended_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Recommended_Size
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Types.AUnit;
      Font : GWindows.Drawing_Objects.Font_Type;
   begin
      Start;

      Check
        ("default",
         Computed => Recommended_Size (Main_Window.Duration_Edit),
         Expected => (Width => 42, Height => 22));

      GWindows.Drawing_Objects.Create_Stock_Font
        (Font, GWindows.Drawing_Objects.ANSI_Variable_Width);
      Set_Font (Main_Window.Duration_Edit, Font);

      --  Force redisplay
      Duration
        (Main_Window.Duration_Edit, Duration (Main_Window.Duration_Edit));
      Background.Test_Delay;

      Check
        ("ANSI variable",
         Computed => Recommended_Size (Main_Window.Duration_Edit),
         Expected => (Width => 33, Height => 19));

      Finish;
   exception
   when others =>
      Finish;
      raise;
   end Test_Recommended_Size;

   procedure Test_Background_Color
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Background_Color
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Colors.AUnit;
      use GWindows.Colors;
      use GWindows.Drawing;
      use GWindows.Types.AUnit;

   begin
      Start;

      declare
         Canvas : Canvas_Type;
      begin
         Test_Duration_Edit_Aux.Get_Canvas (Main_Window, Canvas);

         Check
           ("default background",
            Point (Canvas, 15, 15),
            White);
      end;

      Background_Color (Main_Window.Duration_Edit, Green);

      --  Force redisplay
      Duration
        (Main_Window.Duration_Edit, Duration (Main_Window.Duration_Edit));

      Background.Test_Delay;

      declare
         Canvas : Canvas_Type;
      begin
         Test_Duration_Edit_Aux.Get_Canvas (Main_Window, Canvas);
         Check
           ("green background",
            Point (Canvas, 15, 15),
            Green);
      end;

      Finish;
   exception
   when others =>
      Finish;
      raise;
   end Test_Background_Color;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("GWindows.GControls.Duration.Edit.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      case T.Debug_Level is
         when 1 =>
            Register_Routine
              (T, Test_1'Access, "debug");
         when others =>
            Register_Routine (T, Test_1'Access, "Test_1");
            Register_Routine
              (T, Test_Recommended_Size'Access, "Test_Recommended_Size");
            Register_Routine
              (T, Test_Background_Color'Access, "Test_Background_Color");
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

end GWindows.GControls.Duration.Edit.Test;
