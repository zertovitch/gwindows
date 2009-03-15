--  Abstract :
--
--  See spec
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
--

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with GWindows.Packing_Boxes;
with GWindows.Testing.Generic_Task;
with GWindows.Types.AUnit;
with GWindows.Windows;
with Test_Packing_Boxes_Autosize_Aux; use Test_Packing_Boxes_Autosize_Aux;
package body Test_Packing_Boxes_Autosize is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is begin
      Create
        (Top_Window_Type (Window.all),
         Title  => "test_packing_boxes_autosize");
   end Create;

   Main_Window : aliased Top_Window_Type;

   package Background is new GWindows.Testing.Generic_Task (Create);

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
   --  Tests

   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Types.AUnit;
      use type GWindows.Types.Size_Type;

      procedure Check_Initial_Layout (Label : in String);

      procedure Check_Initial_Layout (Label : in String)
      is begin
         Check
           (Label,
            Computed             => Main_Window,
            Expected_Client_Size =>
              (Width             => 203,
               Height            => 81),
            Expected_Button_1    =>
              (Left              => 10,
               Top               => 5,
               Right             => 74,
               Bottom            => 27),
            Expected_Hbox        =>
              (Left              => 10,
               Top               => 32,
               Right             => 183,
               Bottom            => 66),
            Expected_Button_2    =>
              (Left              => 6,
               Top               => 3,
               Right             => 70,
               Bottom            => 25),
            Expected_Button_3    =>
              (Left              => 73,
               Top               => 3,
               Right             => 161,
               Bottom            => 25));
      end Check_Initial_Layout;

   begin
      --  Test basic operation of Packing_Boxes.Recommended_Size

      Start;

      Check_Initial_Layout ("initial");

      Size (Main_Window, Size (Main_Window) + (20, 20));

      Check
        ("after grow",
         Recommended_Size (Main_Window),
         Calculate_New_Window_Size
           (Main_Window,
            Client_Area_Size => (203, 81)));
      --  Same size as initial layout above.

      Size (Main_Window, Recommended_Size (Main_Window));

      Check_Initial_Layout ("after grow, reset");

      Finish;

   exception
   when AUnit.Assertions.Assertion_Error =>
      Finish;
      raise;

   when E : others =>
      Finish;
      AUnit.Assertions.Assert
        (False, "exception " & Ada.Exceptions.Exception_Name (E));
   end Test_1;

   procedure Test_Fill (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Fill (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Types.AUnit;
      use type GWindows.Types.Size_Type;
      procedure Check_Initial_Layout (Label : in String);

      procedure Check_Initial_Layout (Label : in String)
      is begin
         Check
           (Label,
            Computed             => Main_Window,
            Expected_Client_Size =>
              (Width             => 227, --  Hbox + insets
               Height            => 93), --  2 * Hbox + insets + padding
            Expected_Button_1    =>
              (Left              => 10,
               Top               => 5,
               Right             => 207,
               Bottom            => 39),
            Expected_Hbox        =>
              (Left              => 10,
               Top               => 44,
               Right             => 207, --  2 * button_3 + padding + insets
               Bottom            => 78), --  button_2 + insets
            Expected_Button_2    =>
              (Left              => 6,
               Top               => 3,
               Right             => 94,
               Bottom            => 25),
            Expected_Button_3    =>
              (Left              => 97,
               Top               => 3,
               Right             => 185,
               Bottom            => 25));
      end Check_Initial_Layout;

   begin
      --  Test Packing_Boxes.Recommended_Size when Fill is True.

      Start;
      GWindows.Packing_Boxes.Fill_Width (Main_Window.Hbox, True);
      GWindows.Packing_Boxes.Fill_Height (Main_Window.Hbox, True);
      Fill_Width (Main_Window, True);
      Fill_Height (Main_Window, True);

      Size (Main_Window, Recommended_Size (Main_Window));

      Check_Initial_Layout ("Initial");

      Size (Main_Window, Size (Main_Window) + (20, 20));

      Check
        ("after grow",
         Computed             => Main_Window,
         Expected_Client_Size =>
           (Width             => 227 + 20,
            Height            => 93 + 20),
         Expected_Button_1    =>
           (Left              => 10,
            Top               => 5,
            Right             => 207 + 20,
            Bottom            => 39 + 10),
         Expected_Hbox        =>
           (Left              => 10,
            Top               => 44 + 10,
            Right             => 207 + 20,
            Bottom            => 78 + 20),
         Expected_Button_2    =>
           (Left              => 6,
            Top               => 3,
            Right             => 94 + 10,
            Bottom            => 25 + 10),
         Expected_Button_3    =>
           (Left              => 97 + 10,
            Top               => 3,
            Right             => 185 + 20,
            Bottom            => 25 + 10));

      Check
        ("after grow recommended_size",
         Recommended_Size (Main_Window),
         Calculate_New_Window_Size (Main_Window, (227, 93)));

      Size (Main_Window, Recommended_Size (Main_Window));

      Check_Initial_Layout ("after grow, reset");

      Finish;

   exception
   when AUnit.Assertions.Assertion_Error =>
      Finish;
      raise;

   when E : others =>
      Finish;
      AUnit.Assertions.Assert
        (False, "exception " & Ada.Exceptions.Exception_Name (E));
   end Test_Fill;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Packing_Boxes_Autosize");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_1'Access, "Test_1");
      Register_Routine (T, Test_Fill'Access, "Test_Fill");
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

end Test_Packing_Boxes_Autosize;
