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
with GWindows.Windows;
with Test_Packing_Boxes_Aux;
package body Test_Packing_Boxes is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is begin
      Test_Packing_Boxes_Aux.Create
        (Test_Packing_Boxes_Aux.Top_Window_Type (Window.all),
         Top    => 20,
         Left   => 0,
         Width  => 190,
         Height => 190,
         Title  => "test_packing_boxes");
   end Create;

   Main_Window : aliased Test_Packing_Boxes_Aux.Top_Window_Type;

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
   begin
      Start;

      Test_Packing_Boxes_Aux.Client_Area_Size
        (Main_Window, Width => 190, Height => 190);

      Background.Test_Delay;

      Test_Packing_Boxes_Aux.Check
        ("initial",
         Computed           => Main_Window,
         Expected_Vbox_Size =>
           (Width           => 190,
            Height          => 190),
         Expected_Button_1  =>
           (Left            => 10,
            Top             => 5,
            Right           => 60,
            Bottom          => 25),
         Expected_Hbox      =>
           (Left            => 10,
            Top             => 30,
            Right           => 140,
            Bottom          => 50),
         Expected_Button_2  =>
           (Left            => 6,
            Top             => 3,
            Right           => 56,
            Bottom          => 23),
         Expected_Button_3  =>
           (Left            => 59,
            Top             => 3,
            Right           => 119,
            Bottom          => 23));

      GWindows.Packing_Boxes.Fill_Width (Main_Window.Hbox, True);
      GWindows.Packing_Boxes.Pack (Main_Window.Hbox);
      Background.Test_Delay;

      --  FIXME: Button_2, Button_3 now occupy equal space, even
      --  though Button_3 needs, and initially had, more space.

      Test_Packing_Boxes_Aux.Check
        ("Hbox fill width",
         Computed           => Main_Window,
         Expected_Vbox_Size =>
           (Width           => 190,
            Height          => 190),
         Expected_Button_1  =>
           (Left            => 10,
            Top             => 5,
            Right           => 60,
            Bottom          => 25),
         Expected_Hbox      =>
           (Left            => 10,
            Top             => 30,
            Right           => 140,
            Bottom          => 50),
         Expected_Button_2  =>
           (Left            => 6,
            Top             => 3,
            Right           => 60,
            Bottom          => 23),
         Expected_Button_3  =>
           (Left            => 63,
            Top             => 3,
            Right           => 117,
            Bottom          => 23));

      Test_Packing_Boxes_Aux.Fill_Height (Main_Window, True);
      Test_Packing_Boxes_Aux.Pack (Main_Window);
      Background.Test_Delay;

      Test_Packing_Boxes_Aux.Check
        ("Vbox fill Height",
         Computed           => Main_Window,
         Expected_Vbox_Size =>
           (Width           => 190,
            Height          => 190),
         Expected_Button_1  =>
           (Left            => 10,
            Top             => 5,
            Right           => 60,
            Bottom          => 87),
         Expected_Hbox      =>
           (Left            => 10,
            Top             => 92,
            Right           => 140,
            Bottom          => 174),
         Expected_Button_2  =>
           (Left            => 6,
            Top             => 3,
            Right           => 60,
            Bottom          => 23),
         Expected_Button_3  =>
           (Left            => 63,
            Top             => 3,
            Right           => 117,
            Bottom          => 23));

      Background.Move_To ((30, 191)); --  bottom border
      Background.Drag_To ((30, 171));
      Background.Test_Delay;

      --  Extra space should be equally allocated to Button_1, Hbox;
      --  each should shrink vertically by 10. Hbox is not filled
      --  vertically, so Button_2, Button_3 are not affected.

      Test_Packing_Boxes_Aux.Check
        ("Vbox drag vertical",
         Computed           => Main_Window,
         Expected_Vbox_Size =>
           (Width           => 190,
            Height          => 170),
         Expected_Button_1  =>
           (Left            => 10,
            Top             => 5,
            Right           => 60,
            Bottom          => 77),
         Expected_Hbox      =>
           (Left            => 10,
            Top             => 82,
            Right           => 140,
            Bottom          => 154),
         Expected_Button_2  =>
           (Left            => 6,
            Top             => 3,
            Right           => 60,
            Bottom          => 23),
         Expected_Button_3  =>
           (Left            => 63,
            Top             => 3,
            Right           => 117,
            Bottom          => 23));

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

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Packing_Boxes");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_1'Access, "Test_1");
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

end Test_Packing_Boxes;
