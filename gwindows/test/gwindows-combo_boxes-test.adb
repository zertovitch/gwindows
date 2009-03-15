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
with GWindows.Drawing_Objects;
with GWindows.Testing.Generic_Task;
with GWindows.Types.AUnit;
with GWindows.Windows;
with Test_Combo_Boxes_Aux;
package body GWindows.Combo_Boxes.Test is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is
      use Test_Combo_Boxes_Aux;
      Common_Window : Top_Window_Type renames Top_Window_Type (Window.all);
      Combo         : constant Drop_Down_List_Box_Access :=
        new Drop_Down_List_Box_Type;
   begin
      Create
        (Common_Window,
         Title  => "test_Combo_Boxes");

      Common_Window.Combo := Pointer_To_Combo_Box_Class (Combo);

      Create (Combo.all, Parent => Common_Window);

      Add (Common_Window.Combo.all, "Hello");
      Add (Common_Window.Combo.all, "Forty-Two");
      Add (Common_Window.Combo.all, "Forty-Three");
      Add (Common_Window.Combo.all, "Forty-Four");

      Size (Combo.all, Recommended_Size (Combo.all));
   end Create;

   package Background is new GWindows.Testing.Generic_Task (Create);

   Main_Window : aliased Test_Combo_Boxes_Aux.Top_Window_Type;

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
         Computed => Size (Main_Window.Combo.all),
         Expected => (Width => 102, Height => 24));

      GWindows.Drawing_Objects.Create_Stock_Font
        (Font, GWindows.Drawing_Objects.ANSI_Variable_Width);
      Set_Font (Main_Window.Combo.all, Font);

      Size (Main_Window.Combo.all, Recommended_Size (Main_Window.Combo.all));
      Background.Test_Delay;

      Check
        ("ansi variable",
         Computed => Size (Main_Window.Combo.all),
         Expected => (Width => 79, Height => 21));

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
      return new String'("GWindows.Combo_Boxes.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Recommended_Size'Access, "Test_Recommended_Size");
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

end GWindows.Combo_Boxes.Test;
