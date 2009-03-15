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
with GWindows.GStrings;
with GWindows.Packing_Boxes;
with GWindows.Testing.Generic_Task;
with GWindows.Types.AUnit;
with GWindows.Windows;
with Test_Edit_Boxes_Aux;
package body GWindows.Edit_Boxes.Test is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is
      use Test_Edit_Boxes_Aux;
      Common_Window    : Top_Window_Type renames Top_Window_Type (Window.all);
      Single_Line_Edit : constant Edit_Box_Access := new Edit_Box_Type;

      Multi_Line_Edit  : constant Multi_Line_Edit_Box_Access :=
        new Multi_Line_Edit_Box_Type;
   begin
      Create (Common_Window, Width => 400, Height => 400);

      Packing_Direction (Common_Window, GWindows.Packing_Boxes.Vertical);

      Common_Window.Single_Line_Edit :=
        Pointer_To_Edit_Box_Class (Single_Line_Edit);

      Create
        (Single_Line_Edit.all, Parent => Common_Window, Text => "Forty_Two");

      Size (Single_Line_Edit.all, Recommended_Size (Single_Line_Edit.all));

      Common_Window.Multi_Line_Edit :=
        Pointer_To_Edit_Box_Class (Multi_Line_Edit);

      Create
        (Multi_Line_Edit.all,
         Parent => Common_Window,
         Text   => GStrings.To_GString_From_String
           ("Forty_Two" & ASCII.CR & ASCII.LF & "Forty_Three"));

      Size (Multi_Line_Edit.all, Recommended_Size (Multi_Line_Edit.all));
   end Create;

   package Background is new GWindows.Testing.Generic_Task (Create);

   Main_Window : aliased Test_Edit_Boxes_Aux.Top_Window_Type;

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
        ("single line default",
         Computed => Size (Main_Window.Single_Line_Edit.all),
         Expected => (Width => 80, Height => 24));

      GWindows.Drawing_Objects.Create_Stock_Font
        (Font, GWindows.Drawing_Objects.ANSI_Variable_Width);
      Set_Font (Main_Window.Single_Line_Edit.all, Font);

      Size
        (Main_Window.Single_Line_Edit.all,
         Recommended_Size (Main_Window.Single_Line_Edit.all));
      Background.Test_Delay;

      Check
        ("single line ansi variable",
         Computed => Size (Main_Window.Single_Line_Edit.all),
         Expected => (Width => 63, Height => 21));

      Check
        ("multi line default",
         Computed => Size (Main_Window.Multi_Line_Edit.all),
         Expected => (Width => 174, Height => 40));

      GWindows.Drawing_Objects.Create_Stock_Font
        (Font, GWindows.Drawing_Objects.ANSI_Variable_Width);
      Set_Font (Main_Window.Multi_Line_Edit.all, Font);

      Size
        (Main_Window.Multi_Line_Edit.all,
         Recommended_Size (Main_Window.Multi_Line_Edit.all));
      Background.Test_Delay;

      Check
        ("multi line ansi variable",
         Computed => Size (Main_Window.Multi_Line_Edit.all),
         Expected => (Width => 138, Height => 34));

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
      return new String'("GWindows.Edit_Boxes.Test");
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

end GWindows.Edit_Boxes.Test;
