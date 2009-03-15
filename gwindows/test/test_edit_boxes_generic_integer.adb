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
with GWindows.Edit_Boxes.Generic_Integer;
with GWindows.Key_States;
with GWindows.Testing.Events;
with GWindows.Testing.Generic_Task;
with GWindows.Windows;
with Test_Edit_Boxes_Aux;
package body Test_Edit_Boxes_Generic_Integer is

   type Test_Type is range 123 .. 1235;
   package Integer_Edit_Boxes is new GWindows.Edit_Boxes.Generic_Integer
     (Test_Type);

   procedure Check is new GWindows.Testing.Gen_Check_Discrete (Test_Type);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is
      use Test_Edit_Boxes_Aux;
      Common_Window : Top_Window_Type renames Top_Window_Type (Window.all);
      Integer_Box   : constant Integer_Edit_Boxes.Edit_Box_Access :=
        new Integer_Edit_Boxes.Edit_Box_Type;
   begin
      Create
        (Common_Window,
         Title  => "test_edit_box_generic_enumeral",
         Width  => 400,
         Height => 400);

      Common_Window.Single_Line_Edit :=
        GWindows.Edit_Boxes.Pointer_To_Edit_Box_Class (Integer_Box);

      Integer_Edit_Boxes.Create
        (Integer_Edit_Boxes.Edit_Box_Type (Common_Window.Single_Line_Edit.all),
         Width => 50,
         Parent => Common_Window);
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

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);
   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Testing.Events;
      use GWindows.Key_States;
   begin
      Start;

      declare
         use Integer_Edit_Boxes;
         Integer_Box : Edit_Box_Type renames
           Edit_Box_Type (Main_Window.Single_Line_Edit.all);
      begin
         --  Put focus in edit box, cursor at end
         Background.Move_To ((10, 10));
         Background.Click_Left;
         Key_Stroke (VK_END);

         Background.Test_Delay;
         Check ("0", Current (Integer_Box), 123);

         Key_Stroke ("4");
         Background.Test_Delay;
         Check ("1", Current (Integer_Box), 1234);

         Key_Stroke (VK_BACK);
         Background.Test_Delay;
         Check ("2", Current (Integer_Box), 123);

         Key_Stroke ("A"); --  Should be ignored
         Background.Test_Delay;
         Check ("3", Current (Integer_Box), 123);

         Key_Stroke (VK_BACK); --  Constraint_Error
         Background.Test_Delay;
         Check ("4", Current (Integer_Box), 123);

         --  WORKAROUND: Constraint_Error processing leaves caret
         --  before first digit; move to end.
         Key_Stroke (VK_END);

         Key_Stroke ("5");
         Background.Test_Delay;
         Check ("5", Current (Integer_Box), 1235);

         Key_Stroke ("5");  --  Constraint_Error
         Background.Test_Delay;
         Check ("6", Current (Integer_Box), 1235);

      end;
      Finish;
   exception
   when others =>
      Finish;
      raise;
   end Nominal;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Edit_Boxes_Generic_Integer");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
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

end Test_Edit_Boxes_Generic_Integer;
