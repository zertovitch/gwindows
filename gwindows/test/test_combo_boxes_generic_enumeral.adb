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
with GWindows.Combo_Boxes.Generic_Enumeral;
with GWindows.Testing.Generic_Task;
with GWindows.Windows;
with Test_Combo_Boxes_Aux;
package body Test_Combo_Boxes_Generic_Enumeral is

   type Muppets_Type is (Ernie, Bernie, Oscar);
   --  Note that when these are put in the combo box, they are sorted
   --  in alphabetical order. The combo box index is one based.
   --  Enum   'pos   index
   --  Ernie   0     2
   --  Bernie  1     1
   --  Oscar   2     3

   procedure Check is new GWindows.Testing.Gen_Check_Discrete (Muppets_Type);

   package Muppets_Combo_Boxes is new GWindows.Combo_Boxes.Generic_Enumeral
     (Muppets_Type);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is
      use Test_Combo_Boxes_Aux;
      Common_Window : Top_Window_Type renames Top_Window_Type (Window.all);
      Muppet_Box    : constant Muppets_Combo_Boxes.Combo_Box_Access :=
        new Muppets_Combo_Boxes.Combo_Box_Type;
   begin
      Create
        (Common_Window,
         Title  => "test_combo_box_generic_enumeral");

      Common_Window.Combo :=
        GWindows.Combo_Boxes.Pointer_To_Combo_Box_Class (Muppet_Box);

      Muppets_Combo_Boxes.Create
        (Muppets_Combo_Boxes.Combo_Box_Type (Common_Window.Combo.all),
         Parent => Common_Window);
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

   procedure Nominal
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class);
   procedure Nominal
     (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Start;

      declare
         use Muppets_Combo_Boxes;
         Muppet_Box : Combo_Box_Type renames
           Combo_Box_Type (Main_Window.Combo.all);
      begin
         Current (Muppet_Box, Bernie);
         Background.Test_Delay; --  let user see selection
         Check ("1", Current (Muppet_Box), Bernie);

         Current (Muppet_Box, Oscar);
         Background.Test_Delay; --  let user see selection
         Check ("2", Current (Muppet_Box), Oscar);

         Current (Muppet_Box, Ernie);
         Background.Test_Delay; --  let user see selection
         Check ("3", Current (Muppet_Box), Ernie);
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
      return new String'("Test_Combo_Boxes_Generic_Enumeral");
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

end Test_Combo_Boxes_Generic_Enumeral;
