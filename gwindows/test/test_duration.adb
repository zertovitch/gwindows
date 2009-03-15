--  Abstract:
--
--  See spec
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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with GWindows.GControls.Duration;
with GWindows.Testing.Generic_Task;
with GWindows.Windows;
with Test_Duration_Aux;
package body Test_Duration is

   procedure Create (Window : access GWindows.Windows.Window_Type'Class);

   procedure Create (Window : access GWindows.Windows.Window_Type'Class)
   is begin
      Test_Duration_Aux.Create
        (Test_Duration_Aux.Top_Window_Type (Window.all),
         Top    => 20,
         Left   => 0,
         Width  => 190,
         Height => 190,
         Title  => "test_duration");
   end Create;

   package Background is new GWindows.Testing.Generic_Task (Create);

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

   Main_Window : aliased Test_Duration_Aux.Top_Window_Type;

   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin

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

      Background.Background_Task.Create_Window (Main_Window'Access);

      Background.Background_Task.Run;
      Background.Test_Delay;

      Test_Duration_Aux.Check ("", Main_Window, "01:00");

      GWindows.GControls.Duration.Duration (Main_Window.Duration, 0.0);
      Test_Duration_Aux.Check ("", Main_Window, "00:00");

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
      return new String'("Test_Duration");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_1'Access, "Test_1");
   end Register_Tests;

end Test_Duration;
