--  Abstract :
--
--  Generic window in a background task, with utilities for simulating user
--  input events, for testing application windows.
--
--  Copyright (C) 2004 David Botton.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this program, or you link this program object files with other
--  files to produce an executable, that does not by itself cause the
--  resulting executable to be covered by the GNU General Public
--  License. This exception does not however invalidate any other
--  reasons why the executable file might be covered by the GNU Public
--  License.

with GWindows.Types;
with GWindows.Windows;
generic
   with procedure Create_User_Window
     (Window : access GWindows.Windows.Window_Type'Class);
   --  Creat the Window to be tested.
package GWindows.Testing.Generic_Task is

   Debug_Level     : Natural  := 0;
   Test_Delay_Time : Duration := 0.1;
   --  time required to let background task process all messages

   Single_Click_Time : Duration := 0.3; --  0.2 too small
   --  Long enough, when added to Test_Delay_Time, that two mouse
   --  clicks won't be seen as a double click. This is acutally set by
   --  the user in the mouse control panel, but it's stored in the
   --  registry, and I'm not up to reading that yet.

   ----------
   --  Background task

   task Background_Task is
      pragma Storage_Size (4_000_000);

      entry Init;
      --  Task waits for Init, then waits for each of the following entries,
      --  in the order declared. Then loops back to wait for
      --  Create_Window, or terminate.
      --
      --  This allows several test cases to share a single background
      --  task.

      entry Create_Window
        (Window : in GWindows.Windows.Pointer_To_Window_Class);
      --  Calls Create_User_Window, then shows Window.

      entry Run;
      --  Run message loop.

      entry Wait_Shutdown;
      --  Unblocks after message loop is exited (normally by Window
      --  being destroyed), and background task is ready to terminate.
      --  Useful for just letting user run window to generate messages
      --  for debugging.

   end Background_Task;

   ----------
   --  Input events.
   --
   --  For Keyboard events and other mouse events, use
   --  GWindows.Test_Events.

   procedure Test_Delay;
   --  Delay for Test_Delay_Time. This lets the background window task
   --  process events.

   procedure Close;
   --  Safely close the window, assuming it has the focus.

   function Client_Origin return GWindows.Types.Point_Type;
   --  Return client origin of user window, in screen coordinates.

   procedure Move_To (Point : in GWindows.Types.Point_Type);
   --  Move mouse to Point, in user window coordinates.

   procedure Drag_To (Point : in GWindows.Types.Point_Type);
   --  Press left button, move to Point (in user window coordinates),
   --  release left button. Test delays after each step.

   procedure Click_Left;
   --  Click left mouse button at current position, then delay for
   --  Test_Delay_Time.

   procedure Double_Click_Left;
   --  Double click left mouse button at current position, then delay for
   --  Test_Delay_Time.

end GWindows.Testing.Generic_Task;
