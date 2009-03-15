--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2004 David Botton.  All Rights Reserved.
--
--  Windex is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. Windex is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with Windex; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  Windex, or you link Windex object files with other files to
--  produce an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Text_IO; use Ada.Text_IO;
with GWindows.Application;
with GWindows.Key_States;
with GWindows.Testing.Events;
package body GWindows.Testing.Generic_Task is

   User_Window_Client_Origin : GWindows.Types.Point_Type;

   ----------
   --  Background task

   procedure Debug_Put (Level : in Integer; Message : in String);

   procedure Debug_Put (Level : in Integer; Message : in String)
   is begin
      if Debug_Level >= Level then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Debug_Put;

   task body Background_Task
   is begin
      select
         accept Init;
      or
         terminate;
      end select;

      loop
         select
            accept Create_Window
              (Window : in GWindows.Windows.Pointer_To_Window_Class)
            do
               Debug_Put (1, "App_Task: Creating User Window");
               Create_User_Window (Window);
               Debug_Put (1, "App_Task: Showing User Window");
               GWindows.Windows.Show (Window.all);

               User_Window_Client_Origin := GWindows.Windows.Point_To_Desktop
                 (Window.all, (0, 0));

            end Create_Window;
         or
            --  exit loop
            terminate;
         end select;

         accept Run
         do
            Debug_Put (1, "App_Task: Running Message Loop");
         end Run;

         GWindows.Application.Message_Loop;
         Debug_Put (1, "App_Task: Message loop exited");

         accept Wait_Shutdown;
      end loop;
   end Background_Task;

   procedure Test_Delay
   is begin
      delay Test_Delay_Time;
   end Test_Delay;

   ----------
   --  Events

   procedure Close
   is begin
      GWindows.Testing.Events.Alt_Key_Stroke (GWindows.Key_States.VK_F4);
   end Close;

   function Client_Origin return GWindows.Types.Point_Type
   is begin
      return User_Window_Client_Origin;
   end Client_Origin;

   procedure Move_To (Point : in GWindows.Types.Point_Type)
   is
      use type GWindows.Types.Point_Type;
   begin
      GWindows.Testing.Events.Mouse_Event
         (Move     => True,
          Absolute => True,
          Motion   => User_Window_Client_Origin + Point);
   end Move_To;

   procedure Drag_To (Point : in GWindows.Types.Point_Type)
   is
      use type GWindows.Types.Point_Type;
   begin
      GWindows.Testing.Events.Mouse_Event (Left_Down => True);
      Test_Delay;
      GWindows.Testing.Events.Mouse_Event
         (Move     => True,
          Absolute => True,
          Motion   => User_Window_Client_Origin + Point);
      GWindows.Testing.Events.Mouse_Event (Left_Up => True);
      Test_Delay;
   end Drag_To;

   procedure Click_Left
   is begin
      GWindows.Testing.Events.Mouse_Event (Left_Down => True);
      Test_Delay;
      GWindows.Testing.Events.Mouse_Event (Left_Up => True);
      Test_Delay;
   end Click_Left;

   procedure Double_Click_Left
   is begin
      GWindows.Testing.Events.Mouse_Event (Left_Down => True);
      GWindows.Testing.Events.Mouse_Event (Left_Up => True);
      GWindows.Testing.Events.Mouse_Event (Left_Down => True);
      Test_Delay;
      GWindows.Testing.Events.Mouse_Event (Left_Up => True);
      Test_Delay;
   end Double_Click_Left;

end GWindows.Testing.Generic_Task;
