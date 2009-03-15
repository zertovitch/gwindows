--  Abstract :
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

with GWindows.Base;
with GWindows.Events;
package body Test_Duration_Edit_Aux is

   procedure On_Duration_Edit_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure On_Duration_Edit_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is begin
      GWindows.GControls.Duration.Edit.Duration
        (GWindows.GControls.Duration.Edit.Duration_Edit_Type (Window), 60.0);
   end On_Duration_Edit_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Top_Window_Type) is
   begin
      On_Destroy_Handler (Window, GWindows.Events.Do_End_Loop'Access);

      GWindows.GControls.Duration.Edit.On_Create_Handler
        (Window.Duration_Edit, On_Duration_Edit_Create'Access);

      GWindows.GControls.Duration.Edit.Create (Window.Duration_Edit, Window);

   end On_Create;

end Test_Duration_Edit_Aux;
