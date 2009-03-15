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

with AUnit.Utilities;
with GWindows.Events;
with GWindows.GStrings;
package body Test_Duration_Aux is

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Top_Window_Type) is
   begin
      On_Destroy_Handler (Window, GWindows.Events.Do_End_Loop'Access);

      GWindows.GControls.Duration.Create
        (Window.Duration,
         Initial_Duration => 3600.0,
         Parent           => Window);
   end On_Create;

   -----------
   -- Check --
   -----------

   procedure Check
     (Label    : in String;
      Computed : in Top_Window_Type;
      Expected : in String)
   is
   begin
      AUnit.Utilities.Check
        (Label,
         GWindows.GStrings.To_String
           (GWindows.GControls.Duration.Text (Computed.Duration)),
         Expected);
   end Check;

end Test_Duration_Aux;
