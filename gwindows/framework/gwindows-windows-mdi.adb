------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . W I N D O W S . M A I N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------
--  Window types to ease in the creation of Multi Document Interface
--  Applications

with GWindows.Application;

package body GWindows.Windows.MDI is

   --------------
   -- MDI_Menu --
   --------------

   procedure MDI_Menu
     (Window      : in out MDI_Main_Window_Type;
      Menu        : in     GWindows.Menus.Menu_Type;
      Window_Menu : in     Positive)
   is
   begin
      Window.Top_Menu := Menu;
      Window.Window_Menu_Location := Window_Menu;
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Window), Menu, Window_Menu);
   end MDI_Menu;

   --------------
   -- MDI_Menu --
   --------------

   procedure MDI_Menu
     (Window      : in out MDI_Child_Window_Type;
      Menu        : in     GWindows.Menus.Menu_Type;
      Window_Menu : in     Positive)
   is
   begin
      Window.Child_Menu := Menu;
      Window.Window_Menu_Location := Window_Menu;
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Controlling_Parent (Window).all),
         Menu,
         Window_Menu);
   end MDI_Menu;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out MDI_Main_Window_Type) is
   begin
      GWindows.Menus.Destroy_Menu (Window.Top_Menu);
      GWindows.Application.End_Application;
   end On_Destroy;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out MDI_Child_Window_Type) is
   begin
      GWindows.Menus.Destroy_Menu (Window.Child_Menu);
   end On_Destroy;

   ---------------------
   -- On_MDI_Activate --
   ---------------------

   procedure On_MDI_Activate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent_Win : constant Window_Access :=
        Window_Access (Controlling_Parent (Window));
   begin
      GWindows.Windows.MDI_Menu
        (Parent_Win.all, Window.Child_Menu, Window.Window_Menu_Location);
   end On_MDI_Activate;

   -----------------------
   -- On_MDI_Deactivate --
   -----------------------

   procedure On_MDI_Deactivate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent_Win : constant MDI_Main_Window_Access :=
        MDI_Main_Window_Access (Controlling_Parent (Window));
   begin
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Access (Parent_Win).all,
         Parent_Win.Top_Menu, Parent_Win.Window_Menu_Location);
   end On_MDI_Deactivate;

   -------------------
   -- MDI_Close_All --
   -------------------

   procedure MDI_Close_All (Window : in out MDI_Main_Window_Type)
   is
   begin
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Window),
         Window.Top_Menu,
         Window.Window_Menu_Location);
      GWindows.Windows.MDI_Close_All (GWindows.Windows.Window_Type (Window));
   end MDI_Close_All;

   --------------
   -- Activate --
   --------------

   procedure Activate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent : GWindows.Windows.Window_Type renames
        GWindows.Windows.Window_Type (Controlling_Parent (Window).all);
   begin
      GWindows.Windows.MDI_Active_Window (Parent, Window);
   end Activate;

end GWindows.Windows.MDI;
