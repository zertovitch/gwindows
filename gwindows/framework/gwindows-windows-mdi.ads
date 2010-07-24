------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . W I N D O W S . M A I N                 --
--                                                                          --
--                                 S p e c                                  --
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

with GWindows.Menus;

package GWindows.Windows.MDI is

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type
   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type is a top level MDI window that when closed will
   --  also close the current message loop

   type MDI_Main_Window_Type is new Window_Type with private;
   type MDI_Main_Window_Access is access all MDI_Main_Window_Type;
   type Pointer_To_MDI_Main_Window_Class is
     access all MDI_Main_Window_Type'Class;

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Properties
   -------------------------------------------------------------------------

   procedure MDI_Menu (Window      : in out MDI_Main_Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive);
   --  Sets the MDI Top window menu and uses the sub menu Window_Menu for
   --  MDI window list

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Properties
   -------------------------------------------------------------------------

   procedure MDI_Close_All (Window : in out MDI_Main_Window_Type);
   --  Closes all MDI child windows

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Destroy (Window : in out MDI_Main_Window_Type);
   --  Handles closing down the message loop when the window is closed
   --  and destroying the menu

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type
   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type is a top level MDI window that when closed will
   --  also close the current message loop

   type MDI_Child_Window_Type is new Window_Type with private;
   type MDI_Child_Window_Access is access all MDI_Child_Window_Type;
   type Pointer_To_MDI_Child_Window_Class is
     access all MDI_Child_Window_Type'Class;

   procedure MDI_Menu (Window      : in out MDI_Child_Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive);
   --  Sets the MDI Top window menu and uses the sub menu Window_Menu for
   --  MDI window list

   procedure Activate (Window : in out MDI_Child_Window_Type);
   --  Make this the active MDI window

   -------------------------------------------------------------------------
   --  MDI_Child_Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_MDI_Activate (Window : in out MDI_Child_Window_Type);
   procedure On_MDI_Deactivate (Window : in out MDI_Child_Window_Type);
   --  Handles switching menus

   procedure On_Destroy (Window : in out MDI_Child_Window_Type);
   --  Handles destroying the menu

private

   type MDI_Main_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         Top_Menu             : GWindows.Menus.Menu_Type;
         Window_Menu_Location : Positive;
      end record;

   type MDI_Child_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         Child_Menu             : GWindows.Menus.Menu_Type;
         Window_Menu_Location : Positive;
      end record;

end GWindows.Windows.MDI;
