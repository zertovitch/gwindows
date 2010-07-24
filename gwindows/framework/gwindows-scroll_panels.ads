------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . S C R O L L _ P A N E L S               --
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

with GWindows.Base;
with GWindows.Windows;
with GWindows.Panels;

package GWindows.Scroll_Panels is

   -------------------------------------------------------------------------
   --  Inside_Panel_Type
   -------------------------------------------------------------------------
   --  This is the type of the panel contained in the Scroll Panel

   type Inside_Panel_Type is
     new GWindows.Panels.Panel_Type with null record;

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type
   -------------------------------------------------------------------------
   --  Scroll_Panel_Type can also be used to create a regular window that
   --  scrolls its contents which are placed on the inside panel

   type Scroll_Panel_Type is
     new GWindows.Windows.Window_Type with
      record
         Panel : Inside_Panel_Type;
      end record;
   type Scroll_Panel_Access is access all Scroll_Panel_Type;
   type Pointer_To_Scroll_Panel_Class is access all Scroll_Panel_Type'Class;

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out Scroll_Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create scroll panel control

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Methods
   -------------------------------------------------------------------------

   procedure Panel_Size (Window : in out Scroll_Panel_Type;
                         Width  : in     Integer;
                         Height : in     Integer);
   --  Using this method to set the panel size will insure that scroll bars
   --  are updated if needed on the Scroll Panel. You can also set the
   --  size of the inside panel directly, but the scroll bars will not
   --  be adjusted for the Scroll Panel until the next On_Size event is
   --  received.

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Create (Window : in out Scroll_Panel_Type);
   --  Handles creation of internal panel

   procedure On_Vertical_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles vertical scroll

   procedure On_Horizontal_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles horizontal scroll

   procedure On_Size (Window : in out Scroll_Panel_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Handles size changes

   procedure On_Destroy (Window : in out Inside_Panel_Type);
   --  Handles being destroyed by contained Cancel_Button_Type's

end GWindows.Scroll_Panels;
