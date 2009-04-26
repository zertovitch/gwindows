------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . S C R O L L _ B A R S                  --
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
with GWindows.Constants;

package GWindows.Scroll_Bars is

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type
   -------------------------------------------------------------------------

   type Scroll_Bar_Type is new GWindows.Base.Base_Window_Type with private;
   type Scroll_Bar_Access is access all Scroll_Bar_Type;
   type Pointer_To_Scroll_Bar_Class is access all Scroll_Bar_Type'Class;

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type - Creation Methods
   -------------------------------------------------------------------------

   type Scroll_Direction_Type is (Horizontal, Vertical);

   procedure Create
     (Bar        : in out Scroll_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Direction  : in     Scroll_Direction_Type;
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Scroll_Bar

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type - Properties
   -------------------------------------------------------------------------

   procedure Scroll_Range
     (Bar     : in out Scroll_Bar_Type;
      Minimum : in     Integer;
      Maximum : in     Integer);

   procedure Scroll_Maximum
     (Bar     : in out Scroll_Bar_Type;
      Maximum : in     Integer);

   function Scroll_Maximum
     (Bar : in Scroll_Bar_Type) return Integer;

   procedure Scroll_Minimum
     (Bar     : in out Scroll_Bar_Type;
      Minimum : in     Integer);

   function Scroll_Minimum
     (Bar : in Scroll_Bar_Type) return Integer;
   --  Scroll Range Property

   procedure Scroll_Position
     (Bar      : in out Scroll_Bar_Type;
      Position : in     Integer);

   function Scroll_Position
     (Bar : in Scroll_Bar_Type) return Integer;
   --  Scroll Position

   procedure Scroll_Page_Size
     (Bar  : in out Scroll_Bar_Type;
      Size : in     Natural);

   function Scroll_Page_Size
     (Bar : in Scroll_Bar_Type) return Natural;
   --  Scroll Page_Size

   function Scroll_Drag_Position
     (Bar : in Scroll_Bar_Type) return Integer;
   --  Scroll position during a Thumb_Drag scroll event

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Scroll_Handler
     (Bar     : in out Scroll_Bar_Type;
      Handler : in     GWindows.Base.Scroll_Event);
   procedure Fire_On_Scroll
     (Bar     : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type);
   --  Scroll thumb position is updated in On_Scroll first and then
   --  the On_Scroll_Handler is called

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Scroll
     (Bar     : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type);
   --  Called when scroll bar is moved
   --  Call base class to insure that scroll thumb position is updated

   -------------------------------------------------------------------------
   --  Scroll_Bar_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Horizontal_Scroll
     (Window  : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles horizontal scroll requests

   procedure On_Vertical_Scroll
     (Window  : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles vertical scroll requests

private

   type Scroll_Bar_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Scroll_Event : GWindows.Base.Scroll_Event := null;
      end record;

end GWindows.Scroll_Bars;
