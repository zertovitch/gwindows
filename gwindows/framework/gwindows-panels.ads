------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . P A N E L S                       --
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
with GWindows.Types;
with GWindows.Drawing;
with GWindows.Windows;

package GWindows.Panels is

   -------------------------------------------------------------------------
   --  Panel_Type
   -------------------------------------------------------------------------

   type Panel_Type is new GWindows.Windows.Window_Type with private;
   type Panel_Access is access all Panel_Type;
   type Pointer_To_Panel_Class is access all Panel_Type'Class;

   -------------------------------------------------------------------------
   --  Panel_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create panel control

   -------------------------------------------------------------------------
   --  Panel_Type - Properties
   -------------------------------------------------------------------------

   type Panel_Border_Type is (None, Sunken, Raised);

   procedure Border_Type (Panel  : in out Panel_Type;
                          Border : in     Panel_Border_Type);
   function Border_Type (Panel : in Panel_Type) return Panel_Border_Type;

   procedure Border_Thickness (Panel     : in out Panel_Type;
                               Thickness : in     Positive);
   function Border_Thickness (Panel : in Panel_Type) return Positive;

   -------------------------------------------------------------------------
   --  Panel_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Paint (Window : in out Panel_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);
   --  Handles drawing border

private

   type Panel_Type is new GWindows.Windows.Window_Type with
      record
         Panel_Border           : Panel_Border_Type := None;
         Panel_Border_Thickness : Positive := 1;
      end record;

end GWindows.Panels;
