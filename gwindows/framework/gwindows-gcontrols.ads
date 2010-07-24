------------------------------------------------------------------------------
--                                                                          --
--                   GWINDOWS - Ada 95 RAD GUI Framework                    --
--                                                                          --
--                   G W I N D O W S . G C O N T R O L S                    --
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

with GWindows.Windows;
with GWindows.Base;

package GWindows.GControls is

   -------------------------------------------------------------------------
   --  GControl_Type
   -------------------------------------------------------------------------
   --  GControl_Type is the parent Class of all GWindows GUI Widgets that
   --  are completely based on GWindows and therefore platform independant.
   --  The base GControl implements nothing but a blank panel and is a
   --  child of the Window_Type class. The Text parameter sets the text
   --  property for the control, but there is no GUI representation of that
   --  text property.

   type GControl_Type is new GWindows.Windows.Window_Type with private;
   type GControl_Access is access all GControl_Type;
   type Pointer_To_GControl_Class is access all GControl_Type'Class;

   -------------------------------------------------------------------------
   --  GControl_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out GControl_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      All_Keys   : in     Boolean;
      Container  : in     Boolean;
      Show       : in     Boolean;
      Is_Dynamic : in     Boolean);
   --  Create GControl

   -------------------------------------------------------------------------
   --  GControl_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   --  GControl_Type is a child of GWindow_Type for that reason the
   --  create methods of GWindow_Type are also available to
   --  GControl_Types. These create methods should not be used even
   --  though they may be functional. The same behavior can be created
   --  by creating the GControl_Type or any of its children as a child
   --  of a window and setting the Dock property of the control to
   --  Fill.

private

   type GControl_Type is new GWindows.Windows.Window_Type with null record;

end GWindows.GControls;
