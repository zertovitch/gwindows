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

with GWindows.Types;
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Base;
with GWindows.Windows;

package GWindows.Drawing_Panels is

   -------------------------------------------------------------------------
   --  Drawing_Canvas_Type
   -------------------------------------------------------------------------
   --  Drawing canvas is used by the Drawing_Panel_Type. It is the canvas
   --  type used for all painting on Drawing_Panel_Types.

   type Drawing_Canvas_Type is
     new GWindows.Drawing.Memory_Canvas_Type with private;

   procedure Finalize (Canvas : in out Drawing_Canvas_Type);
   --  Requests a redraw of Drawing_Panel when finalized
   --  Redraw (Window)

   -------------------------------------------------------------------------
   --  Drawing_Panel_Type
   -------------------------------------------------------------------------
   --  Drawing Panels are used as a sort of permanent canvas. It handles
   --  automaticly saving any drawing made on it to its internal memory
   --  bitmap and restoring it on any On_Paint events received

   type Drawing_Panel_Type is new GWindows.Windows.Window_Type with private;
   type Drawing_Panel_Access is access all Drawing_Panel_Type;
   type Pointer_To_Drawing_Panel_Class is access all Drawing_Panel_Type'Class;

   procedure Create
     (Window     : in out Drawing_Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Drawing_Panel_Type - Properties
   -------------------------------------------------------------------------

   procedure Auto_Resize (Window : in out Drawing_Panel_Type;
                          Value  : in     Boolean            := True);
   function Auto_Resize (Window : in Drawing_Panel_Type) return Boolean;
   --  If set to true, drawing canvas will automaticly be resized to
   --  size of window whenever a On_Size message is received
   --  This is true by default

   -------------------------------------------------------------------------
   --  Drawing_Panel_Type - Methods
   -------------------------------------------------------------------------

   procedure Get_Canvas (Window : in     Drawing_Panel_Type;
                         Canvas : in out Drawing_Canvas_Type'Class);
   --  Returns the internal memory canvas for drawing in memory.
   --  Once drawing is complete in memory, Redraw should be called.
   --  If you wish to have the new drawing redrawn immediately call
   --  Redraw (Window, Redraw_Now => True)

   procedure Resize_Canvas (Window : in out Drawing_Panel_Type;
                            Copy   : in     Boolean            := True);
   --  Redimensions the internal memory canvas to fit the current
   --  dimensions of the Drawing_Panel. If copy is true, as much of the
   --  old bitmap as will fit on the new one will be copied.

   procedure Resize_Canvas (Window : in out Drawing_Panel_Type;
                            Width  : in     Integer;
                            Height : in     Integer;
                            Copy   : in     Boolean            := True);
   --  Redimension the internal memory canvas to a set size. If Auto_Resize
   --  is true, this size will be reset on the next change in size of the
   --  Drawing_Panel. If copy is true, as much of the old bitmap as will
   --  fit on the new one will be copied.

   -------------------------------------------------------------------------
   --  Drawing_Panel_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Paint (Window : in out Drawing_Panel_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);
   --  Handles drawing border

   procedure On_Size (Window : in out Drawing_Panel_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Handles automaticly resizing canvas when window is resized

   procedure On_Create (Window : in out Drawing_Panel_Type);
   --  Handles creation of first internal bitmap

   procedure On_Erase_Background
     (Window : in out Drawing_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);
   --  Handles background erase to prevent draw of background
   --  If Auto_Resize is turned off and the Drawing Panel client area
   --  is greater then the size of the internal panel, this will create
   --  very strange results unless On_Erase_Background is overridden
   --  and calls the base Window_Type On_Erase_Background

private

   type Drawing_Panel_Type is new GWindows.Windows.Window_Type with
      record
         Bitmap        : GWindows.Drawing_Objects.Bitmap_Type;
         Memory_Canvas : GWindows.Drawing.Memory_Canvas_Type;
         Width         : Integer := 0;
         Height        : Integer := 0;
         Auto_Resize   : Boolean := True;
      end record;

   type Drawing_Canvas_Type is
     new GWindows.Drawing.Memory_Canvas_Type with
      record
         Parent : GWindows.Types.Handle;
      end record;

end GWindows.Drawing_Panels;
