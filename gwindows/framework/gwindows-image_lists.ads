------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . I M A G E _ L I S T S                  --
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

with Ada.Finalization;

with GWindows.Types;
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Base;

package GWindows.Image_Lists is

   type Image_List_Type is
     new Ada.Finalization.Limited_Controlled with private;
   procedure Initialize (List : in out Image_List_Type);
   procedure Finalize (List : in out Image_List_Type);
   --  Insures that image lists are properly destroyed, etc.

   procedure Create (List          : in out Image_List_Type;
                     Width, Height : in     Positive;
                     Initial_Size  : in     Positive;
                     Grow_By       : in     Natural         := 1);

   procedure Create (List    : in out Image_List_Type;
                     Name    : in     GString;
                     Width   : in     Positive;
                     Grow_By : in     Natural         := 1);
   --  Create an image list from a resource use #XXX for numeric resources
   --  Width is the size of each image in a long bitmap, height is height of
   --  bitmap

   procedure Create_From_File (List      : in out Image_List_Type;
                               File_Name : in     GString;
                               Width     : in     Positive;
                               Grow_By   : in     Natural         := 1);
   --  Create an image list from a bitmap file

   procedure Duplicate (In_List  : in     Image_List_Type;
                        Out_List :    out Image_List_Type);

   function Count (List : in Image_List_Type) return Integer;

   procedure Add (List   : in out Image_List_Type;
                  Bitmap : in     GWindows.Drawing_Objects.Bitmap_Type);

   procedure Add_Icon (List : in out Image_List_Type;
                       Icon : in     GWindows.Drawing_Objects.Icon_Type);

   procedure Get_Icon (List  : in     Image_List_Type;
                       Index : in     Natural;
                       Icon  :    out GWindows.Drawing_Objects.Icon_Type);

   procedure Replace_Icon (List  : in out Image_List_Type;
                           Index : in     Natural;
                           Icon  : in     GWindows.Drawing_Objects.Icon_Type);

   procedure Draw (List   : in out Image_List_Type;
                   Canvas : in out GWindows.Drawing.Canvas_Type'Class;
                   Index  : in     Natural;
                   X, Y   : in     Integer);

   procedure Destroy (List : in out Image_List_Type);

   procedure Handle (List   : in out Image_List_Type;
                     Handle : in     GWindows.Types.Handle);
   function Handle (List : in Image_List_Type) return GWindows.Types.Handle;

   --  Drag and Drop

   procedure Begin_Drag (List  : in out Image_List_Type;
                         Index : in     Integer         := 0;
                         X, Y  : in     Integer         := 0);
   --  Sets image for drag and drop
   --  Index is index of image to use
   --  X, Y is hotspot location

   procedure End_Drag;

   procedure Drag_Cursor_Image (List  : in out Image_List_Type;
                                Index : in     Integer         := 0;
                                X, Y  : in     Integer         := 0);
   --  Sets image to combine with drag image for drag and drop
   --  Index is index of image to use
   --  X, Y is hotspot location

   procedure Drag_Enter (Window : in out GWindows.Base.Base_Window_Type'Class;
                         X, Y   : in     Integer);
   --  Start dragging image at X, Y relative to Window (not just client area)

   procedure Drag_Leave (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  End dragging of image

   procedure Drag_Move (Window : in out GWindows.Base.Base_Window_Type'Class;
                        X, Y   : in     Integer);
   --  Drag image to X, Y relative to window

   procedure Drag_Show_Image;

   procedure Drag_Hide_Image;

private

   type Image_List_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         HIMAGELIST : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      end record;

end GWindows.Image_Lists;
