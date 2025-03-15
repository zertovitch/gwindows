------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . I M A G E _ L I S T S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2021 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
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

   type Color_Option_Type is
     (Default,
      Depth_4, Depth_8, Depth_16, Depth_24, Depth_32,
      --  Specific color depths.
      Create_DIB_Section,
      --  "Creates DIB section bitmap rather than a compatible bitmap.
      --   This flag is useful for loading a bitmap without mapping it
      --   to the colors of the display device."
      Copy_From_Resource
      --  "Tries to reload an icon or cursor resource from the original
      --   resource file rather than simply copying the current image."
      );

   procedure Create (List          : in out Image_List_Type;
                     Width, Height : in     Positive;
                     Initial_Size  : in     Positive;
                     Grow_By       : in     Natural           := 1;
                     Color_Option  : in     Color_Option_Type := Default);

   procedure Create
     (List          : in out Image_List_Type;
      Name          : in     GString;
      Width         : in     Positive;
      Grow_By       : in     Natural           := 1;
      Color_Option  : in     Color_Option_Type := Create_DIB_Section);
   --  Create an image list from a resource use #XXX for numeric resources
   --  Width is the size of each image in a long bitmap, height is height of
   --  bitmap

   procedure Create_From_File
     (List         : in out Image_List_Type;
      File_Name    : in     GString;
      Width        : in     Positive;
      Grow_By      : in     Natural           := 1;
      Color_Option : in     Color_Option_Type := Default);
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
   --  Sets image for drag and drop.
   --  In response to subsequent On_Mouse_Move callbacks,
   --  you can move the drag image by using the Drag_Move procedure below.
   --
   --  Index is index of image to use
   --  X, Y is hotspot location

   procedure End_Drag;

   procedure Drag_Cursor_Image (List  : in out Image_List_Type;
                                Index : in     Integer         := 0;
                                X, Y  : in     Integer         := 0);
   --  Creates a new drag image by combining the specified
   --  image (typically a mouse cursor image) with the current drag image.
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
