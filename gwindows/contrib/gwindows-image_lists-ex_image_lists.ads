------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.image_lists.ex_image_lists                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------
--  You may set a color depth up to 32 bit when creating an
--    Extended Image List
--  Important for images with transparent background.
------------------------------------------------------------------------------

package GWindows.Image_Lists.Ex_Image_Lists is

   type Ex_Image_List_Type is new Image_List_Type with null record;
   pragma Obsolescent (Ex_Image_List_Type,
     "Color depths are now covered by Color_Option parameter " &
     "in Create* methods of parent type");

   type Color_Depth_Type is (Depth_4, Depth_8, Depth_16, Depth_24, Depth_32);

   procedure CreateEx (List          : in out Ex_Image_List_Type;
                       Width, Height : in     Positive;
                       Initial_Size  : in     Positive;
                       Grow_By       : in     Natural         := 1;
                       Color_Depth   : in Color_Depth_Type    := Depth_32);
   --  Create an empty image-list

   procedure CreateEx (List         : in out Ex_Image_List_Type;
                       Name         : in     GString;
                       Width        : in     Positive;
                       Grow_By      : in     Natural         := 1;
                       Color_Depth  : in Color_Depth_Type    := Depth_32);
   --  Create an image list from a resource use #XXX for numeric resources
   --  Width is the size of each image in a long bitmap, height is height of
   --  bitmap

end GWindows.Image_Lists.Ex_Image_Lists;
