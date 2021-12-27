------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.image_lists.ex_image_lists                     --
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
