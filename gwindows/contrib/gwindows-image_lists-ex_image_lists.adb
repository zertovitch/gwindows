------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.image_lists.ex_image_lists                     --
--                                                                          --
--                                 B o d y                                  --
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

with GWindows.GStrings;
with GWindows.Internal;

package body GWindows.Image_Lists.Ex_Image_Lists is

   function Get_Color_Flag (Color_Depth : in Color_Depth_Type) return Natural;

   ILC_COLOR4              : constant := 4;
   ILC_COLOR8              : constant := 8;
   ILC_COLOR16             : constant := 16#10#;
   ILC_COLOR24             : constant := 16#18#;
   ILC_COLOR32             : constant := 16#20#;

   --------------
   -- CreateEx --
   --------------

   procedure CreateEx (List          : in out Ex_Image_List_Type;
                       Width, Height : in     Positive;
                       Initial_Size  : in     Positive;
                       Grow_By       : in     Natural         := 1;
                       Color_Depth  : in Color_Depth_Type    := Depth_32)
   is
      function ImageList_Create
        (CX    : Positive := Width;
         XY    : Positive := Height;
         FLAGS : Natural  := 0;
         Init  : Positive := Initial_Size;
         Grow  : Natural  := Grow_By)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_Create, "ImageList_Create");
   begin
      Handle (List, ImageList_Create (FLAGS => Get_Color_Flag (Color_Depth)));
   end CreateEx;


   procedure CreateEx (List    : in out Ex_Image_List_Type;
                       Name    : in     GString;
                       Width   : in     Positive;
                       Grow_By : in     Natural         := 1;
                       Color_Depth  : in Color_Depth_Type    := Depth_32)
   is
      C_Text : Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (Name));

      function ImageList_LoadImage
        (HINST : Interfaces.C.long      := GWindows.Internal.Current_hInstance;
         Name  : Interfaces.C.char_array := C_Text;
         CX    : Positive                := Width;
         Grow  : Natural                 := Grow_By;
         CREF  : Interfaces.C.unsigned   := 16#FF000000#;
         UT    : Integer                 := 0;
         FLAGS : Natural                 := 0)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_LoadImage, "ImageList_LoadImage");
   begin
      Handle (List,
              ImageList_LoadImage (FLAGS => Get_Color_Flag (Color_Depth)));
   end CreateEx;

   --------------------
   -- body functions --
   --------------------

   --------------------
   -- Get_Color_Flag --
   --------------------

   function Get_Color_Flag (Color_Depth : in Color_Depth_Type) return Natural
   is
   begin
      case Color_Depth is
         when Depth_4 =>
            return ILC_COLOR4;
         when Depth_8 =>
            return ILC_COLOR8;
         when Depth_16 =>
            return ILC_COLOR16;
         when Depth_24 =>
            return ILC_COLOR24;
         when Depth_32 =>
            return ILC_COLOR32;
         when others =>
            return 0;
      end case;
   end Get_Color_Flag;

end GWindows.Image_Lists.Ex_Image_Lists;
