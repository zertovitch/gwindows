------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.image_lists.ex_image_lists                     --
--                                                                          --
--                                 B o d y                                  --
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
      C_Text : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (Name));

      function ImageList_LoadImage
        (HINST : GWindows.Types.Handle   := GWindows.Internal.Current_hInstance;
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
      end case;
   end Get_Color_Flag;

end GWindows.Image_Lists.Ex_Image_Lists;
