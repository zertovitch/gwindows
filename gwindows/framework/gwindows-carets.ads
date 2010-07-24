------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C A R E T S                        --
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
with GWindows.Drawing_Objects;
with GWindows.Types;

package GWindows.Carets is

   --  IMORTANT:
   --  Carets should be created in the On_Focus event and destroyed in the
   --  On_Lost_Focus event

   procedure Create_Solid_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer                              := 0;
      Height : in     Integer                              := 0);
   --  Create a solid caret. If width and heigh are 0 the default size
   --  is used

   procedure Create_Gray_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer                              := 0;
      Height : in     Integer                              := 0);
   --  Create a gray caret. If width and heigh are 0 the default size
   --  is used

   procedure Create_Bitmap_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Bitmap : in     GWindows.Drawing_Objects.Bitmap_Type);
   --  Create a bitmap caret

   procedure Destroy_Caret;

   procedure Show_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Hide_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Set_Caret_Position (X, Y : Integer);

   function Get_Caret_Position return GWindows.Types.Point_Type;

end GWindows.Carets;
