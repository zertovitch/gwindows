------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . T Y P E S                         --
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

with System;
with Ada.Unchecked_Conversion;

package GWindows.Types is

   type Handle is new System.Address;
   Null_Handle : constant Handle := Handle (System.Null_Address);
   type Wparam is mod 2 ** Standard'Address_Size;
   type Lparam is new Wparam;
   type Lresult is new Wparam;

   function To_Handle (I : Integer) return Handle;
   function To_Handle (I : Interfaces.C.long) return Handle;
   function To_Handle is new Ada.Unchecked_Conversion (Lparam, Handle);
   function To_Handle is new Ada.Unchecked_Conversion (Wparam, Handle);
   function To_Lresult is new Ada.Unchecked_Conversion (Handle, Lresult);
   function To_Wparam (I : Integer) return Wparam;
   function To_Lparam (I : Integer) return Lparam;
   function To_Integer (Result : Lresult) return Integer;

   type Point_Type is
      record
         X, Y : Integer;
      end record;

   function "+" (Left, Right : in Point_Type) return Point_Type;
   function "-" (Left, Right : in Point_Type) return Point_Type;

   type Point_Array_Type is
     array (Positive range <>) of Point_Type;

   type Rectangle_Type is
      record
         Left, Top, Right, Bottom : Integer;
      end record;

   procedure Right_Bottom (Rect : in out Rectangle_Type;
                           Point : in Point_Type);
   --  Set Rect.Right := Point.X, Rect.Bottom := Point.Y

   function Right_Bottom (Rect : in Rectangle_Type) return Point_Type;
   --  return (X => Rect.Right, Y => Rect.Bottom)

   procedure Left_Top (Rect : in out Rectangle_Type; Point : in Point_Type);
   --  Set Rect.Left := Point.X, Rect.Top := Point.Y

   function Left_Top (Rect : in Rectangle_Type) return Point_Type;
   --  return (X => Rect.Left, Y => Rect.Top)

   pragma Inline (Right_Bottom, Left_Top);

   function Inside (Point : in Point_Type; Rect : in Rectangle_Type)
                   return Boolean;
   --  Returns True if Point is inside or just touching Rect.

   type Size_Type is
      record
         Width, Height : Integer;
      end record;

   function "+" (Left, Right : in Size_Type) return Size_Type;
   function "-" (Left, Right : in Size_Type) return Size_Type;

   function "-" (Left, Right : in Point_Type) return Size_Type;

   function "+" (Left : in Point_Type; Right : in Size_Type) return Point_Type;
   function "+" (Left : in Size_Type; Right : in Point_Type) return Point_Type;

   function Size (Rectangle : in Rectangle_Type) return Size_Type;

   function Max (Left, Right : in Size_Type) return Size_Type;
   --  component-wise Max

end GWindows.Types;
