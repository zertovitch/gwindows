------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . T Y P E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2023 David Botton                   --
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

with System;
with Ada.Unchecked_Conversion;

package GWindows.Types is

   type BYTE is new Interfaces.C.unsigned_char;
   type WORD is new Interfaces.C.unsigned_short;
   type DWORD is new Interfaces.C.unsigned;
   type DWORD_PTR is mod 2 ** Standard'Address_Size;
   --  From the Windows documentation:
   --    "An unsigned long type for pointer precision. Use when casting
   --     a pointer to a long type to perform pointer arithmetic. (Also
   --     commonly used for general 32-bit parameters that have been
   --     extended to 64 bits in 64-bit Windows.)"

   type Handle is new System.Address;
   Null_Handle : constant Handle := Handle (System.Null_Address);
   type Wparam is mod 2 ** Standard'Address_Size;
   type Lparam is new Wparam;
   type Lresult is new Wparam;
   type INT_PTR is range
     -(2 ** (Standard'Address_Size - 1)) ..
      (2 ** (Standard'Address_Size - 1)) - 1;
   for INT_PTR'Size use Standard'Address_Size;

   type LPTSTR is access all GChar_C;
   pragma No_Strict_Aliasing (LPTSTR);

   function To_Handle (I : Integer) return Handle;
   function To_Handle (I : Interfaces.C.long) return Handle;
   function To_Handle is new Ada.Unchecked_Conversion (Lparam, Handle);
   function To_Handle is new Ada.Unchecked_Conversion (Wparam, Handle);
   function To_Lresult is new Ada.Unchecked_Conversion (Handle, Lresult);
   function To_Lresult is new Ada.Unchecked_Conversion (INT_PTR, Lresult);
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
