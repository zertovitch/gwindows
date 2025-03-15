------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . T Y P E S                         --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

package body GWindows.Types is

   function To_Handle (I : Integer) return Handle is
      type Int is range -(2 ** (Standard'Address_Size - 1)) ..
                         (2 ** (Standard'Address_Size - 1) - 1);
      function To_Handle is new Ada.Unchecked_Conversion (Int, Handle);
   begin
      return To_Handle (Int (I));
   end To_Handle;

   function To_Wparam (I : Integer) return Wparam is
      type Int is range -(2 ** (Standard'Address_Size - 1)) ..
                         (2 ** (Standard'Address_Size - 1) - 1);
      function To_Wparam is new Ada.Unchecked_Conversion (Int, Wparam);
   begin
      return To_Wparam (Int (I));
   end To_Wparam;

   function To_Lparam (I : Integer) return Lparam is
      type Int is range -(2 ** (Standard'Address_Size - 1)) ..
                         (2 ** (Standard'Address_Size - 1) - 1);
      function To_Lparam is new Ada.Unchecked_Conversion (Int, Lparam);
   begin
      return To_Lparam (Int (I));
   end To_Lparam;

   function To_Integer (Result : Lresult) return Integer is
      type Int is range -(2 ** (Standard'Address_Size - 1)) ..
                         (2 ** (Standard'Address_Size - 1) - 1);
      function To_Int is new Ada.Unchecked_Conversion (Lresult, Int);
   begin
      return Integer (To_Int (Result));
   end To_Integer;

   function To_Handle (I : Interfaces.C.long) return Handle is
      type Uns is mod 2 ** Standard'Address_Size;
      function To_Handle is new Ada.Unchecked_Conversion (Uns, Handle);
   begin
      return To_Handle (Uns (I));
   end To_Handle;

   function "+" (Left, Right : in Point_Type) return Point_Type is
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : in Point_Type) return Point_Type is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function "-" (Left, Right : in Point_Type) return Size_Type is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   --------------------
   --  Right_Bottom  --
   --------------------

   procedure Right_Bottom (Rect : in out Rectangle_Type; Point : in Point_Type)
   is begin
      Rect.Right  := Point.X;
      Rect.Bottom := Point.Y;
   end Right_Bottom;

   function Right_Bottom (Rect : in Rectangle_Type) return Point_Type
   is begin
      return (X => Rect.Right, Y => Rect.Bottom);
   end Right_Bottom;

   ----------------
   --  Left_Top  --
   ----------------

   procedure Left_Top (Rect : in out Rectangle_Type; Point : in Point_Type)
   is begin
      Rect.Left := Point.X;
      Rect.Top  := Point.Y;
   end Left_Top;

   function Left_Top (Rect : in Rectangle_Type) return Point_Type
   is begin
      return (X => Rect.Left, Y => Rect.Top);
   end Left_Top;

   --------------
   --  Inside  --
   --------------

   function Inside (Point : in Point_Type; Rect : in Rectangle_Type)
                   return Boolean
   is begin
      return Point.X >= Rect.Left and
         Point.X <= Rect.Right and
         Point.Y >= Rect.Top and
         Point.Y <= Rect.Bottom;
   end Inside;

   function "+" (Left, Right : in Size_Type) return Size_Type is
   begin
      return (Left.Width + Right.Width,
              Left.Height + Right.Height);
   end "+";

   function "-" (Left, Right : in Size_Type) return Size_Type is
   begin
      return (Left.Width - Right.Width,
              Left.Height - Right.Height);
   end "-";

   function "+" (Left : in Point_Type; Right : in Size_Type) return Point_Type
   is begin
      return (Left.X + Right.Width, Left.Y + Right.Height);
   end "+";

   function "+" (Left : in Size_Type; Right : in Point_Type) return Point_Type
   is begin
      return (Left.Width + Right.X, Left.Height + Right.Y);
   end "+";

   ------------
   --  Size  --
   ------------

   function Size (Rectangle : in Rectangle_Type) return Size_Type
   is begin
      return
        (Width  => Rectangle.Right - Rectangle.Left,
         Height => Rectangle.Bottom - Rectangle.Top);
   end Size;

   -----------
   --  Max  --
   -----------

   function Max (Left, Right : in Size_Type) return Size_Type
   is begin
      return
        (Width  => Integer'Max (Left.Width, Right.Width),
         Height => Integer'Max (Left.Height, Right.Height));
   end Max;

end GWindows.Types;
