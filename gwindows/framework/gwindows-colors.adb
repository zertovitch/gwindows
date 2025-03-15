------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C O L O R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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

with Ada.Unchecked_Conversion;

package body GWindows.Colors is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   --------------
   -- To_Color --
   --------------

   function To_Color (RGB : RGB_Type)
                     return Color_Type
   is
      function To_Color_Type is new Ada.Unchecked_Conversion (RGB_Type,
                                                              Color_Type);
   begin
      return To_Color_Type (RGB);
   end To_Color;

   function To_Color (Red_Value, Green_Value, Blue_Value : Color_Range) return Color_Type
   is
   begin
      return To_Color (RGB_Type'(Red_Value, Green_Value, Blue_Value, 0));
   end To_Color;

   ------------
   -- To_RGB --
   ------------

   function To_RGB (Color : in Color_Type) return RGB_Type
   is
      function To_RGB_Type is new Ada.Unchecked_Conversion (Color_Type,
                                                            RGB_Type);
   begin
      return To_RGB_Type (Color);
   end To_RGB;

   ------------------
   -- System_Color --
   ------------------

   function System_Color (Color_Const : Integer) return Color_Type
   is
      function GetSysColor
        (nIndex : Integer := Color_Const)
        return Color_Type;
      pragma Import (StdCall, GetSysColor, "GetSysColor");
   begin
      return GetSysColor;
   end System_Color;

end GWindows.Colors;
