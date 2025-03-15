------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     G W I N D O W S . C O L O R S                        --
--                                                                          --
--                                 S p e c                                  --
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

with Interfaces.C;

package GWindows.Colors is

   type Color_Type is new Interfaces.C.unsigned;

   type Color_Range is range 0 .. 255;
   for Color_Range'Size use 8;

   type RGB_Type is
      record
         Red    : Color_Range := 0;
         Green  : Color_Range := 0;
         Blue   : Color_Range := 0;
         Unused : Color_Range := 0;
      end record;
   for RGB_Type'Size use 32;
   for RGB_Type use
      record
         Red    at 0 range 0 .. 7;
         Green  at 1 range 0 .. 7;
         Blue   at 2 range 0 .. 7;
         Unused at 3 range 0 .. 7;
      end record;

   function To_Color (Red_Value, Green_Value, Blue_Value : Color_Range) return Color_Type;
   function To_Color (RGB : RGB_Type) return Color_Type;
   function To_RGB (Color : in Color_Type) return RGB_Type;

   COLOR_SCROLLBAR            : constant := 0;
   COLOR_BACKGROUND           : constant := 1;
   COLOR_ACTIVECAPTION        : constant := 2;
   COLOR_INACTIVECAPTION      : constant := 3;
   COLOR_MENU                 : constant := 4;
   COLOR_WINDOW               : constant := 5;
   COLOR_WINDOWFRAME          : constant := 6;
   COLOR_MENUTEXT             : constant := 7;
   COLOR_WINDOWTEXT           : constant := 8;
   COLOR_CAPTIONTEXT          : constant := 9;
   COLOR_ACTIVEBORDER         : constant := 10;
   COLOR_INACTIVEBORDER       : constant := 11;
   COLOR_APPWORKSPACE         : constant := 12;
   COLOR_HIGHLIGHT            : constant := 13;
   COLOR_HIGHLIGHTTEXT        : constant := 14;
   COLOR_BTNFACE              : constant := 15;
   COLOR_BTNSHADOW            : constant := 16;
   COLOR_GRAYTEXT             : constant := 17;
   COLOR_BTNTEXT              : constant := 18;
   COLOR_INACTIVECAPTIONTEXT  : constant := 19;
   COLOR_BTNHIGHLIGHT         : constant := 20;
   COLOR_3DDKSHADOW           : constant := 21;
   COLOR_3DLIGHT              : constant := 22;
   COLOR_INFOTEXT             : constant := 23;
   COLOR_INFOBK               : constant := 24;
   COLOR_DESKTOP              : constant := 1;
   COLOR_3DFACE               : constant := 15;
   COLOR_3DSHADOW             : constant := 16;
   COLOR_3DHIGHLIGHT          : constant := 20;
   COLOR_3DHILIGHT            : constant := 20;
   COLOR_BTNHILIGHT           : constant := 20;
   --  System Color Constants

   function System_Color (Color_Const : Integer) return Color_Type;
   --  Returns color set in system

   White       : constant Color_Type := 16#FFFFFF#;
   Black       : constant Color_Type := 16#000000#;
   Silver      : constant Color_Type := 16#E0E0E0#;
   Light_Gray  : constant Color_Type := 16#C0C0C0#;
   Gray        : constant Color_Type := 16#808080#;
   Dark_Gray   : constant Color_Type := 16#404040#;
   Red         : constant Color_Type := 16#0000FF#;
   Dark_Red    : constant Color_Type := 16#000080#;
   Green       : constant Color_Type := 16#00FF00#;
   Dark_Green  : constant Color_Type := 16#008000#;
   Light_Blue  : constant Color_Type := 16#FFDD88#;
   Blue        : constant Color_Type := 16#FF0000#;
   Dark_Blue   : constant Color_Type := 16#800000#;
   Yellow      : constant Color_Type := 16#00FFFF#;
   Magenta     : constant Color_Type := 16#FF00FF#;
   Cyan        : constant Color_Type := 16#FFFF00#;
   Pink        : constant Color_Type := 16#AFAFFF#;
   Orange      : constant Color_Type := 16#00C8FF#;
   Dark_Orange : constant Color_Type := 16#248DF0#;
   --  Basic Colors

end GWindows.Colors;
