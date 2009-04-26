------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C O L O R S                        --
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

   function To_Color (Red, Green, Blue : Color_Range) return Color_Type
   is
   begin
      return To_Color (RGB_Type'(Red, Green, Blue, 0));
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
