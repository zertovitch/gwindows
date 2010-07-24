------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                       G W I N D O W S . E R R O R S                      --
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

with Ada.Exceptions;

with System.Storage_Elements;

with Interfaces.C;

with GWindows.GStrings;

package body GWindows.Errors is

   function Strip (Image_String : GString) return GString;

   ---------------------
   -- Error_To_String --
   ---------------------

   function Error_To_String (Error_Number : in Integer) return GString is
      FORMAT_MESSAGE_FROM_SYSTEM : constant := 4096;
      MAX_ERROR                  : constant := 1024;
      Message                    : GString_C (0 .. MAX_ERROR);

      procedure FormatMessage
        (dwFlags      : Integer                    :=
           FORMAT_MESSAGE_FROM_SYSTEM;
         lpSource     : Interfaces.C.unsigned_long := 0;
         hr           : Integer                    := Error_Number;
         dwLanguageId : Interfaces.C.unsigned_long := 0;
         lpBuffer     : access GChar_C             := Message (0)'Access;
         nSize        : Interfaces.C.unsigned_long := MAX_ERROR;
         Arguments    : Interfaces.C.unsigned_long := 0);
      pragma Import (StdCall, FormatMessage, "FormatMessage" &
                    Character_Mode_Identifier);
   begin
      FormatMessage;
      return GWindows.GStrings.To_GString_From_C (Message);
   end Error_To_String;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in Integer)
   is
   begin
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (Win32_Error'Identity,
            GWindows.GStrings.To_String (Get_Last_Error));
         raise Win32_Error;
      end if;
   end Error_Check;

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Integer
   is
      function GetLastError return Integer;
      pragma Import (StdCall, GetLastError, "GetLastError");
   begin
      return GetLastError;
   end Get_Last_Error;

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return GString
   is
   begin
      return Error_To_String (Get_Last_Error);
   end Get_Last_Error;

   ---------------
   -- To_String --
   ---------------

   function To_String (Address : System.Address) return GString
   is
   begin
      return Strip
        (GWindows.GStrings.To_GString_From_String
         (System.Storage_Elements.To_Integer (Address)'Img));
   end To_String;

   -----------
   -- Strip --
   -----------

   function Strip (Image_String : GString) return GString
   is
   begin
      return Image_String (Image_String'First + 1 .. Image_String'Last);
   end Strip;

end GWindows.Errors;
