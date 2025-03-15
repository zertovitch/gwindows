------------------------------------------------------------------------------
--                                                                          --
--           GWINDOWS - Ada 95 Framework for Windows Development            --
--                                                                          --
--                       G W I N D O W S . E R R O R S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2020 David Botton                   --
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

with Ada.Exceptions;

with System.Storage_Elements;

with GWindows.GStrings,
     GWindows.Types;

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
        (dwFlags      : GWindows.Types.DWORD    :=
           FORMAT_MESSAGE_FROM_SYSTEM;
         lpSource     : access GChar_C          := null;
         dwMessageId  : GWindows.Types.DWORD    :=
           GWindows.Types.DWORD (Error_Number);
         dwLanguageId : GWindows.Types.DWORD    := 0;
         lpBuffer     : access GChar_C          := Message (0)'Access;
         nSize        : GWindows.Types.DWORD    := MAX_ERROR;
         Arguments    : access GChar_C          := null);
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
      end if;
   end Error_Check;

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Integer
   is
      function GetLastError return GWindows.Types.DWORD;
      pragma Import (StdCall, GetLastError, "GetLastError");
   begin
      return Integer (GetLastError);
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
