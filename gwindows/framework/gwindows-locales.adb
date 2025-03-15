------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                      G W I N D O W S . L O C A L E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--               Copyright (C) 2014 - 2022 Gautier de Montmollin            --
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
--   http://sf.net/projects/gnavi/                                          --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Constants;
with GWindows.GStrings;
with GWindows.Types;

package body GWindows.Locales is

   --  Full list of the MS Locale Information Constants:
   --  http://msdn.microsoft.com/en-us/library/dd464799(v=vs.85).aspx

   LOCALE_SLIST      : constant := 16#0000_000C#;
   LOCALE_SDECIMAL   : constant := 16#0000_000E#;
   LOCALE_STHOUSAND  : constant := 16#0000_000F#;

   function Get_List_Separator return GString is
   begin
      return Get_Locale_Info (LOCALE_SLIST);
   end Get_List_Separator;

   function Get_Decimal_Separator return GString is
   begin
      return Get_Locale_Info (LOCALE_SDECIMAL);
   end Get_Decimal_Separator;

   function Get_Thousands_Separator return GString is
   begin
      return Get_Locale_Info (LOCALE_STHOUSAND);
   end Get_Thousands_Separator;

   LOCALE_USER_DEFAULT : constant := 16#400#;

   function Get_Locale_Info (Locale_Info_Code : Integer) return GString is

      Buffer : GString_C (0 .. Constants.Max_Text);

      function GetLocaleInfo
        (Locale   : Interfaces.C.long  := LOCALE_USER_DEFAULT;
         LCType   : Interfaces.C.long  := Interfaces.C.long (Locale_Info_Code);
         lpLCData : Types.LPTSTR       := Buffer (0)'Unchecked_Access;
         cchData  : Interfaces.C.int   := Constants.Max_Text)
      return Interfaces.C.int;

      pragma Import (StdCall, GetLocaleInfo,
                    "GetLocaleInfo" & Character_Mode_Identifier);

      use type Interfaces.C.int;
   begin
      if GetLocaleInfo /= 0 then
         return GWindows.GStrings.To_GString_From_C (Buffer);
      else
         raise Get_Locale_Info_Failed;
      end if;
   end Get_Locale_Info;

end GWindows.Locales;
