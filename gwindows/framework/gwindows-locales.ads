------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                      G W I N D O W S . L O C A L E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2014 Gautier de Montmollin              --
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
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides access to the so-called locale
--  - see http://en.wikipedia.org/wiki/Locale - as set in Windows
--  in the "Region and Language" control panel.
--  The locale is also known as the "regional settings".
--
--  Note that language and country informations are in Ada.Locales (Ada 2012+).
--
--  Thanks to Nicolas Brunot for advice and pointers on MSDN.

package GWindows.Locales is

   --  Get some default locale informations for current user

   function Get_List_Separator return GString;
   --  Typically ',' or ';' (used as such by Excel for
   --  Input-Output of CSV files (unfortunately))

   function Get_Decimal_Separator return GString;
   function Get_Thousands_Separator return GString;

   --  General function for retrieving locale informations, in case not defined
   --  in the above functions.

   function Get_Locale_Info (Locale_Info_Code : Integer) return GString;
   --  Locale_Info_Code: a value of one of the MS Locale Information Constants:
   --  http://msdn.microsoft.com/en-us/library/dd464799(v=vs.85).aspx

   Get_Locale_Info_Failed : exception;

end GWindows.Locales;
