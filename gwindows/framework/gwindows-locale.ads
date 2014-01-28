------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                       G W I N D O W S . L O C A L E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2014 Gautier de Montmollin              --
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
--  Thanks to Nicolas Brunot for advice and pointers on MSDN.

package GWindows.Locale is

   function Get_Decimal_Separator return GString;
   function Get_Thousands_Separator return GString;

   --  General function for retrieving locale informations, in case not defined
   --  in the above functions.

   function Get_Locale_Info (Locale_Info_Code : Integer) return GString;
   --  Locale_Info_Code: a value of one of the MS Locale Information Constants:
   --  http://msdn.microsoft.com/en-us/library/dd464799(v=vs.85).aspx

end GWindows.Locale;
