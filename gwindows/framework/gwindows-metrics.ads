------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . M E T R I C S                       --
--                                                                          --
--                                 S p e c                                  --
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

package GWindows.Metrics is

   function Get_System_Metric (Index : Integer) return Integer;

   SM_CXSCREEN                : constant := 0;
   SM_CYSCREEN                : constant := 1;
   SM_CXVSCROLL               : constant := 2;
   SM_CYHSCROLL               : constant := 3;
   SM_CYCAPTION               : constant := 4;
   SM_CXBORDER                : constant := 5;
   SM_CYBORDER                : constant := 6;
   SM_CXDLGFRAME              : constant := 7;
   SM_CYDLGFRAME              : constant := 8;
   SM_CYVTHUMB                : constant := 9;
   SM_CXHTHUMB                : constant := 10;
   SM_CXICON                  : constant := 11;
   SM_CYICON                  : constant := 12;
   SM_CXCURSOR                : constant := 13;
   SM_CYCURSOR                : constant := 14;
   SM_CYMENU                  : constant := 15;
   SM_CXFULLSCREEN            : constant := 16;
   SM_CYFULLSCREEN            : constant := 17;
   SM_CYKANJIWINDOW           : constant := 18;
   SM_MOUSEPRESENT            : constant := 19;
   SM_CYVSCROLL               : constant := 20;
   SM_CXHSCROLL               : constant := 21;
   SM_DEBUG                   : constant := 22;
   SM_SWAPBUTTON              : constant := 23;
   SM_RESERVED1               : constant := 24;
   SM_RESERVED2               : constant := 25;
   SM_RESERVED3               : constant := 26;
   SM_RESERVED4               : constant := 27;
   SM_CXMIN                   : constant := 28;
   SM_CYMIN                   : constant := 29;
   SM_CXSIZE                  : constant := 30;
   SM_CYSIZE                  : constant := 31;
   SM_CXFRAME                 : constant := 32;
   SM_CYFRAME                 : constant := 33;
   SM_CXMINTRACK              : constant := 34;
   SM_CYMINTRACK              : constant := 35;
   SM_CXDOUBLECLK             : constant := 36;
   SM_CYDOUBLECLK             : constant := 37;
   SM_CXICONSPACING           : constant := 38;
   SM_CYICONSPACING           : constant := 39;
   SM_MENUDROPALIGNMENT       : constant := 40;
   SM_PENWINDOWS              : constant := 41;
   SM_DBCSENABLED             : constant := 42;
   SM_CMOUSEBUTTONS           : constant := 43;
   SM_CXFIXEDFRAME            : constant := 7;
   SM_CYFIXEDFRAME            : constant := 8;
   SM_CXSIZEFRAME             : constant := 32;
   SM_CYSIZEFRAME             : constant := 33;
   SM_SECURE                  : constant := 44;
   SM_CXEDGE                  : constant := 45;
   SM_CYEDGE                  : constant := 46;
   SM_CXMINSPACING            : constant := 47;
   SM_CYMINSPACING            : constant := 48;
   SM_CXSMICON                : constant := 49;
   SM_CYSMICON                : constant := 50;
   SM_CYSMCAPTION             : constant := 51;
   SM_CXSMSIZE                : constant := 52;
   SM_CYSMSIZE                : constant := 53;
   SM_CXMENUSIZE              : constant := 54;
   SM_CYMENUSIZE              : constant := 55;
   SM_ARRANGE                 : constant := 56;
   SM_CXMINIMIZED             : constant := 57;
   SM_CYMINIMIZED             : constant := 58;
   SM_CXMAXTRACK              : constant := 59;
   SM_CYMAXTRACK              : constant := 60;
   SM_CXMAXIMIZED             : constant := 61;
   SM_CYMAXIMIZED             : constant := 62;
   SM_NETWORK                 : constant := 63;
   SM_CLEANBOOT               : constant := 67;
   SM_CXDRAG                  : constant := 68;
   SM_CYDRAG                  : constant := 69;
   SM_SHOWSOUNDS              : constant := 70;
   SM_CXMENUCHECK             : constant := 71;
   SM_CYMENUCHECK             : constant := 72;
   SM_SLOWMACHINE             : constant := 73;
   SM_MIDEASTENABLED          : constant := 74;
   SM_MOUSEWHEELPRESENT       : constant := 75;
   SM_CMETRICS                : constant := 76;

end GWindows.Metrics;
