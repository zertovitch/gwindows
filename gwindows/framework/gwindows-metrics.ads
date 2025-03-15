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
