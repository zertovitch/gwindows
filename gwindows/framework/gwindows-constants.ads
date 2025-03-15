------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . C O N S T A N T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2022 David Botton                   --
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

package GWindows.Constants is

   Use_Default : constant := -2147483648;
   --  Constant used for X, Y, Width, or Height to direct OS to use
   --  a default value

   IDOK                       : constant := 1;
   IDCANCEL                   : constant := 2;
   IDABORT                    : constant := 3;
   IDRETRY                    : constant := 4;
   IDIGNORE                   : constant := 5;
   IDYES                      : constant := 6;
   IDNO                       : constant := 7;
   IDCLOSE                    : constant := 8;
   IDHELP                     : constant := 9;
   --  Common Win32 IDs

   IDM_FIRSTCHILD             : constant := 5000;
   --  When using MDI windows you should not have any menus with command
   --  IDs from IDM_FIRSTCHILD to IDM_FIRSTCHILD - 1 plus the maximum number
   --  of MDI child windows you will use.

   Max_Text : constant := 255;
   --  Maximum text size in various controls.

end GWindows.Constants;
