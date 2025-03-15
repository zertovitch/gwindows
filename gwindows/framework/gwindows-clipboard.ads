------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                  G W I N D O W S . C L I P B O A R D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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

with GWindows.Windows;

with Ada.Strings.Unbounded;

package GWindows.Clipboard is

   procedure Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in GString);

   procedure Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in GString_Unbounded);

   function Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type)
      return GString;

   function Is_Clipboard_Text_Available
      return Boolean;

   --  The following subprograms are older versions
   --  (pre-Aug-2018), supporting only ANSI strings:

   procedure Set_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in String);
   pragma Obsolescent (Set_Clipboard_Text);

   procedure Set_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in Ada.Strings.Unbounded.Unbounded_String);
   pragma Obsolescent (Set_Clipboard_Text);

   function Get_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type)
      return String;
   pragma Obsolescent (Get_Clipboard_Text);

   function Is_Clipboard_Text
      return Boolean;
   pragma Obsolescent (Is_Clipboard_Text);

end GWindows.Clipboard;
