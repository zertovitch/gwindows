------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                  G W I N D O W S . C L I P B O A R D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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
