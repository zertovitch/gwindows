------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . K E Y  _ S T A T E S                   --
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

with Interfaces.C;

package body GWindows.Key_States is

   function GetKeyState (nVirtKey : Integer)
                        return Interfaces.C.unsigned_short;
   --  winuser.h declares this to return SHORT, but we need it
   --  unsigned for Is_Key_Down.
   pragma Import (StdCall, GetKeyState, "GetKeyState");

   -----------------
   -- Is_Key_Down --
   -----------------

   function Is_Key_Down
     (Virtual_Key_Code : Integer)
      return Boolean
   is
      use type Interfaces.C.unsigned;

      State : constant Interfaces.C.unsigned :=
        Interfaces.C.unsigned (GetKeyState (Virtual_Key_Code));
   begin
      return (State and 128) = 128;
   end Is_Key_Down;

   --------------------
   -- Is_Key_Toggled --
   --------------------

   function Is_Key_Toggled
     (Virtual_Key_Code : Integer)
      return Boolean
   is
      use type Interfaces.C.unsigned;

      State : constant Interfaces.C.unsigned :=
        Interfaces.C.unsigned (GetKeyState (Virtual_Key_Code));
   begin
      return (State and 1) = 1;
   end Is_Key_Toggled;

end GWindows.Key_States;
