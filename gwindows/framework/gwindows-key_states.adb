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
