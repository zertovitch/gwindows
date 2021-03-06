------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.windows.ex_windows.ex_main                     --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------
--  Ex_Main_Window_Type is a top level window that closes the
--  message loop when destroyed
------------------------------------------------------------------------------

package GWindows.Windows.Ex_Windows.Ex_Main is

   type Ex_Main_Window_Type is new Ex_Window_Type with private;
   type Ex_Main_Window_Access is access all Ex_Main_Window_Type;
   type Pointer_To_Ex_Main_Window_Class is
     access all Ex_Main_Window_Type'Class;

   --------------------
   -- event-handling --
   --------------------

   procedure On_Destroy (Window : in out Ex_Main_Window_Type);
   --  Shuts down the message loop when the window is closed

private

   type Ex_Main_Window_Type is
     new GWindows.Windows.Ex_Windows.Ex_Window_Type with null record;
end GWindows.Windows.Ex_Windows.Ex_Main;
