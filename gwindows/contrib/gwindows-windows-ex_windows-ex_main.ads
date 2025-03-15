------------------------------------------------------------------------------
--                                                                          --
--                  gwindows.windows.ex_windows.ex_main                     --
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
