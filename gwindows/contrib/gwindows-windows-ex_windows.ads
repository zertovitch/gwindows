------------------------------------------------------------------------------
--                                                                          --
--                       gwindows.windows.ex_windows                        --
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
--  The Ex_Window_Type supports the tooltip-notification
--  Necessary to display the tooltip-text for a toolbar-button
------------------------------------------------------------------------------

with GWindows.Base; use GWindows.Base;
with GWindows.Common_Controls.Ex_Tb; use GWindows.Common_Controls.Ex_Tb;

package GWindows.Windows.Ex_Windows is

   type Ex_Window_Type is new Window_Type with private;
   type Ex_Window_Access is access all Ex_Window_Type;
   type Pointer_To_Ex_Window_Class is access all Ex_Window_Type'Class;

   procedure Activate_Tooltip (Window  : in out Ex_Window_Type;
                               Toolbar : in out Ex_Toolbar_Control_Type);
   --  Activate the Tooltips

   --------------------
   -- event-handling --
   --------------------

   procedure On_Notify (Window       : in out Ex_Window_Type;
                        Message      : in     Pointer_To_Notification;
                        Control      : in     Pointer_To_Base_Window_Class;
                        Return_Value : in out Interfaces.C.long);
private

   type Ex_Window_Type is new Window_Type with
      record
         Toolbar : Ex_Toolbar_Control_Access;
      end record;

end GWindows.Windows.Ex_Windows;
