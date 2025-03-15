------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     G W I N D O W S . T A S K B A R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                  Copyright (C) 2014 Gautier de Montmollin                --
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

--  This package deals with the taskbar.

--  TaskbarLib package and children were obtained from the TypeLib
--  TaskbarLib.tlb (available on the Internet)
--  and the BindCOM command: bindcom TaskbarLib.tlb TaskbarLib

with TaskbarLib.ITaskbarList3_Interface;

with GWindows.Base;
with GNATCOM.Errors;

package GWindows.Taskbar is

   type Taskbar_List is tagged private;

   type Taskbar_Progress_State is
     (No_Progress, Indeterminate, Normal, Error, Paused);

   procedure Set_Progress_State (
      List   : in out Taskbar_List;
      Window : GWindows.Base.Base_Window_Type'Class;
      State  : Taskbar_Progress_State
   );

   procedure Set_Progress_Value (
      List      : in out Taskbar_List;
      Window    : GWindows.Base.Base_Window_Type'Class;
      Completed : Natural;
      Total     : Natural
   );

   Taskbar_Interface_Not_Supported : exception
      renames
      GNATCOM.Errors.NO_INTERFACE_ERROR;

   --  Possible cause: Windows version is prior to Windows 7

private

   type Taskbar_List is new
      TaskbarLib.ITaskbarList3_Interface.ITaskbarList3_Type with
   record
      Initialized : Boolean := False;
   end record;

end GWindows.Taskbar;
