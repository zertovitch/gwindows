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

--  This package deals with the taskbar.

--  TaskbarLib package and children were obtained from the TypeLib
--  TaskbarLib.tlb (available on the Internet)
--  and the BindCOM command: bindcom TaskbarLib.tlb TaskbarLib

with TaskbarLib.ITaskbarList3_Interface;

with GWindows.Base;

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

   Taskbar_Operation_Failed : exception;
   --  Possible cause: Windows version is prior to 7

private

   type Taskbar_List is new
      TaskbarLib.ITaskbarList3_Interface.ITaskbarList3_Type with
   record
      Initialized : Boolean := False;
   end record;

end GWindows.Taskbar;
