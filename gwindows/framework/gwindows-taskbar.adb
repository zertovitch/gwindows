------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     G W I N D O W S . T A S K B A R                      --
--                                                                          --
--                                 B o d y                                  --
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

with GWindows.Types, GNATCOM.Initialize, Interfaces.C;

package body GWindows.Taskbar is

   --  Internal
   procedure Init_if_needed (List : in out Taskbar_List) is
   begin
     if List.Initialized then
       return;
     end if;
     Create (List, TaskbarLib.CLSID_TaskbarList);
     HrInit (List);
     List.Initialized := True;
   end Init_if_needed;

   procedure Set_Progress_State (
      List   : in out Taskbar_List;
      Window : GWindows.Base.Base_Window_Type'Class;
      State  : Taskbar_Progress_State
   )
   is
      hwnd : constant Interfaces.C.long :=
                Interfaces.C.long (GWindows.Types.To_Lresult
                   (GWindows.Base.Handle (Window)));
      TBPF_NOPROGRESS    : constant := 16#0000_0000#;
      TBPF_INDETERMINATE : constant := 16#0000_0001#;
      TBPF_NORMAL        : constant := 16#0000_0002#;
      TBPF_ERROR         : constant := 16#0000_0004#;
      TBPF_PAUSED        : constant := 16#0000_0008#;
   begin
      Init_if_needed (List);
      case State is
         when No_Progress =>
            SetProgressState (List, hwnd, TBPF_NOPROGRESS);
         when Indeterminate =>
            SetProgressState (List, hwnd, TBPF_INDETERMINATE);
         when Normal =>
            SetProgressState (List, hwnd, TBPF_NORMAL);
         when Error =>
            SetProgressState (List, hwnd, TBPF_ERROR);
         when Paused =>
            SetProgressState (List, hwnd, TBPF_PAUSED);
      end case;
   end Set_Progress_State;

   procedure Set_Progress_Value (
      List      : in out Taskbar_List;
      Window    : GWindows.Base.Base_Window_Type'Class;
      Completed : Natural;
      Total     : Natural
   )
   is
      hwnd : constant Interfaces.C.long :=
                Interfaces.C.long (GWindows.Types.To_Lresult
                   (GWindows.Base.Handle (Window)));
   begin
      Init_if_needed (List);
      SetProgressValue (List, hwnd,
        (Interfaces.C.unsigned (Completed), 0),
        (Interfaces.C.unsigned (Total), 0)
      );
   end Set_Progress_Value;

begin
   GNATCOM.Initialize.Initialize_COM;
end GWindows.Taskbar;
