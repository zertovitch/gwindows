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

with GWindows.Types, GNATCOM.Initialize, Interfaces.C;

package body GWindows.Taskbar is

   procedure Init_if_needed (List : in out Taskbar_List) is
   begin
     if List.initialized then
       return;
     end if;
     Create (List, TaskbarLib.CLSID_TaskbarList);
     HrInit (List);
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
      TBPF_NOPROGRESS    : constant := 16#00000000#;
      TBPF_INDETERMINATE : constant := 16#00000001#;
      TBPF_NORMAL        : constant := 16#00000002#;
      TBPF_ERROR         : constant := 16#00000004#;
      TBPF_PAUSED        : constant := 16#00000008#;
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
