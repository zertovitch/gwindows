------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                       W I N D O W S _ T I M E R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2023 Gautier de Montmollin                 --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Types, Interfaces.C;

package body Windows_Timers is

  use GWindows.Base, Interfaces.C;

  --  Windows interfacing part.
  --  Functionalities from Windows API's Winuser.

  type TIMERPROC is access procedure (hwnd    : GWindows.Types.Handle;
                                      uMsg    : Interfaces.C.unsigned;
                                      idEvent : Interfaces.C.unsigned;
                                      dwTime  : Interfaces.C.unsigned_long);
  pragma Convention (Stdcall, TIMERPROC);

  function SetTimer (hWnd        : GWindows.Types.Handle;
                     nIDEvent    : Interfaces.C.unsigned;
                     uElapse     : Interfaces.C.unsigned;
                     lpTimerFunc : TIMERPROC)
                    return Interfaces.C.unsigned;

  function KillTimer (hWnd     : GWindows.Types.Handle;
                      uIDEvent : Interfaces.C.unsigned)
                     return Interfaces.C.int;

  pragma Import (Stdcall, SetTimer, "SetTimer");
  pragma Import (Stdcall, KillTimer, "KillTimer");

  ---------------
  -- Set_Timer --
  ---------------

  procedure Set_Timer (Window       : GWindows.Base.Base_Window_Type'Class;
                       ID_Event     : Natural;
                       Milliseconds : Natural)
  is
    res : Interfaces.C.unsigned;
  begin
    res := SetTimer (hWnd        => Handle (Window),
                     nIDEvent    => Interfaces.C.unsigned (ID_Event),
                     uElapse     => Interfaces.C.unsigned (Milliseconds),
                     lpTimerFunc => null);
    if res = 0 then
      raise error;
    end if;
  end Set_Timer;

  ----------------
  -- Kill_Timer --
  ----------------

  procedure Kill_Timer (Window   : GWindows.Base.Base_Window_Type'Class;
                        ID_Event : Natural)
  is
    res : Interfaces.C.int;
  begin
    res := KillTimer (hWnd     => Handle (Window),
                      uIDEvent => Interfaces.C.unsigned (ID_Event));
    if res = 0 then
      raise error;
    end if;
  end Kill_Timer;

end Windows_Timers;
