------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                      G W I N D O W S . T I M E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2010 - 2023 Gautier de Montmollin             --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Types, Interfaces.C;

package body GWindows.Timers is

  use GWindows.Base, Interfaces.C;

  --  Windows interfacing part.
  --  Functionalities from Windows API's Winuser.

  type TIMERPROC is access procedure (hwnd    : Types.Handle;
                                      uMsg    : Interfaces.C.unsigned;
                                      idEvent : Interfaces.C.unsigned;
                                      dwTime  : Interfaces.C.unsigned_long);
  pragma Convention (Stdcall, TIMERPROC);

  function SetTimer (hWnd        : Types.Handle;
                     nIDEvent    : Interfaces.C.unsigned;
                     uElapse     : Interfaces.C.unsigned;
                     lpTimerFunc : TIMERPROC)
                     return Interfaces.C.unsigned;

  function KillTimer (hWnd     : Types.Handle;
                      uIDEvent : Interfaces.C.unsigned)
                      return Interfaces.C.int;

  pragma Import (Stdcall, SetTimer, "SetTimer");
  pragma Import (Stdcall, KillTimer, "KillTimer");

  ---------------
  -- Set_Timer --
  ---------------

  procedure Set_Timer (Window       : Base.Base_Window_Type'Class;
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

  procedure Kill_Timer (Window   : Base.Base_Window_Type'Class;
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

end GWindows.Timers;
