------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                       W I N D O W S _ T I M E R S                        --
--                                                                          --
--                                 S p e c                                  --
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

--  Set_Timer... makes the system produce a periodic message of value WM_TIMER
--                 to the specified window.
--
--  Kill_Timer.. removes the timer.

--  Windows_Timers is used in the following open-source project:
--
--    GWenerator : (shipped with GWindows)
--    LEA        : https://l-e-a.sourceforge.io/

with GWindows.Base;

package Windows_Timers is

  WM_TIMER : constant := 16#113#;

  procedure Set_Timer (Window       : GWindows.Base.Base_Window_Type'Class;
                       ID_Event     : Natural;
                       Milliseconds : Natural);

  procedure Kill_Timer (Window   : GWindows.Base.Base_Window_Type'Class;
                        ID_Event : Natural);

  error : exception;

end Windows_Timers;
