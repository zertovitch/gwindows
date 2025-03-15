------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                      G W I N D O W S . T I M E R S                       --
--                                                                          --
--                                 S p e c                                  --
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

--  Set_Timer... makes the system produce a periodic message of value WM_TIMER
--                 to the specified window.
--
--  Kill_Timer.. removes the timer.

--  GWindows.Timers is used in the following open-source projects:
--
--    GWenerator : (shipped with GWindows)
--    LEA        : https://l-e-a.sourceforge.io/

with GWindows.Base;

package GWindows.Timers is

  WM_TIMER : constant := 16#113#;

  procedure Set_Timer (Window       : Base.Base_Window_Type'Class;
                       ID_Event     : Natural;
                       Milliseconds : Natural);

  procedure Kill_Timer (Window   : Base.Base_Window_Type'Class;
                        ID_Event : Natural);

  error : exception;

end GWindows.Timers;
