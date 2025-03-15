------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . M U L T I _ M E D I A                  --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

package GWindows.Multi_Media is
   pragma Linker_Options ("-lwinmm");

   procedure Play_Sound_From_File (File_Name     : in GString;
                                   Async_Play    : in Boolean := False;
                                   Wait          : in Boolean := False;
                                   Loop_Sound    : in Boolean := False;
                                   Default_Sound : in Boolean := True;
                                   Interrupt     : in Boolean := True);
   --  Plays a wave file.
   --  Async_Play    if true plays the sound asynchronously
   --  Wait          if true will wait for sound driver to become available
   --                if busy
   --  Loop_Sound    if true will keeping replaying sound until next call to
   --                a Play_Sound procedure
   --  Default_Sound if true will play the default system sound if it can
   --                not play the requested sound
   --  Interrupt     if true will interrupt the playing of another sound and
   --                play the current sound

   procedure Play_Sound_From_Alias (Alias_Name    : in GString;
                                    Async_Play    : in Boolean := False;
                                    Wait          : in Boolean := False;
                                    Loop_Sound    : in Boolean := False;
                                    Default_Sound : in Boolean := True;
                                    Interrupt     : in Boolean := True);
   --  Plays a sound based on an alias name established in win.ini / registry
   --  i.e. system sounds. For example: Play_Sound_From_Alias ("MouseClick");
   --
   --  You can use any of the labels from
   --  "HKEY_CURRENT_USER\AppEvents\EvetLables"

   procedure Play_Sound_From_Resource (Resource_Name : in GString;
                                       Async_Play    : in Boolean := False;
                                       Wait          : in Boolean := False;
                                       Loop_Sound    : in Boolean := False;
                                       Default_Sound : in Boolean := True;
                                       Interrupt     : in Boolean := True);
   --  Plays a sound from a WAVE resource

end GWindows.Multi_Media;
