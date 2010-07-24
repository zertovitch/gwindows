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
