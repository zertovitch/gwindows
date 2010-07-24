------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . M U L T I _ M E D I A                  --
--                                                                          --
--                                 B o d y                                  --
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

with Interfaces.C;
with GWindows.GStrings;
with GWindows.Application;
with GWindows.Types;

package body GWindows.Multi_Media is
--     SND_SYNC                        : constant := 0;
   SND_ASYNC                       : constant := 1;
   SND_NODEFAULT                   : constant := 2;
--     SND_MEMORY                      : constant := 4;
   SND_LOOP                        : constant := 8;
   SND_NOSTOP                      : constant := 16;
   SND_NOWAIT                      : constant := 16#00002000#;
   SND_ALIAS                       : constant := 16#00010000#;
   SND_FILENAME                    : constant := 16#00020000#;
   SND_RESOURCE                    : constant := 16#00040004#;

   procedure Play_Sound
     (Name          : in GString;
      Name_Kind     : in Interfaces.C.unsigned;
      Async_Play    : in Boolean               := False;
      Wait          : in Boolean               := False;
      Loop_Sound    : in Boolean               := False;
      Default_Sound : in Boolean               := True;
      Interrupt     : in Boolean               := True);
   --  Plays a sound based on Name_Kind

   ----------------
   -- Play_Sound --
   ----------------

   procedure Play_Sound
     (Name          : in GString;
      Name_Kind     : in Interfaces.C.unsigned;
      Async_Play    : in Boolean               := False;
      Wait          : in Boolean               := False;
      Loop_Sound    : in Boolean               := False;
      Default_Sound : in Boolean               := True;
      Interrupt     : in Boolean               := True)
   is
      use type Interfaces.C.unsigned;

      C_Text : GString_C := GWindows.GStrings.To_GString_C (Name);

      Flags  : Interfaces.C.unsigned := Name_Kind;

      procedure PlaySound
        (Sound    : access GChar_C        := C_Text (C_Text'First)'Access;
         Handle   : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         fdwSound : in     Interfaces.C.unsigned);
      pragma Import (StdCall, PlaySound,
                       "PlaySound" & Character_Mode_Identifier);
   begin
      if Async_Play then
         Flags := Flags or SND_ASYNC;
      end if;

      if not Wait then
         Flags := Flags or SND_NOWAIT;
      end if;

      if Loop_Sound then
         Flags := Flags or SND_LOOP;
      end if;

      if not Default_Sound then
         Flags := Flags or SND_NODEFAULT;
      end if;

      if not Interrupt then
         Flags := Flags or SND_NOSTOP;
      end if;

      if Name_Kind = SND_RESOURCE then
         PlaySound (Handle   => GWindows.Application.hInstance,
                    fdwSound => Flags);
      else
         PlaySound (fdwSound => Flags);
      end if;

   end Play_Sound;

   ---------------------------
   -- Play_Sound_From_Alias --
   ---------------------------

   procedure Play_Sound_From_Alias
     (Alias_Name    : in GString;
      Async_Play    : in Boolean := False;
      Wait          : in Boolean := False;
      Loop_Sound    : in Boolean := False;
      Default_Sound : in Boolean := True;
      Interrupt     : in Boolean := True)
   is
   begin
      Play_Sound (Alias_Name, SND_ALIAS,
                  Async_Play, Wait, Loop_Sound, Default_Sound, Interrupt);
   end Play_Sound_From_Alias;

   --------------------------
   -- Play_Sound_From_File --
   --------------------------

   procedure Play_Sound_From_File
     (File_Name     : in GString;
      Async_Play    : in Boolean := False;
      Wait          : in Boolean := False;
      Loop_Sound    : in Boolean := False;
      Default_Sound : in Boolean := True;
      Interrupt     : in Boolean := True)
   is
   begin
      Play_Sound (File_Name, SND_FILENAME,
                  Async_Play, Wait, Loop_Sound, Default_Sound, Interrupt);
   end Play_Sound_From_File;

   ------------------------------
   -- Play_Sound_From_Resource --
   ------------------------------

   procedure Play_Sound_From_Resource
     (Resource_Name : in GString;
      Async_Play    : in Boolean := False;
      Wait          : in Boolean := False;
      Loop_Sound    : in Boolean := False;
      Default_Sound : in Boolean := True;
      Interrupt     : in Boolean := True)
   is
   begin
      Play_Sound (Resource_Name, SND_RESOURCE,
                  Async_Play, Wait, Loop_Sound, Default_Sound, Interrupt);
   end Play_Sound_From_Resource;

end GWindows.Multi_Media;
