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
