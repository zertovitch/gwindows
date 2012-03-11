--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 David Botton.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with This program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this program, or you link this program object files with other
--  files to produce an executable, that does not by itself cause the
--  resulting executable to be covered by the GNU General Public
--  License. This exception does not however invalidate any other
--  reasons why the executable file might be covered by the GNU Public
--  License.

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with GWindows.Key_States;
with GWindows.Metrics;
with Interfaces;
package body GWindows.Testing.Events is

   Small_Delay : constant Duration := 0.02;

   type Unsigned_30 is mod 2**30;

   type Win32_Key_Event_Flag_Type is record
      Extended_Key : Boolean;
      Key_Up       : Boolean;
      Reserved     : Unsigned_30 := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, Win32_Key_Event_Flag_Type);
   for Win32_Key_Event_Flag_Type use record
      Extended_Key at 0 range 0 ..  0;
      Key_Up       at 0 range 1 ..  1;
      Reserved     at 0 range 2 .. 31;
   end record;
   for Win32_Key_Event_Flag_Type'Size use 32;

   procedure Win32_Keybd_Event
      (BVk         : in Interfaces.Unsigned_8;
       bScan       : in Interfaces.Unsigned_8;
       DwFlags     : in Win32_Key_Event_Flag_Type;
       dwExtraInfo : in Interfaces.Unsigned_32);
   pragma Import (StdCall, Win32_Keybd_Event, "keybd_event");

   function To_Virtual is new Ada.Unchecked_Conversion
      (Source => Character,
       Target => Interfaces.Unsigned_8);

   procedure Key_Event
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Up           : in Boolean;
      Key_Delay        : in Duration := Default_Delay)
   is
      Flags : Win32_Key_Event_Flag_Type;
   begin
      Flags.Key_Up := Key_Up;
      Win32_Keybd_Event (Virtual_Key_Code, 20, Flags, 0);
      delay Key_Delay;
   end Key_Event;

   procedure Key_Stroke
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Delay        : in Duration := Default_Delay)
   is begin
      Key_Event (Virtual_Key_Code, Key_Up => False, Key_Delay => Key_Delay);
      Key_Event (Virtual_Key_Code, Key_Up => True, Key_Delay => Key_Delay);
   end Key_Stroke;

   procedure Alt_Key_Stroke
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Delay        : in Duration := Default_Delay)
   is begin
      Key_Event (Key_States.VK_MENU, Key_Up => False, Key_Delay => Key_Delay);
      Key_Event (Virtual_Key_Code, Key_Up => False, Key_Delay => Key_Delay);
      Key_Event (Virtual_Key_Code, Key_Up => True, Key_Delay => Key_Delay);
      Key_Event (Key_States.VK_MENU, Key_Up => True, Key_Delay => Key_Delay);
   end Alt_Key_Stroke;

   procedure Key_Stroke
     (Key       : in     Character_128;
      Shifted   : in out Boolean;
      Key_Delay : in     Duration      := Default_Delay)
   is
      use ASCII, Ada.Characters.Handling, Key_States;
      procedure Set_Shifted;

      procedure Set_Shifted
      is begin
         if not Shifted then
            Key_Event (VK_SHIFT, Key_Up => False, Key_Delay => 2 * Key_Delay);
            Shifted := True;
         end if;
      end Set_Shifted;

      procedure Set_Not_Shifted;

      procedure Set_Not_Shifted
      is begin
         if Shifted then
            Key_Event (VK_SHIFT, Key_Up => True, Key_Delay => 2 * Key_Delay);
            Shifted := False;
         end if;
      end Set_Not_Shifted;
   begin
      --  Use case statement to ensure we handle all characters
      case Key is
      when Nul | Soh | Stx | Eot | Enq | Ack | Bel | Lf | Vt | So | Si |
         Dle | Dc1 | Dc2 | Dc3 | Dc4 | Nak | Syn | Etb |
         Can | Em | Sub | Esc | Fs | Gs | Rs | Us =>
         null;

      when ' ' =>
         Set_Not_Shifted;
         Key_Stroke (VK_SPACE, Key_Delay);

      when '!' =>
         Set_Shifted;
         Key_Stroke (VK_1, Key_Delay);

      when '"' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_7, Key_Delay);

      when '#' =>
         Set_Shifted;
         Key_Stroke (VK_3, Key_Delay);

      when '$' =>
         Set_Shifted;
         Key_Stroke (VK_4, Key_Delay);

      when '%' =>
         Set_Shifted;
         Key_Stroke (VK_5, Key_Delay);

      when '&' =>
         Set_Shifted;
         Key_Stroke (VK_7, Key_Delay);

      when ''' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_7, Key_Delay);

      when '(' =>
         Set_Shifted;
         Key_Stroke (VK_9, Key_Delay);

      when ')' =>
         Set_Shifted;
         Key_Stroke (VK_0, Key_Delay);

      when '*' =>
         Set_Shifted;
         Key_Stroke (VK_8, Key_Delay);

      when '+' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_PLUS, Key_Delay);

      when ',' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_COMMA, Key_Delay);

      when '-' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_MINUS, Key_Delay);

      when '.' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_PERIOD, Key_Delay);

      when '/' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_2, Key_Delay);

      when Etx | Bs | Ht | Ff | Cr |
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        =>
         Set_Not_Shifted;
         Key_Stroke (To_Virtual (Key), Key_Delay);

      when ':' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_1, Key_Delay);

      when ';' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_1, Key_Delay);

      when '<' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_COMMA, Key_Delay);

      when '=' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_PLUS, Key_Delay);

      when '>' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_PERIOD, Key_Delay);

      when '?' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_2, Key_Delay);

      when '@' =>
         Set_Shifted;
         Key_Stroke (VK_2, Key_Delay);

      when  'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
         'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' |
         'W' | 'X' | 'Y' | 'Z' =>
         Set_Shifted;
         Key_Stroke (To_Virtual (Key), Key_Delay);

      when '[' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_4, Key_Delay);

      when '\' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_5, Key_Delay);

      when ']' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_6, Key_Delay);

      when '^' =>
         Set_Shifted;
         Key_Stroke (VK_6, Key_Delay);

      when '_' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_MINUS, Key_Delay);

      when '`' =>
         Set_Not_Shifted;
         Key_Stroke (VK_OEM_3, Key_Delay);

      when  'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
         'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
         'w' | 'x' | 'y' | 'z' =>
         Set_Not_Shifted;
         Key_Stroke (To_Virtual (To_Upper (Key)), Key_Delay);

      when '{' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_4, Key_Delay);

      when '|' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_5, Key_Delay);

      when '}' =>
         Set_Shifted;
         Key_Stroke (VK_OEM_6, Key_Delay);

      when '~' =>
         Key_Stroke (VK_OEM_3, Key_Delay);
      when Del =>
         Set_Not_Shifted;
         Key_Stroke (VK_DELETE, Key_Delay);

      end case;

   end Key_Stroke;

   procedure Key_Stroke
     (Keys      : in String;
      Key_Delay : in Duration := Default_Delay)
   is
      use Ada.Characters.Handling;
      Shifted_Up : Boolean := False;
   begin
      --  Sometimes the current shift state gets out of sync; force
      --  not shifted.
      Key_Event
        (GWindows.Key_States.VK_SHIFT,
         Key_Up => True,
         Key_Delay => 2 * Key_Delay);

      for I in Keys'Range loop
         if Keys (I) in Character_128 then
            Key_Stroke (Keys (I), Shifted_Up, Small_Delay);
         end if;
      end loop;

      if Shifted_Up then
         Key_Event
           (GWindows.Key_States.VK_SHIFT,
            Key_Up => True,
            Key_Delay => Key_Delay);
      end if;
   end Key_Stroke;

   ----------
   --  Mouse events

   function To_Mickeys
     (Point : in GWindows.Types.Point_Type)
     return GWindows.Types.Point_Type;

   function To_Mickeys
     (Point : in GWindows.Types.Point_Type)
     return GWindows.Types.Point_Type
   is
      Screen_X : constant Integer :=
        Metrics.Get_System_Metric (Metrics.SM_CXSCREEN);
      Screen_Y : constant Integer :=
        Metrics.Get_System_Metric (Metrics.SM_CYSCREEN);
   begin
      return
         (X => Point.X * 65_536 / Screen_X,
          Y => Point.Y * 65_536 / Screen_Y);
   end To_Mickeys;

   type Win32_Mouse_Event_Flags_Type is record
      Move        : Boolean;
      Left_Down   : Boolean;
      Left_Up     : Boolean;
      Right_Down  : Boolean;
      Right_Up    : Boolean;
      Middle_Down : Boolean;
      Middle_Up   : Boolean;
      Reserved_1  : Boolean;
      Reserved_2  : Boolean;
      Reserved_3  : Boolean;
      Reserved_4  : Boolean;
      Wheel       : Boolean;
      Absolute    : Boolean;
      Reserved_5  : Boolean;
      Reserved_6  : Boolean;
      Reserved_7  : Boolean;
   end record;

   for Win32_Mouse_Event_Flags_Type use record
      Move        at 0 range  0 ..  0;
      Left_Down   at 0 range  1 ..  1;
      Left_Up     at 0 range  2 ..  2;
      Right_Down  at 0 range  3 ..  3;
      Right_Up    at 0 range  4 ..  4;
      Middle_Down at 0 range  5 ..  5;
      Middle_Up   at 0 range  6 ..  6;
      Reserved_1  at 0 range  7 ..  7;
      Reserved_2  at 0 range  8 ..  8;
      Reserved_3  at 0 range  9 ..  9;
      Reserved_4  at 0 range 10 .. 10;
      Wheel       at 0 range 11 .. 11;
      Reserved_5  at 0 range 12 .. 12;
      Reserved_6  at 0 range 13 .. 13;
      Reserved_7  at 0 range 14 .. 14;
      Absolute    at 0 range 15 .. 15;
   end record;
   pragma Convention (C_Pass_By_Copy, Win32_Mouse_Event_Flags_Type);

   procedure Win32_Mouse_Event
      (DwFlags     : in Win32_Mouse_Event_Flags_Type;
       Dx          : in Interfaces.Integer_32;
       Dy          : in Interfaces.Integer_32;
       CButtons    : in Interfaces.Unsigned_32   := 0;
       DwExtraInfo : in Interfaces.Unsigned_32);
   --  Winuser.h has unsigned_long for Dx, Dy; but relative motion has
   --  to be signed!
   pragma Import (StdCall, Win32_Mouse_Event, "mouse_event");

   procedure Mouse_Event
      (Move        : in Boolean := False;
       Left_Down   : in Boolean := False;
       Left_Up     : in Boolean := False;
       Right_Down  : in Boolean := False;
       Right_Up    : in Boolean := False;
       Middle_Down : in Boolean := False;
       Middle_Up   : in Boolean := False;
       Wheel       : in Boolean := False;
       Absolute    : in Boolean := False;
       Motion      : in GWindows.Types.Point_Type := (0, 0))
   is
      Mickeys : GWindows.Types.Point_Type := To_Mickeys (Motion);
   begin
      if Absolute then
         Mickeys := To_Mickeys (Motion);
      else
         Mickeys := Motion;
      end if;

      Win32_Mouse_Event
        (DwFlags        =>
           (Move        => Move,
            Left_Down   => Left_Down,
            Left_Up     => Left_Up,
            Right_Down  => Right_Down,
            Right_Up    => Right_Up,
            Middle_Down => Middle_Down,
            Middle_Up   => Middle_Up,
            Wheel       => Wheel,
            Absolute    => Absolute,
            others      => False),
         Dx            => Interfaces.Integer_32 (Mickeys.X),
         Dy            => Interfaces.Integer_32 (Mickeys.Y),
         CButtons      => 0,
         DwExtraInfo   => 0);

   end Mouse_Event;

end GWindows.Testing.Events;
