--  Abstract :
--
--  Facilities for generating keyboard events. Mainly used in testing.
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

with GWindows.Types;
package GWindows.Testing.Events is

   Default_Delay : Duration := 0.02;
   --  Good for my 1 GHz laptop.

   procedure Key_Event
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Up           : in Boolean;
      Key_Delay        : in Duration := Default_Delay);
   --  Generate a key-up or key-down event. Virtual_Key_Code is from
   --  GWindows.Key_States. The scan code is not correct. Delay for
   --  Key_Delay after the event, to let the event be processed.

   procedure Key_Stroke
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Delay        : in Duration := Default_Delay);
   --  Generate key-down and key-up events. The scan code is not
   --  correct.

   procedure Alt_Key_Stroke
     (Virtual_Key_Code : in Interfaces.Unsigned_8;
      Key_Delay        : in Duration := Default_Delay);
   --  Generate key-down and key-up events, with alt key (VK_MENU)
   --  down. The scan code is not correct.

   subtype Character_128 is Character range ASCII.Nul .. ASCII.Del;

   procedure Key_Stroke
     (Key       : in     Character_128;
      Shifted   : in out Boolean;
      Key_Delay : in     Duration      := Default_Delay);
   --  Generate key-down and key-up events for Key, with shift events
   --  as necessary. If Shifted is true, a VK_SHIFT (key_up => False)
   --  event has been sent. Shifted is modified if VK_SHIFT events are
   --  sent. Key_Delay is used between shift events and key events.

   procedure Key_Stroke
     (Keys      : in String;
      Key_Delay : in Duration := Default_Delay);
   --  Generate a sequence of key-down, key-up events to type Keys,
   --  with Key_Delay in between. Shift events are used to generate
   --  uppercase letters.
   --
   --  Ignores characters not in Characters_128, or for which there is
   --  no key code.

   ----------
   --  Mouse events
   --
   --  For absolute mouse moves, the screen has coordinates (0, 0) for
   --  left top and (65,636, 65,536) for right bottom (65,536 Mickeys
   --  across the screen).

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
       Motion      : in GWindows.Types.Point_Type := (0, 0));

end GWindows.Testing.Events;
