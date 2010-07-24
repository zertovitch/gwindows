------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . K E Y  _ S T A T E S                   --
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

package GWindows.Key_States is

   --  Virtual key codes from "Win32 Programming", Brent E. Rector,
   --  Joseph M. Newcomer, table 7.3
   --
   --  Codes not in "Win32 programming" are from winuser.h

   VK_LBUTTON    : constant := 1;
   --  Virtual key: Left mouse button
   VK_RBUTTON    : constant := 2;
   --  Virtual key: Right mouse button
   VK_CANCEL     : constant := 3;
   --  Virtual key: Used for control+break processing
   VK_MBUTTON    : constant := 4;
   --  Virtual key: Middle mouse button
   VK_BACK       : constant := 8;
   --  Virtual key: Backspace
   VK_TAB        : constant := 9;
   --  Virtual key: Tab
   VK_CLEAR      : constant := 12;
   --  Virtual key: Clear
   VK_RETURN     : constant := 13;
   --  Virtual key: Enter
   VK_SHIFT      : constant := 16;
   --  Virtual key: Shift
   VK_CONTROL    : constant := 17;
   --  Virtual key: Ctrl
   VK_MENU       : constant := 18;
   --  Virtual key: Alt
   VK_PAUSE      : constant := 19;
   --  Virtual key: Pause
   VK_CAPITAL    : constant := 20;
   --  Virtual key: Caps Lock
   VK_ESCAPE     : constant := 27;
   --  Virtual key: Esc
   VK_SPACE      : constant := 32;
   --  Virtual key: Spacebar
   VK_PRIOR      : constant := 33;
   --  Virtual key: Page Up
   VK_NEXT       : constant := 34;
   --  Virtual key: Page Down
   VK_END        : constant := 35;
   --  Virtual key: End
   VK_HOME       : constant := 36;
   --  Virtual key: Home
   VK_LEFT       : constant := 37;
   --  Virtual key: Left Arrow
   VK_UP         : constant := 38;
   --  Virtual key: Right Arrow
   VK_RIGHT      : constant := 39;
   --  Virtual key: Up Arrow
   VK_DOWN       : constant := 40;
   --  Virtual key: Down Arrow
   VK_SELECT     : constant := 41;
   --  Virtual key: Select
   VK_PRINT      : constant := 42;
   --  Virtual key: OEM Specific
   VK_EXECUTE    : constant := 43;
   --  Virtual key: Execute
   VK_SNAPSHOT   : constant := 44;
   --  Virtual key: PrtScr
   VK_INSERT     : constant := 45;
   --  Virtual key: Ins
   VK_DELETE     : constant := 46;
   --  Virtual key: Del
   VK_HELP       : constant := 47;
   --  Virtual key: Help
   VK_0          : constant := 48;
   --  Virtual key: 0
   VK_1          : constant := 49;
   --  Virtual key: 1
   VK_2          : constant := 50;
   --  Virtual key: 2
   VK_3          : constant := 51;
   --  Virtual key: 3
   VK_4          : constant := 52;
   --  Virtual key: 4
   VK_5          : constant := 53;
   --  Virtual key: 5
   VK_6          : constant := 54;
   --  Virtual key: 6
   VK_7          : constant := 55;
   --  Virtual key: 7
   VK_8          : constant := 56;
   --  Virtual key: 8
   VK_9          : constant := 57;
   --  Virtual key: 9
   VK_A          : constant := 65;
   --  Virtual key: A
   VK_B          : constant := 66;
   --  Virtual key: B
   VK_C          : constant := 67;
   --  Virtual key: C
   VK_D          : constant := 68;
   --  Virtual key: D
   VK_E          : constant := 69;
   --  Virtual key: E
   VK_F          : constant := 70;
   --  Virtual key: F
   VK_G          : constant := 71;
   --  Virtual key: G
   VK_H          : constant := 72;
   --  Virtual key: H
   VK_I          : constant := 73;
   --  Virtual key: I
   VK_J          : constant := 74;
   --  Virtual key: J
   VK_K          : constant := 75;
   --  Virtual key: K
   VK_L          : constant := 76;
   --  Virtual key: L
   VK_M          : constant := 77;
   --  Virtual key: M
   VK_N          : constant := 78;
   --  Virtual key: N
   VK_O          : constant := 79;
   --  Virtual key: O
   VK_P          : constant := 80;
   --  Virtual key: P
   VK_Q          : constant := 81;
   --  Virtual key: Q
   VK_R          : constant := 82;
   --  Virtual key: R
   VK_S          : constant := 83;
   --  Virtual key: S
   VK_T          : constant := 84;
   --  Virtual key: T
   VK_U          : constant := 85;
   --  Virtual key: U
   VK_V          : constant := 86;
   --  Virtual key: V
   VK_W          : constant := 87;
   --  Virtual key: W
   VK_X          : constant := 88;
   --  Virtual key: X
   VK_Y          : constant := 89;
   --  Virtual key: Y
   VK_Z          : constant := 90;
   --  Virtual key: Z
   VK_NUMPAD0    : constant := 96;
   --  Virtual key: 0 on number pad
   VK_NUMPAD1    : constant := 97;
   --  Virtual key: 1 on number pad
   VK_NUMPAD2    : constant := 98;
   --  Virtual key: 2 on number pad
   VK_NUMPAD3    : constant := 99;
   --  Virtual key: 3 on number pad
   VK_NUMPAD4    : constant := 100;
   --  Virtual key: 4 on number pad
   VK_NUMPAD5    : constant := 101;
   --  Virtual key: 5 on number pad
   VK_NUMPAD6    : constant := 102;
   --  Virtual key: 6 on number pad
   VK_NUMPAD7    : constant := 103;
   --  Virtual key: 7 on number pad
   VK_NUMPAD8    : constant := 104;
   --  Virtual key: 8 on number pad
   VK_NUMPAD9    : constant := 105;
   --  Virtual key: 9 on number pad
   VK_MULTIPLY   : constant := 106;
   --  Virtual key: Multiply
   VK_ADD        : constant := 107;
   --  Virtual key: Add
   VK_SEPARATOR  : constant := 108;
   --  Virtual key: Separator
   VK_SUBTRACT   : constant := 109;
   --  Virtual key: Subtract
   VK_DECIMAL    : constant := 110;
   --  Virtual key: Decimal
   VK_DIVIDE     : constant := 111;
   --  Virtual key: Divide
   VK_F1         : constant := 112;
   --  Virtual key: F1
   VK_F2         : constant := 113;
   --  Virtual key: F2
   VK_F3         : constant := 114;
   --  Virtual key: F3
   VK_F4         : constant := 115;
   --  Virtual key: F4
   VK_F5         : constant := 116;
   --  Virtual key: F5
   VK_F6         : constant := 117;
   --  Virtual key: F6
   VK_F7         : constant := 118;
   --  Virtual key: F7
   VK_F8         : constant := 119;
   --  Virtual key: F8
   VK_F9         : constant := 120;
   --  Virtual key: F9
   VK_F10        : constant := 121;
   --  Virtual key: F10
   VK_F11        : constant := 122;
   --  Virtual key: F11
   VK_F12        : constant := 123;
   --  Virtual key: F12
   VK_F13        : constant := 124;
   --  Virtual key: F13
   VK_F14        : constant := 125;
   --  Virtual key: F14
   VK_F15        : constant := 126;
   --  Virtual key: F15
   VK_F16        : constant := 127;
   --  Virtual key: F16
   VK_F17        : constant := 128;
   --  Virtual key: F17
   VK_F18        : constant := 129;
   --  Virtual key: F18
   VK_F19        : constant := 130;
   --  Virtual key: F19
   VK_F20        : constant := 131;
   --  Virtual key: F20
   VK_F21        : constant := 132;
   --  Virtual key: F21
   VK_F22        : constant := 133;
   --  Virtual key: F22
   VK_F23        : constant := 134;
   --  Virtual key: F23
   VK_F24        : constant := 135;
   --  Virtual key: F24
   VK_NUMLOCK    : constant := 144;
   --  Virtual key: NumLock
   VK_SCROLL     : constant := 145;
   --  Virtual key: ScrollLock
   VK_LSHIFT     : constant := 160;
   VK_RSHIFT     : constant := 161;
   VK_LCONTROL   : constant := 162;
   VK_RCONTROL   : constant := 163;
   VK_LMENU      : constant := 164;
   VK_RMENU      : constant := 165;
      --  A6 .. B9 undefined
   VK_OEM_1      : constant := 16#BA#; -- ; :
   VK_OEM_PLUS   : constant := 16#BB#; -- = +
   VK_OEM_COMMA  : constant := 16#BC#; -- , <
   VK_OEM_MINUS  : constant := 16#BD#; -- - _
   VK_OEM_PERIOD : constant := 16#BE#; -- . >
   VK_OEM_2      : constant := 16#BF#; -- / ?
   VK_OEM_3      : constant := 16#C0#; -- ` ~
   --  C1 .. DA undefined
   VK_OEM_4      : constant := 16#DB#; -- [ {
   VK_OEM_5      : constant := 16#DC#; -- \ |
   VK_OEM_6      : constant := 16#DD#; -- ] {
   VK_OEM_7      : constant := 16#DE#; -- ' "
   --  DF .. E3 undefined
   VK_OEM_8      : constant := 16#E4#;
   VK_PROCESSKEY : constant := 229;
   VK_ATTN       : constant := 246;
   VK_CRSEL      : constant := 247;
   VK_EXSEL      : constant := 248;
   VK_EREOF      : constant := 249;
   VK_PLAY       : constant := 250;
   VK_ZOOM       : constant := 251;
   VK_NONAME     : constant := 252;
   VK_PA1        : constant := 253;
   VK_OEM_CLEAR  : constant := 254;

   function Is_Key_Down (Virtual_Key_Code : Integer)
                        return Boolean;

   function Is_Key_Toggled (Virtual_Key_Code : Integer)
                           return Boolean;

end GWindows.Key_States;
