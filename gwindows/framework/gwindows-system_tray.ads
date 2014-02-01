------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . S Y S T E M _ T R A Y                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2014 Gautier de Montmollin              --
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
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  This package enables applications to add, modify and delete a system tray
--  icon. The system tray is the portion usually visible on the bottom right
--  of the Windows desktop, with the clock, loudspeaker icon and LAN icon.
--  The system tray is documented in Windows as notification area and is
--  sometimes also called the status area.

with GWindows.Types, GNATCOM.Types;
with GWindows.Base;
with GWindows.Drawing_Objects;

package GWindows.System_Tray is

   type Notify_Icon_Data is tagged private;

   type Notify_Icon_Action is (Add, Modify, Delete, Set_Focus, Set_Version);

   procedure Set_Window (
     Data   : in out Notify_Icon_Data;
     Window : GWindows.Base.Base_Window_Type'Class
   );

   procedure Set_Icon (
     Data   : in out Notify_Icon_Data;
     Icon   : GWindows.Drawing_Objects.Icon_Type
   );

   ---------------------------------------------------------------
   --  The action part: add, modify, delete a system tray icon  --
   ---------------------------------------------------------------

   procedure Notify_Icon (
     Data   : Notify_Icon_Data;
     Action : Notify_Icon_Action
   );

private

   --  Notify icon data:
   --  http://msdn.microsoft.com/en-us/library/
   --    windows/desktop/bb773352(v=vs.85).aspx

   --  Tool tip
   Max_Tip_Text : constant := 127;
   type Tip_Text is new GString_C (0 .. Max_Tip_Text);

   --  Balloon notification
   Max_Info_Text : constant := 255;
   type Info_Text is new GString_C (0 .. Max_Info_Text);

   --  Title for a balloon notification
   Max_Info_Title_Text : constant := 63;
   type Info_Title_Text is new GString_C (0 .. Max_Info_Title_Text);

   use type Interfaces.C.unsigned;

   type NOTIFYICONDATA is record
      cbSize           : Interfaces.C.unsigned := NOTIFYICONDATA'Size / 8;
      hWnd             : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      uID              : Interfaces.C.unsigned;
      uFlags           : Interfaces.C.unsigned;
      uCallbackMessage : Interfaces.C.unsigned;
      hIcon            : GWindows.Types.Handle;
      szTip            : Tip_Text;
      dwState          : GWindows.Types.Lparam;
      dwStateMask      : GWindows.Types.Lparam;
      szInfo           : Info_Text;
      uTimeout         : Interfaces.C.unsigned;
      uVersion         : Interfaces.C.unsigned;
      szInfoTitle      : Info_Title_Text;
      dwInfoFlags      : GWindows.Types.Lparam;
      GUID             : GNATCOM.Types.GUID      := GNATCOM.Types.GUID_NULL;
      hBalloonIcon     : GWindows.Types.Handle;
   end record;

   type Notify_Icon_Data is tagged record
      C_data : aliased NOTIFYICONDATA;
   end record;

end GWindows.System_Tray;
