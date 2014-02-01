------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . S Y S T E M _ T R A Y                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                  Copyright (C) 2014 Gautier de Montmollin                --
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

   ---------------------------
   --  The data setup part  --
   ---------------------------

   procedure Set_Window (
      Data   : in out Notify_Icon_Data;
      Window : GWindows.Base.Base_Window_Type'Class
   );

   procedure Set_Icon (
      Data   : in out Notify_Icon_Data;
      Icon   : GWindows.Drawing_Objects.Icon_Type;
      ID     : Natural  --  Application-defined identifier of the taskbar icon.
   );

   procedure Set_Tool_Tip (
      Data   : in out Notify_Icon_Data;
      Text   : GString
   );

   procedure Set_Balloon_Icon (
      Data   : in out Notify_Icon_Data;
      Icon   : GWindows.Drawing_Objects.Icon_Type
   );

   --  This Icon will be seen when User_Icon is used as parameter in
   --  in Set_Balloon.

   type Notify_Balloon_Icon_Type is
     (No_Icon, Information_Icon, Warning_Icon, Error_Icon, User_Icon);

   procedure Set_Balloon (
      Data   : in out Notify_Icon_Data;
      Text   : GString;
      Title  : GString := "";
      Icon   : Notify_Balloon_Icon_Type := No_Icon
   );

   --  NB: User_Icon not working so far.

   ---------------------------------------------------------------
   --  The action part: add, modify, delete a system tray icon  --
   ---------------------------------------------------------------

   type Notify_Icon_Action is (Add, Modify, Delete, Set_Focus, Set_Version);

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
   subtype Tip_Text is GString_C (0 .. Max_Tip_Text);

   --  Balloon notification
   Max_Info_Text : constant := 255;
   subtype Info_Text is GString_C (0 .. Max_Info_Text);

   --  Title for a balloon notification
   Max_Info_Title_Text : constant := 63;
   subtype Info_Title_Text is GString_C (0 .. Max_Info_Title_Text);

   use type Interfaces.C.unsigned;

   type NOTIFYICONDATA is record
      cbSize           : Interfaces.C.unsigned := NOTIFYICONDATA'Size / 8;
      hWnd             : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      uID              : Interfaces.C.unsigned;
      uFlags           : Interfaces.C.unsigned := 0;
      uCallbackMessage : Interfaces.C.unsigned := 0;
      hIcon            : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      szTip            : Tip_Text              := (others => GString_C_Null);
      dwState          : GWindows.Types.Lparam := 0;
      dwStateMask      : GWindows.Types.Lparam := 0;
      szInfo           : Info_Text             := (others => GString_C_Null);
      uTimeout_Version : Interfaces.C.unsigned := 0;
      szInfoTitle      : Info_Title_Text       := (others => GString_C_Null);
      dwInfoFlags      : GWindows.Types.Lparam := 0;
      GUID             : GNATCOM.Types.GUID    := GNATCOM.Types.GUID_NULL;
      hBalloonIcon     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
   end record;

   type Notify_Icon_Data is tagged record
      C_data : aliased NOTIFYICONDATA;
   end record;

end GWindows.System_Tray;
