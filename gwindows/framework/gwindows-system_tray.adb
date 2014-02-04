------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . S Y S T E M _ T R A Y                --
--                                                                          --
--                                 B o d y                                  --
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

with GWindows.GStrings;                 use GWindows.GStrings;

package body GWindows.System_Tray  is

   --  Flags

   --  NIF_MESSAGE  : constant := 16#01#;  --  uCallbackMessage is valid.
   NIF_ICON     : constant := 16#02#;  --  The hIcon member is valid.
   NIF_TIP      : constant := 16#04#;  --  The szTip member is valid.
   --  NIF_STATE    : constant := 16#08#;  --  dwState and dwStateMask valid.
   NIF_INFO     : constant := 16#10#;  --  Display a balloon notification.
   --  NIF_GUID     : constant := 16#20#;
         --  Windows 7 and later: The guidItem is valid;
         --  Windows Vista and earlier: Reserved.
   --  NIF_REALTIME : constant := 16#40#;
         --  If the balloon notification cannot be
         --  displayed immediately, discard it.
   --  NIF_SHOWTIP  : constant := 16#80#;  --  Use the standard tooltip.

   --  State

   --  NIS_HIDDEN       : constant := 1; -- The icon is hidden.
   --  NIS_SHAREDICON   : constant := 2;
         --  The icon resource is shared between multiple icons.

   procedure Set_Window (
     Data   : in out Notify_Icon_Data;
     Window : GWindows.Base.Base_Window_Type'Class
   )
   is
     use GWindows.Base;
   begin
     Data.C_data.hWnd := Handle (Window);
   end Set_Window;

   --  Info Flags

   --  NIIF_NONE    : constant := 16#00#; --  No icon.
   --  NIIF_INFO    : constant := 16#01#; --  An information icon.
   --  NIIF_WARNING : constant := 16#02#; --  A warning icon.
   --  NIIF_ERROR   : constant := 16#03#; --  An error icon.
   --  NIIF_USER    : constant := 16#04#;
      --  Windows XP: Use the icon identified in hIcon as
      --              the notification balloon's title icon.
      --  Windows Vista and later: Use the icon identified
      --              in hBalloonIcon as the notification
      --              balloon's title icon.
   --  NIIF_NOSOUND            : constant := 16#10#;
   --  NIIF_LARGE_ICON         : constant := 16#20#;
   --  NIIF_RESPECT_QUIET_TIME : constant := 16#80#;
      --  Do not display the balloon notification if the
      --  current user is in "quiet time"
   --  NIIF_ICON_MASK          : constant := 16#0F#;

   procedure Set_Icon (
     Data   : in out Notify_Icon_Data;
     Icon   : GWindows.Drawing_Objects.Icon_Type;
     ID     : Natural  --  Application-defined identifier of the taskbar icon.
   )
   is
     use GWindows.Drawing_Objects;
   begin
     Data.C_data.hIcon  := Handle (Icon);
     Data.C_data.uID    := Interfaces.C.unsigned (ID);
     Data.C_data.uFlags := Data.C_data.uFlags or NIF_ICON;
   end Set_Icon;

   procedure Set_Tool_Tip (
     Data   : in out Notify_Icon_Data;
     Text   : GString
   )
   is
   begin
     To_GString_C (Text, Data.C_data.szTip);
     Data.C_data.uFlags := Data.C_data.uFlags or NIF_TIP;
   end Set_Tool_Tip;

   procedure Set_Balloon_Icon (
      Data   : in out Notify_Icon_Data;
      Icon   : GWindows.Drawing_Objects.Icon_Type
   )
   is
     use GWindows.Drawing_Objects;
   begin
     Data.C_data.hBalloonIcon := Handle (Icon);
   end Set_Balloon_Icon;

   procedure Set_Balloon (
      Data   : in out Notify_Icon_Data;
      Text   : GString;
      Title  : GString := "";
      Icon   : Notify_Balloon_Icon_Type := No_Icon
   )
   is
   begin
     To_GString_C (Text, Data.C_data.szInfo);
     To_GString_C (Title, Data.C_data.szInfoTitle);
     Data.C_data.uTimeout_Version := 4;
     Data.C_data.dwInfoFlags := Notify_Balloon_Icon_Type'Pos (Icon);
     Data.C_data.uFlags := Data.C_data.uFlags or NIF_INFO;
   end Set_Balloon;

   procedure Notify_Icon (
     Data   : Notify_Icon_Data;
     Action : Notify_Icon_Action
   )
   is

      Data_Copy : Notify_Icon_Data := Data;

      type LP is access all NOTIFYICONDATA;

      --  Shell_NotifyIcon:
      --  http://msdn.microsoft.com/en-us/library/
      --    windows/desktop/bb762159%28v=vs.85%29.aspx

      function Shell_NotifyIcon
        (dwMessage        : GWindows.Types.Lparam :=
                               Notify_Icon_Action'Pos (Action);
         PNOTIFYICONDATA  : LP := Data_Copy.C_data'Access)
      return Interfaces.C.int;

      pragma Import (StdCall, Shell_NotifyIcon,
                     "Shell_NotifyIcon" & Character_Mode_Identifier);

      use type Interfaces.C.int;
   begin
      if Shell_NotifyIcon = 0 then
         raise Notify_Action_Failed;
      end if;
   end Notify_Icon;

end GWindows.System_Tray;
