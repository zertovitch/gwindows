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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  This package enables applications to add, modify and delete a system tray
--  icon. The system tray is the portion usually visible on the bottom right
--  of the Windows desktop, with the clock, loudspeaker icon and LAN icon.
--  The "system tray" is originally documented in Windows as
--  "notification area", is abbreviated as "systray" and is
--  sometimes also called the "status area".

with GWindows.Types, GNATCOM.Types;
with GWindows.Base;
with GWindows.Drawing_Objects;

package GWindows.System_Tray is

   type Notify_Icon_Data is tagged private;

   ----------------------------------------------------------------
   --  The data setup part. No action so far, just preparation.  --
   ----------------------------------------------------------------

   procedure Set_Window (
      Data   : in out Notify_Icon_Data;
      Window : GWindows.Base.Base_Window_Type'Class
   );

   procedure Set_Icon (
      Data   : in out Notify_Icon_Data;
      Icon   : GWindows.Drawing_Objects.Icon_Type;
      ID     : Natural  --  Application-defined identifier of the taskbar icon.
   );

   procedure Clear_Icon (Data : in out Notify_Icon_Data);

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

   WM_TRAY_MESSAGE : constant Integer;
   --  This is the default message sent to the window indicated at Set_Window
   --  If you want a distinct message per icon, pass
   --  WM_TRAY_MESSAGE + i with i = 0,1,2,... to the following procedure.

   procedure Set_Windows_Messaging (
      Data    : in out Notify_Icon_Data;
      Message :        Integer := WM_TRAY_MESSAGE
   );

   procedure Clear_Windows_Messaging (
      Data   : in out Notify_Icon_Data
   );

   ---------------------------------------------------------------
   --  The action part: add, modify, delete a system tray icon  --
   ---------------------------------------------------------------

   type Notify_Icon_Action is (Add, Modify, Delete, Set_Focus, Set_Version);

   procedure Notify_Icon (
      Data   : Notify_Icon_Data;
      Action : Notify_Icon_Action
   );

   Notify_Action_Failed : exception;

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

   WM_USER : constant := 16#400#;
   WM_TRAY_MESSAGE : constant Integer := WM_USER + 140;
   --  ^ the value added is just happening to be larger than elsewhere GWindows

   type NOTIFYICONDATA is record
      cbSize           : Interfaces.C.unsigned := NOTIFYICONDATA'Size / 8;
      hWnd             : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      uID              : Interfaces.C.unsigned;
      uFlags           : Interfaces.C.unsigned := 0;
      uCallbackMessage : Interfaces.C.unsigned :=
                            Interfaces.C.unsigned (WM_TRAY_MESSAGE);
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
