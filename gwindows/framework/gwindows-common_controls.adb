------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--      G W I N D O W S . W I N D O W S . C O M M O N _ C O N T R O L S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2013 David Botton                   --
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

with Ada.Unchecked_Conversion;

with System;

with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;
with GWindows.GStrings.Unbounded;
with GWindows.Internal;
with GWindows.Utilities;

package body GWindows.Common_Controls is
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   ACS_CENTER                   : constant := 1;
--     ACS_TRANSPARENT              : constant := 2;
--     ACS_AUTOPLAY                 : constant := 4;
   ACM_OPENA                    : constant := 1124;
   ACM_OPENW                    : constant := 1127;
   ACM_PLAY                     : constant := 1125;
   ACM_STOP                     : constant := 1126;
   ACN_START                    : constant := 1;
   ACN_STOP                     : constant := 2;

   DTM_FIRST                    : constant := 16#1000#;
   DTM_GETSYSTEMTIME            : constant := DTM_FIRST + 1;
   DTM_SETSYSTEMTIME            : constant := DTM_FIRST + 2;
   DTM_SETRANGE                 : constant := DTM_FIRST + 4;
   DTM_SETFORMAT                : constant := DTM_FIRST + 5;

   GDT_VALID                    : constant := 0;
   GDT_NONE                     : constant := 1;

   GDTR_MIN                     : constant := 1;
   GDTR_MAX                     : constant := 2;

   WM_USER                      : constant := 16#400#;
--     PBM_SETRANGE                 : constant := (WM_USER + 1);
   PBM_SETPOS                   : constant := (WM_USER + 2);
   PBM_DELTAPOS                 : constant := (WM_USER + 3);
   PBM_SETSTEP                  : constant := (WM_USER + 4);
   PBM_STEPIT                   : constant := (WM_USER + 5);
   PBM_SETRANGE32               : constant := (WM_USER + 6);
--     PBM_GETRANGE                 : constant := (WM_USER + 7);
   PBM_GETPOS                   : constant := (WM_USER + 8);
--     PBM_SETBARCOLOR              : constant := (WM_USER + 9);
--     PBM_SETBKCOLOR               : constant := 16#2001#;

   UDM_SETRANGE                 : constant := (WM_USER + 101);
--     UDM_GETRANGE                 : constant := (WM_USER + 102);
   UDM_SETPOS                   : constant := (WM_USER + 103);
--     UDM_GETPOS                   : constant := (WM_USER + 104);
--     UDM_SETBUDDY                 : constant := (WM_USER + 105);
--     UDM_GETBUDDY                 : constant := (WM_USER + 106);
--     UDM_SETACCEL                 : constant := (WM_USER + 107);
--     UDM_GETACCEL                 : constant := (WM_USER + 108);
--     UDM_SETBASE                  : constant := (WM_USER + 109);
--     UDM_GETBASE                  : constant := (WM_USER + 110);

   TBM_GETPOS              : constant := (WM_USER);
   TBM_GETRANGEMIN         : constant := (WM_USER + 1);
   TBM_GETRANGEMAX         : constant := (WM_USER + 2);
--    TBM_GETTIC              : constant := (WM_USER + 3);
--    TBM_SETTIC              : constant := (WM_USER + 4);
   TBM_SETPOS              : constant := (WM_USER + 5);
--    TBM_SETRANGE            : constant := (WM_USER + 6);
   TBM_SETRANGEMIN         : constant := (WM_USER + 7);
   TBM_SETRANGEMAX         : constant := (WM_USER + 8);
--     TBM_CLEARTICS           : constant := (WM_USER + 9);
--     TBM_SETSEL              : constant := (WM_USER + 10);
--     TBM_SETSELSTART         : constant := (WM_USER + 11);
--     TBM_SETSELEND           : constant := (WM_USER + 12);
--     TBM_GETPTICS            : constant := (WM_USER + 14);
--     TBM_GETTICPOS           : constant := (WM_USER + 15);
--     TBM_GETNUMTICS          : constant := (WM_USER + 16);
--     TBM_GETSELSTART         : constant := (WM_USER + 17);
--     TBM_GETSELEND           : constant := (WM_USER + 18);
--     TBM_CLEARSEL            : constant := (WM_USER + 19);
--     TBM_SETTICFREQ          : constant := (WM_USER + 20);
--     TBM_SETPAGESIZE         : constant := (WM_USER + 21);
--     TBM_GETPAGESIZE         : constant := (WM_USER + 22);
--     TBM_SETLINESIZE         : constant := (WM_USER + 23);
--     TBM_GETLINESIZE         : constant := (WM_USER + 24);
--     TBM_GETTHUMBRECT        : constant := (WM_USER + 25);
--     TBM_GETCHANNELRECT      : constant := (WM_USER + 26);
--     TBM_SETTHUMBLENGTH      : constant := (WM_USER + 27);
--     TBM_GETTHUMBLENGTH      : constant := (WM_USER + 28);
--     TBM_SETTOOLTIPS         : constant := (WM_USER + 29);
--     TBM_GETTOOLTIPS         : constant := (WM_USER + 30);
--     TBM_SETTIPSIDE          : constant := (WM_USER + 31);

   LVM_FIRST               : constant := 16#1000#;
   LVM_GETITEMCOUNT        : constant := LVM_FIRST + 4;
   LVM_SETITEMA            : constant := LVM_FIRST + 6;
   LVM_SETITEMW            : constant := LVM_FIRST + 76;
   LVM_INSERTITEMA         : constant := LVM_FIRST + 7;
   LVM_INSERTITEMW         : constant := LVM_FIRST + 77;
--     LVM_DELETEITEM          : constant := LVM_FIRST + 8;
   LVM_DELETEALLITEMS      : constant := LVM_FIRST + 9;
   LVM_SETCOLUMNA          : constant := LVM_FIRST + 26;
   LVM_SETCOLUMNW          : constant := LVM_FIRST + 96;
   LVM_INSERTCOLUMNA       : constant := LVM_FIRST + 27;
   LVM_INSERTCOLUMNW       : constant := LVM_FIRST + 97;
   LVM_GETITEMSTATE        : constant := LVM_FIRST + 44;
   LVM_GETSELECTEDCOUNT    : constant := LVM_FIRST + 50;
   LVM_SETIMAGELIST        : constant := LVM_FIRST + 3;  --  AnSp

   LVSIL_NORMAL            : constant := 0;  --  AnSp
   LVSIL_SMALL             : constant := 1;  --  AnSp
   LVSIL_STATE             : constant := 2;  --  AnSp

--     LVIS_FOCUSED            : constant := 16#0001#;
   LVIS_SELECTED           : constant := 16#0002#;
--     LVIS_CUT                : constant := 16#0004#;
--     LVIS_DROPHILITED        : constant := 16#0008#;
--     LVIS_ACTIVATING         : constant := 16#0020#;

--     LVIS_OVERLAYMASK        : constant := 16#0F00#;
--     LVIS_STATEIMAGEMASK     : constant := 16#F000#;

   type SYSTEMTIME is
      record
         wYear         : Interfaces.C.short;
         wMonth        : Interfaces.C.short;
         wDayOfWeek    : Interfaces.C.short := 0;
         wDay          : Interfaces.C.short;
         wHour         : Interfaces.C.short;
         wMinute       : Interfaces.C.short;
         wSecond       : Interfaces.C.short;
         wMilliseconds : Interfaces.C.short := 0;
      end record;

   function Calendar_To_SYSTEMTIME (Time : Ada.Calendar.Time)
                                   return SYSTEMTIME;

   function SYSTEMTIME_To_Calendar (Time : SYSTEMTIME)
                                   return Ada.Calendar.Time;

   LVIF_TEXT               : constant := 16#0001#;
   LVIF_IMAGE              : constant := 16#0002#;
--     LVIF_PARAM              : constant := 16#0004#;
--     LVIF_STATE              : constant := 16#0008#;
--     LVIF_INDENT             : constant := 16#0010#;
--     LVIF_NORECOMPUTE        : constant := 16#0800#;

   type LVITEM is
      record
         Mask      : Interfaces.C.unsigned := 0;
         Item      : Integer := 0;
         SubItem   : Integer := 0;
         State     : Interfaces.C.unsigned := 0;
         StateMask : Interfaces.C.unsigned := 0;
         Text      : LPTSTR := null;
         TextMax   : Integer := 0;
         Image     : Integer;
         lParam    : GWindows.Types.Lparam := 0;
         Indent    : Integer;
         iGroupId  : Integer;
         cColumns  : Interfaces.C.unsigned := 0;
         PuColumns : LPTSTR := null;
      end record;

--     LVCF_FMT                : constant := 16#0001#;
   LVCF_WIDTH              : constant := 16#0002#;
   LVCF_TEXT               : constant := 16#0004#;
--     LVCF_SUBITEM            : constant := 16#0008#;
--     LVCF_IMAGE              : constant := 16#0010#;
--     LVCF_ORDER              : constant := 16#0020#;

--     LVCFMT_LEFT             : constant := 16#0000#;
--     LVCFMT_RIGHT            : constant := 16#0001#;
--     LVCFMT_CENTER           : constant := 16#0002#;
--     LVCFMT_JUSTIFYMASK      : constant := 16#0003#;
--     LVCFMT_IMAGE            : constant := 16#0800#;
--     LVCFMT_BITMAP_ON_RIGHT  : constant := 16#1000#;
--     LVCFMT_COL_HAS_IMAGES   : constant := 16#8000#;

   type LVCOLUMN is
      record
         Mask      : Interfaces.C.unsigned := 0;
         Format    : Interfaces.C.unsigned := 0;
         Width     : Integer := 0;
         Text      : LPTSTR := null;
         TextMax   : Integer := 0;
         SubItem   : Integer := 0;
         Image     : Integer := 0;
         Order     : Integer := 0;
      end record;

   TV_FIRST                : constant := 16#1100#;
   TVM_INSERTITEMA         : constant := TV_FIRST + 0;
   TVM_INSERTITEMW         : constant := TV_FIRST + 50;
   TVM_DELETEITEM          : constant := TV_FIRST + 1;
   TVM_EXPAND              : constant := TV_FIRST + 2;
   TVM_GETNEXTITEM         : constant := TV_FIRST + 10;
   TVM_GETITEMA            : constant := TV_FIRST + 12;
   TVM_GETITEMW            : constant := TV_FIRST + 62;
   TVM_GETCOUNT            : constant := TV_FIRST + 5;  --  AnSp
   TVM_SETITEMA            : constant := TV_FIRST + 13;  --  AnSp
   TVM_SETITEMW            : constant := TV_FIRST + 63;  --  AnSp
   TVM_SETIMAGELIST        : constant := TV_FIRST + 9;  --  AnSp
   TVM_SELECTITEM          : constant := TV_FIRST + 11; -- GdM

   TVGN_ROOT               : constant := 16#0000#;
   TVGN_NEXT               : constant := 16#0001#;
   TVGN_PREVIOUS           : constant := 16#0002#;
   TVGN_PARENT             : constant := 16#0003#;
   TVGN_CHILD              : constant := 16#0004#;
--     TVGN_FIRSTVISIBLE       : constant := 16#0005#;
--     TVGN_NEXTVISIBLE        : constant := 16#0006#;
--     TVGN_PREVIOUSVISIBLE    : constant := 16#0007#;
--     TVGN_DROPHILITE         : constant := 16#0008#;
   TVGN_CARET              : constant := 16#0009#;
--     TVGN_LASTVISIBLE        : constant := 16#000A#;

   TVIF_TEXT               : constant := 16#0001#;
   TVIF_IMAGE              : constant := 16#0002#;  --  AnSp
--     TVIF_PARAM              : constant := 16#0004#;
--     TVIF_STATE              : constant := 16#0008#;
--     TVIF_HANDLE             : constant := 16#0010#;
   TVIF_SELECTEDIMAGE      : constant := 16#0020#;  --  AnSp
--     TVIF_CHILDREN           : constant := 16#0040#;
--     TVIF_INTEGRAL           : constant := 16#0080#;
--     TVIS_SELECTED           : constant := 16#0002#;
--     TVIS_CUT                : constant := 16#0004#;
--     TVIS_DROPHILITED        : constant := 16#0008#;
--     TVIS_BOLD               : constant := 16#0010#;
--     TVIS_EXPANDED           : constant := 16#0020#;
--     TVIS_EXPANDEDONCE       : constant := 16#0040#;
--     TVIS_EXPANDPARTIAL      : constant := 16#0080#;
--     TVIS_OVERLAYMASK        : constant := 16#0F00#;
--     TVIS_STATEIMAGEMASK     : constant := 16#F000#;
--     TVIS_USERMASK           : constant := 16#F000#;

   TCM_FIRST               : constant := 16#1300#;
   TCM_GETITEMCOUNT        : constant := (TCM_FIRST + 4);
   TCM_GETITEMA            : constant := (TCM_FIRST + 5);
   TCM_SETITEMA            : constant := (TCM_FIRST + 6);
   TCM_INSERTITEMA         : constant := (TCM_FIRST + 7);
   TCM_GETITEMW            : constant := (TCM_FIRST + 60);
   TCM_SETITEMW            : constant := (TCM_FIRST + 61);
   TCM_INSERTITEMW         : constant := (TCM_FIRST + 62);
   TCM_DELETEITEM          : constant := (TCM_FIRST + 8);
   TCM_DELETEALLITEMS      : constant := (TCM_FIRST + 9);
   TCM_GETCURSEL           : constant := (TCM_FIRST + 11);
   TCM_SETCURSEL           : constant := (TCM_FIRST + 12);
   --  TCM_SETCURFOCUS         : constant := (TCM_FIRST + 48);
   TCM_ADJUSTRECT          : constant := (TCM_FIRST + 40);
   TCM_GETROWCOUNT         : constant := (TCM_FIRST + 44);

   TCIF_TEXT               : constant := 16#0001#;
--     TCIF_IMAGE              : constant := 16#0002#;
--     TCIF_RTLREADING         : constant := 16#0004#;
   TCIF_PARAM              : constant := 16#0008#;
--    TCIF_STATE              : constant := 16#0010#;

   type TCITEM is
      record
         Mask           : Interfaces.C.unsigned := 0;
         State          : Interfaces.C.unsigned := 0;
         State_Mask     : Interfaces.C.unsigned := 0;
         Text           : LPTSTR := null;
         TextMax        : Integer := 0;
         Image          : Integer := 0;
         LPARAM         : GWindows.Base.Pointer_To_Base_Window_Class := null;
      end record;

   --  WM_USER = 16#400#
   TB_ADDBUTTONS       : constant := (WM_USER + 20);
   TB_LOADIMAGES       : constant := (WM_USER + 50);
   TB_SETIMAGELIST     : constant := (WM_USER + 48);
   TB_ADDBITMAP        : constant := (WM_USER + 19);
   TB_BUTTONSTRUCTSIZE : constant := (WM_USER + 30);
   TB_SETSTATE         : constant := (WM_USER + 17);
   TB_GETSTATE         : constant := (WM_USER + 18);
   TB_SETSTYLE         : constant := 1080;
   TB_GETSTYLE         : constant := 1081;
   TB_SETEXTENDEDSTYLE : constant := (WM_USER + 84);

   --  * AnSp: Styles now defined in ads
   --  * AnSp   TBSTATE_ENABLED     : constant := 16#4#;
   --  * AnSp   TBSTATE_HIDDEN        : constant := 16#8#;
   --  * AnSp   TBSTATE_INDETERMINATE : constant := 16#10#;
   --  * AnSp   TBSTYLE_SEP         : constant := 1;
   --  * AnSp: Next 6 constants needed by the new functions
   TB_GETBUTTONINFOW   : constant := (WM_USER + 63);
   TB_SETBUTTONINFOW   : constant := (WM_USER + 64);
   TB_GETBUTTONINFOA   : constant := (WM_USER + 65);
   TB_SETBUTTONINFOA   : constant := (WM_USER + 66);
   TB_GETBUTTONSIZE    : constant := WM_USER + 58;

   TBIF_STATE          : constant := 16#00000004#;
   TBIF_STYLE          : constant := 16#00000008#;
   --  * AnSp: up to here

   type TBBUTTON is
      record
         Image          : Integer := 0;
         Command        : Integer := 0;
         State          : Interfaces.C.unsigned_char := 0;
         Style          : Interfaces.C.unsigned_char := 0;
         Reserved1      : Interfaces.C.unsigned_char := 0;
         Reserved2      : Interfaces.C.unsigned_char := 0;
         Data           : Integer := 0;
         IString        : Integer := -1;
      end record;

--     TTM_ACTIVATE            : constant := (WM_USER + 1);
   TTM_SETDELAYTIME        : constant := (WM_USER + 3);
   TTM_ADDTOOLA            : constant := (WM_USER + 4);
   TTM_ADDTOOLW            : constant := (WM_USER + 50);
   TTM_DELTOOLA            : constant := (WM_USER + 5);
   TTM_DELTOOLW            : constant := (WM_USER + 51);
--     TTM_NEWTOOLRECTA        : constant := (WM_USER + 6);
--     TTM_NEWTOOLRECTW        : constant := (WM_USER + 52);
--     TTM_RELAYEVENT          : constant := (WM_USER + 7);
--     TTM_GETTOOLINFOA        : constant := (WM_USER + 8);
--     TTM_GETTOOLINFOW        : constant := (WM_USER + 53);
--     TTM_SETTOOLINFOA        : constant := (WM_USER + 9);
--     TTM_SETTOOLINFOW        : constant := (WM_USER + 54);
--     TTM_HITTESTA            : constant := (WM_USER + 10);
--     TTM_HITTESTW            : constant := (WM_USER + 55);
--     TTM_GETTEXTA            : constant := (WM_USER + 11);
--     TTM_GETTEXTW            : constant := (WM_USER + 56);
--     TTM_UPDATETIPTEXTA      : constant := (WM_USER + 12);
--     TTM_UPDATETIPTEXTW      : constant := (WM_USER + 57);
--     TTM_GETTOOLCOUNT        : constant := (WM_USER + 13);
--     TTM_ENUMTOOLSA          : constant := (WM_USER + 14);
--     TTM_ENUMTOOLSW          : constant := (WM_USER + 58);
--     TTM_GETCURRENTTOOLA     : constant := (WM_USER + 15);
--     TTM_GETCURRENTTOOLW     : constant := (WM_USER + 59);
--     TTM_WINDOWFROMPOINT     : constant := (WM_USER + 16);
--     TTM_TRACKACTIVATE       : constant := (WM_USER + 17);
--     TTM_TRACKPOSITION       : constant := (WM_USER + 18);
--     TTM_SETTIPBKCOLOR       : constant := (WM_USER + 19);
--     TTM_SETTIPTEXTCOLOR     : constant := (WM_USER + 20);
   TTM_GETDELAYTIME        : constant := (WM_USER + 21);
--     TTM_GETTIPBKCOLOR       : constant := (WM_USER + 22);
--     TTM_GETTIPTEXTCOLOR     : constant := (WM_USER + 23);
   TTM_SETMAXTIPWIDTH      : constant := (WM_USER + 24);
--     TTM_GETMAXTIPWIDTH      : constant := (WM_USER + 25);
--     TTM_SETMARGIN           : constant := (WM_USER + 26);
--     TTM_GETMARGIN           : constant := (WM_USER + 27);
--     TTM_POP                 : constant := (WM_USER + 28);
--     TTM_UPDATE              : constant := (WM_USER + 29);

   TTF_IDISHWND            : constant := 16#0001#;
--     TTF_CENTERTIP           : constant := 16#0002#;
--     TTF_RTLREADING          : constant := 16#0004#;
   TTF_SUBCLASS            : constant := 16#0010#;
--     TTF_TRACK               : constant := 16#0020#;
--     TTF_ABSOLUTE            : constant := 16#0080#;
--     TTF_TRANSPARENT         : constant := 16#0100#;
--     TTF_DI_SETITEM          : constant := 16#8000#;

   type TOOLINFO is
      record
         Size       : Integer := TOOLINFO'Size / 8;
         Flags      : Interfaces.C.unsigned := 0;
         HWND       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         UID        : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         Rect       : GWindows.Types.Rectangle_Type := (0, 0, 0, 0);
         hInst      : GWindows.Types.Handle :=
            GWindows.Internal.Current_hInstance;
         Text       : LPTSTR := null;
         lParam     : GWindows.Types.Lparam := 0;
      end record;

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Control : in out Common_Control_Type) is
   begin
      Fire_On_Click (Control);
   end On_Click;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (Control : in out Common_Control_Type) is
   begin
      Fire_On_Double_Click (Control);
   end On_Double_Click;

   --------------------
   -- On_Right_Click --
   --------------------

   procedure On_Right_Click (Control : in out Common_Control_Type) is
   begin
      Fire_On_Right_Click (Control);
   end On_Right_Click;

   ---------------------------
   -- On_Right_Double_Click --
   ---------------------------

   procedure On_Right_Double_Click (Control : in out Common_Control_Type) is
   begin
      Fire_On_Right_Double_Click (Control);
   end On_Right_Double_Click;

   ----------------------
   -- On_Out_Of_Memory --
   ----------------------

   procedure On_Out_Of_Memory (Control : in out Common_Control_Type) is
   begin
      Fire_On_Out_Of_Memory (Control);
   end On_Out_Of_Memory;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Control : in out Common_Control_Type) is
   begin
      Fire_On_Focus (Control);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Control : in out Common_Control_Type) is
   begin
      Fire_On_Lost_Focus (Control);
   end On_Lost_Focus;

   ---------------
   -- On_Return --
   ---------------

   procedure On_Return (Control : in out Common_Control_Type) is
   begin
      Fire_On_Return (Control);
   end On_Return;

   --------------
   -- On_Hover --
   --------------

   procedure On_Hover (Control : in out Common_Control_Type) is
   begin
      Fire_On_Hover (Control);
   end On_Hover;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Common_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      NM_OUTOFMEMORY               : constant := -1;
      NM_CLICK                     : constant := -2;
      NM_DBLCLK                    : constant := -3;
      NM_RETURN                    : constant := -4;
      NM_RCLICK                    : constant := -5;
      NM_RDBLCLK                   : constant := -6;
      NM_SETFOCUS                  : constant := -7;
      NM_KILLFOCUS                 : constant := -8;
      NM_HOVER                     : constant := -13;
   begin
      case Message.Code is
         when NM_OUTOFMEMORY =>
            On_Out_Of_Memory (Common_Control_Type'Class (Window));
         when NM_CLICK =>
            On_Click (Common_Control_Type'Class (Window));
         when NM_DBLCLK =>
            On_Double_Click (Common_Control_Type'Class (Window));
         when NM_RETURN =>
            On_Return (Common_Control_Type'Class (Window));
         when NM_RCLICK =>
            On_Right_Click (Common_Control_Type'Class (Window));
         when NM_RDBLCLK =>
            On_Right_Double_Click (Common_Control_Type'Class (Window));
         when NM_SETFOCUS =>
            On_Focus (Common_Control_Type'Class (Window));
         when NM_KILLFOCUS =>
            On_Lost_Focus (Common_Control_Type'Class (Window));
         when NM_HOVER =>
            On_Hover (Common_Control_Type'Class (Window));
         when others =>
            null;
      end case;

   end On_Notify;

   ------------
   -- Create --
   ------------

   procedure Create
     (Bar        : in out Status_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Size_Grip  : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)

   is
      SBS_SIZEGRIP : constant := 16#100#;
      SBT_TOOLTIPS : constant := 16#800#;
      Styles       : Interfaces.C.unsigned  := SBT_TOOLTIPS;
   begin
      if Size_Grip then
         Styles := Styles or SBS_SIZEGRIP;
      end if;
      Create_Control (Bar, Parent,
                      "msctls_statusbar32",
                      Text,
                      0, 0, 0, 0, 0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Bar);
      end if;

   end Create;

   -----------
   -- Parts --
   -----------

   procedure Parts (Bar       : in out Status_Bar_Type;
                    Positions : in     Status_Bar_Position_Type)
   is
      SB_SETPARTS : constant := 1028;

      procedure SendMessage
         (hwnd   : GWindows.Types.Handle := Handle (Bar);
          uMsg   : Interfaces.C.int      := SB_SETPARTS;
          wParam : GWindows.Types.Wparam :=
             GWindows.Types.Wparam (Positions'Length);
          lParam : System.Address        := Positions'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Parts;

   ----------
   -- Text --
   ----------

   procedure Text (Bar  : in out Status_Bar_Type;
                   Text : in     GString;
                   Part : in     Natural;
                   How  : in     Status_Kind_Type := Sunken)
   is
      SB_SETTEXTA   : constant := 16#401#;
      SB_SETTEXTW   : constant := 16#40B#;
      SBT_NOBORDERS : constant := 16#100#;
      SBT_POPOUT    : constant := 16#200#;

      Format : constant array (Status_Kind_Type) of Integer :=
        (Flat   => SBT_NOBORDERS,
         Sunken => 0,
         Raised => SBT_POPOUT);

      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure SendMessageA
         (hwnd   : GWindows.Types.Handle := Handle (Bar);
          uMsg   : Interfaces.C.int      := SB_SETTEXTA;
          wParam : GWindows.Types.Wparam :=
             GWindows.Types.Wparam (Format (How) + Part);
          lParam : System.Address := C_Text'Address);
      pragma Import (StdCall, SendMessageA, "SendMessageA");

      procedure SendMessageW
         (hwnd   : GWindows.Types.Handle := Handle (Bar);
          uMsg   : Interfaces.C.int      := SB_SETTEXTW;
          wParam : GWindows.Types.Wparam :=
             GWindows.Types.Wparam (Format (How) + Part);
          lParam : System.Address := C_Text'Address);
      pragma Import (StdCall, SendMessageW, "SendMessageW");

   begin
      pragma Warnings (Off);
      if Character_Mode_Identifier = "A" then
         SendMessageA;
      else
         SendMessageW;
      end if;
      pragma Warnings (On);
   end Text;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color (Bar   : in out Status_Bar_Type;
                               Color : GWindows.Colors.Color_Type) is
      SB_SETBKCOLOR : constant := 16#2001#;
      procedure SendMessageA
         (hwnd   : GWindows.Types.Handle := Handle (Bar);
          uMsg   : Interfaces.C.int      := SB_SETBKCOLOR;
          wParam : GWindows.Types.Wparam := 0;
          lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Color));
      pragma Import (StdCall, SendMessageA, "SendMessageA");
   begin
      SendMessageA;
   end Background_Color;

   ----------------------
   -- On_Click_Handler --
   ----------------------

   procedure On_Click_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Click_Event := Handler;
   end On_Click_Handler;

   -------------------
   -- Fire_On_Click --
   -------------------

   procedure Fire_On_Click (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Click_Event /= null then
         Control.On_Click_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Click;

   -----------------------------
   -- On_Double_Click_Handler --
   -----------------------------

   procedure On_Double_Click_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Double_Click_Event := Handler;
   end On_Double_Click_Handler;

   --------------------------
   -- Fire_On_Double_Click --
   --------------------------

   procedure Fire_On_Double_Click (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Double_Click_Event /= null then
         Control.On_Double_Click_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Double_Click;

   ----------------------------
   -- On_Right_Click_Handler --
   ----------------------------

   procedure On_Right_Click_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Right_Click_Event := Handler;
   end On_Right_Click_Handler;

   -------------------------
   -- Fire_On_Right_Click --
   -------------------------

   procedure Fire_On_Right_Click (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Right_Click_Event /= null then
         Control.On_Right_Click_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Right_Click;

   -----------------------------------
   -- On_Right_Double_Click_Handler --
   -----------------------------------

   procedure On_Right_Double_Click_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Right_Double_Click_Event := Handler;
   end On_Right_Double_Click_Handler;

   --------------------------------
   -- Fire_On_Right_Double_Click --
   --------------------------------

   procedure Fire_On_Right_Double_Click (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Right_Double_Click_Event /= null then
         Control.On_Right_Double_Click_Event
           (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Right_Double_Click;

   -----------------------
   -- On_Return_Handler --
   -----------------------

   procedure On_Return_Handler (Control : in out Common_Control_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Return_Event := Handler;
   end On_Return_Handler;

   --------------------
   -- Fire_On_Return --
   --------------------

   procedure Fire_On_Return (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Return_Event /= null then
         Control.On_Return_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Return;

   ------------------------------
   -- On_Out_Of_Memory_Handler --
   ------------------------------

   procedure On_Out_Of_Memory_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Out_Of_Memory_Event := Handler;
   end On_Out_Of_Memory_Handler;

   ---------------------------
   -- Fire_On_Out_Of_Memory --
   ---------------------------

   procedure Fire_On_Out_Of_Memory (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Out_Of_Memory_Event /= null then
         Control.On_Out_Of_Memory_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Out_Of_Memory;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Focus_Event /= null then
         Control.On_Focus_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Lost_Focus_Event /= null then
         Control.On_Lost_Focus_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Lost_Focus;

   ----------------------
   -- On_Hover_Handler --
   ----------------------

   procedure On_Hover_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Hover_Event := Handler;
   end On_Hover_Handler;

   -------------------
   -- Fire_On_Hover --
   -------------------

   procedure Fire_On_Hover (Control : in out Common_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Hover_Event /= null then
         Control.On_Hover_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Hover;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Animation_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer                              := 1;
      Height     : in     Integer                              := 1;
      Auto_Size  : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles     : Interfaces.C.unsigned := 0;
   begin
      if not Auto_Size then
         Styles := ACS_CENTER;
      end if;

      Create_Control (Control, Parent,
                      "SysAnimate32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;

   end Create;

   ----------
   -- Open --
   ----------

   procedure Open (Control : in out Animation_Control_Type;
                   Name    : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Name);

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_OPENA;
         wParam : GWindows.Types.Lparam := 0;
         lParam : access GChar_C        := C_Text (C_Text'First)'Access);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_OPENW;
         wParam : GWindows.Types.Lparam := 0;
         lParam : GString_C             := C_Text);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Control : in out Animation_Control_Type)
   is
      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_OPENA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_OPENW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Close;

   ----------
   -- Play --
   ----------

   procedure Play (Control     : in out Animation_Control_Type;
                   Repeat      : in     Integer                := 1;
                   Start_Frame : in     Natural                := 0;
                   End_Frame   : in     Integer                := -1)
   is
      use GWindows.Types;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_PLAY;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Repeat);
         lParam : GWindows.Types.Lparam :=
           GWindows.Types.Lparam (Interfaces.C.short (Start_Frame)) +
           16#10000# * GWindows.Types.Lparam (Interfaces.C.short (End_Frame)));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Play;

   ----------
   -- Seek --
   ----------

   procedure Seek (Control : in out Animation_Control_Type;
                   Frame   : in     Natural)
   is
   begin
      Play (Control, 1, Frame, Frame);
   end Seek;

   procedure Stop (Control : in out Animation_Control_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := ACM_STOP;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Stop;

   ----------------------
   -- On_Start_Handler --
   ----------------------

   procedure On_Start_Handler (Control : in out Animation_Control_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Start_Event := Handler;
   end On_Start_Handler;

   -------------------
   -- Fire_On_Start --
   -------------------

   procedure Fire_On_Start (Control : in out Animation_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Start_Event /= null then
         Control.On_Start_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Start;

   ----------------------
   -- On_Stop_Handler --
   ----------------------

   procedure On_Stop_Handler (Control : in out Animation_Control_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Stop_Event := Handler;
   end On_Stop_Handler;

   -------------------
   -- Fire_On_Stop --
   -------------------

   procedure Fire_On_Stop (Control : in out Animation_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Stop_Event /= null then
         Control.On_Stop_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Stop;

   --------------
   -- On_Start --
   --------------

   procedure On_Start (Control : in out Animation_Control_Type)
   is
   begin
      Fire_On_Start (Control);
   end On_Start;

   -------------
   -- On_Stop --
   -------------

   procedure On_Stop (Control : in out Animation_Control_Type)
   is
   begin
      Fire_On_Stop (Control);
   end On_Stop;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out Animation_Control_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when ACN_START =>
            On_Start (Animation_Control_Type'Class (Window));
         when ACN_STOP =>
            On_Stop (Animation_Control_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Date_Time_Picker_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Format     : in     Date_Format                          := Long_Format;
      Method     : in     Date_Selection_Method                := Calendar;
      None_OK    : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      use GWindows.GStrings.Unbounded;

      Styles : Interfaces.C.unsigned := 0;

      type Format_Array is array (Date_Format) of Interfaces.C.unsigned;

      Format_Val : constant Format_Array := (Long_Format  => 4,
                                             Short_Format => 0,
                                             Time_Format  => 9);
   begin
      if Method = Up_Down then
         Styles := Styles or 1;
      end if;

      if None_OK then
         Styles := Styles or 2;
      end if;

      case Format is
         when Long_Format =>
            Control.Format :=
               GWindows.GStrings.To_GString_Unbounded ("dddd, MMMM d, yyyy");
         when Short_Format =>
            Control.Format :=
               GWindows.GStrings.To_GString_Unbounded ("M/d/yy");
         when Time_Format =>
            Control.Format :=
               GWindows.GStrings.To_GString_Unbounded ("h:m:s tt");
      end case;

      Create_Control
        (Window     => Control,
         Parent     => Parent,
         Win_Class  => "SysDateTimePick32",
         Text       => "",
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         ID         => 0,
         Styles     => Styles or Format_Val (Format),
         Is_Dynamic => Is_Dynamic);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Control);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Control, Width => New_Size.Width, Height => New_Size.Height);
         end;
      end if;

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   generic
      with function Replace (Item : in GWindows.GString)
                            return GWindows.GString;
      --  Item is one of the format specifiers; replace with an
      --  appropriate string.
   function Gen_Process_Format (Item : in GWindows.GString)
                               return GWindows.GString;
   function Gen_Process_Format (Item : in GWindows.GString)
                               return GWindows.GString
   is
      use GWindows.GStrings.Unbounded;
      Result : GString_Unbounded;
      Next   : Integer := Item'First;
      Last   : Integer;
   begin
      loop
         exit when Next > Item'Last;

         case Item (Next) is
            when 'd' =>
               if Next + 3 <= Item'Last and then
                 Item (Next .. Next + 3) = "dddd"
               then
                  Last := Next + 3;
               elsif Next + 2 <= Item'Last and then
                 Item (Next .. Next + 2) = "ddd"
               then
                  Last := Next + 2;
               elsif Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "dd"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 'h' =>
               if Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "hh"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 'H' =>
               if Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "HH"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 'm' =>
               if Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "mm"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 'M' =>
               if Next + 3 <= Item'Last and then
                 Item (Next .. Next + 3) = "MMMM"
               then
                  Last := Next + 3;
               elsif Next + 2 <= Item'Last and then
                 Item (Next .. Next + 2) = "MMM"
               then
                  Last := Next + 2;
               elsif Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "MM"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 's' =>
               if Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "ss"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 't' =>
               if Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "tt"
               then
                  Last := Next + 1;
               else
                  Last := Next;
               end if;

            when 'y' =>
               if Next + 3 <= Item'Last and then
                 Item (Next .. Next + 3) = "yyyy"
               then
                  Last := Next + 3;
               elsif Next + 1 <= Item'Last and then
                 Item (Next .. Next + 1) = "yy"
               then
                  Last := Next + 1;
               else
                  Last := Next - 1;
               end if;

            when others =>
               Last := Next - 1;
         end case;

         if Last = Next - 1 then
            Result := Result & Item (Next);
            Next   := Next + 1;
         else
            Result := Result & Replace (Item (Next .. Last));
            Next   := Last + 1;
         end if;
      end loop;

      return GWindows.GStrings.To_GString_From_Unbounded (Result);
   end Gen_Process_Format;

   --------------------------------
   --  Largest_Formatted_String  --
   --------------------------------

   function Largest_Formatted_String (Item : in GWindows.GString)
                                     return GWindows.GString
   is
      function Largest (Item : in GWindows.GString) return GWindows.GString;

      function Largest (Item : in GWindows.GString) return GWindows.GString
      --  Return string that is the largest in displayed points,
      --  assuming a reasonable proportional font.
      is begin
         --  FIXME: this assumes English
         if Item = "d" or Item = "dd" then
            return "30";
         elsif Item = "ddd" then
            return "Wed";
         elsif Item = "dddd" then
            return "Wednesday";
         elsif Item = "h" or Item = "hh" then
            return "12";
         elsif Item = "H" or Item = "HH" then
            return "23";
         elsif Item = "m" or Item = "mm" then
            return "59";
         elsif Item = "M" or Item = "MM" then
            return "12";
         elsif Item = "MMM" then
            return "Mar";
         elsif Item = "MMMM" then
            return "December";
         elsif Item = "s" or Item = "ss" then
            return "59";
         elsif Item = "t" then
            return "A";
         elsif Item = "tt" then
            return "AM";
         elsif Item = "yy" then
            return "00";
         elsif Item = "yyyy" then
            return "2000";
         else
            return Item;
         end if;
      end Largest;

      function Process is new Gen_Process_Format (Largest);
   begin
      return Process (Item);
   end Largest_Formatted_String;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Control : in Date_Time_Picker_Type)
                             return GWindows.Types.Size_Type
   is
      use GWindows.Types;

      Canvas : GWindows.Drawing.Canvas_Type;
      Font   : GWindows.Drawing_Objects.Font_Type;
      Extra  : Size_Type := (12, 6); --  white space around text, borders.
      Text_Size : Size_Type;
   begin
      Get_Canvas (Control, Canvas);
      Get_Font (Control, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);

      Text_Size := GWindows.Drawing.Text_Output_Size
        (Canvas,
         Largest_Formatted_String
           (GWindows.GStrings.To_GString_From_Unbounded (Control.Format)));

      Extra := Extra + (Text_Size.Height, 0); --  Drop arrow or up/down arrows

      return Calculate_New_Window_Size (Control, Text_Size + Extra);
   end Recommended_Size;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Date_Time_Picker_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      DTN_FIRST          : constant := -760;
      DTN_DATETIMECHANGE : constant := DTN_FIRST + 1;
   begin
      case Message.Code is
         when DTN_DATETIMECHANGE =>
            On_Date_Time_Change (Date_Time_Picker_Type'Class (Window));
         when others =>
            On_Notify (Common_Control_Type (Window),
                       Message, Control, Return_Value);
      end case;

   end On_Notify;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Date_Time_Picker_Type)
   is
   begin
      Tab_Stop (Control);
   end On_Create;

   -------------------------
   -- On_Date_Time_Change --
   -------------------------

   procedure On_Date_Time_Change (Control : in out Date_Time_Picker_Type)
   is
   begin
      Fire_On_Date_Time_Change (Control);
   end On_Date_Time_Change;

   ---------------------------------
   -- On_Date_Time_Change_Handler --
   ---------------------------------

   procedure On_Date_Time_Change_Handler
     (Control : in out Date_Time_Picker_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Date_Time_Change_Event := Handler;
   end On_Date_Time_Change_Handler;

   ------------------------------
   -- Fire_On_Date_Time_Change --
   ------------------------------

   procedure Fire_On_Date_Time_Change
     (Control : in out Date_Time_Picker_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Date_Time_Change_Event /= null then
         Control.On_Date_Time_Change_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Date_Time_Change;

   ----------------------
   -- Date_Time_Format --
   ----------------------

   procedure Date_Time_Format (Control : in out Date_Time_Picker_Type;
                               Format  : in     GString)
   is

      C_Text : Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (Format));

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_SETFORMAT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access Interfaces.C.char := C_Text (C_Text'First)'Access);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Control.Format :=
        GWindows.GStrings.To_GString_Unbounded (Format);
      SendMessage;
   end Date_Time_Format;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range (Control     : in out Date_Time_Picker_Type;
                        Range_Start : in     Ada.Calendar.Time;
                        Range_End   : in     Ada.Calendar.Time)
   is
      type Range_Type is
         record
            Start_Range : SYSTEMTIME;
            End_Range   : SYSTEMTIME;
         end record;

      The_Range : Range_Type := (Calendar_To_SYSTEMTIME (Range_Start),
                                 Calendar_To_SYSTEMTIME (Range_End));

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_SETRANGE;
         wParam : GWindows.Types.Wparam :=
            GWindows.Types.Wparam (GDTR_MIN + GDTR_MAX);
         lParam : in out Range_Type);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage (lParam => The_Range);
   end Set_Range;

   ---------------
   -- Date_Time --
   ---------------

   procedure Date_Time (Control   : in out Date_Time_Picker_Type;
                        Date_Time : in     Ada.Calendar.Time)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_SETSYSTEMTIME;
         wParam : GWindows.Types.Wparam := GDT_VALID;
         lParam : in out SYSTEMTIME);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      C_Time : SYSTEMTIME := Calendar_To_SYSTEMTIME (Date_Time);
   begin
      SendMessage (lParam => C_Time);
   end Date_Time;

   function Date_Time (Control : in Date_Time_Picker_Type)
                      return Ada.Calendar.Time
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_GETSYSTEMTIME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : out SYSTEMTIME);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      C_Time : SYSTEMTIME;
   begin
      SendMessage (lParam => C_Time);

      return SYSTEMTIME_To_Calendar (C_Time);
   end Date_Time;

   ----------------------------
   -- Calendar_To_SYSTEMTIME --
   ----------------------------

   function Calendar_To_SYSTEMTIME (Time : Ada.Calendar.Time)
                                   return SYSTEMTIME
   is
      use Ada.Calendar;

      C_Time      : SYSTEMTIME;
      Seconds     : Ada.Calendar.Day_Duration;
      Temp        : Ada.Calendar.Day_Duration;
   begin
      C_Time.wYear   := Interfaces.C.short (Year (Time));
      C_Time.wMonth  := Interfaces.C.short (Month (Time));
      C_Time.wDay    := Interfaces.C.short (Day (Time));

      Seconds := Ada.Calendar.Seconds (Time);

      Temp := Seconds / (60*60);
      C_Time.wHour   :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      Temp := (Seconds - (Ada.Calendar.Day_Duration
                          (C_Time.wHour) * (60*60))) / 60;
      C_Time.wMinute :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      Temp := Seconds -
        ((Ada.Calendar.Day_Duration (C_Time.wHour) * (60 * 60)) +
         (Ada.Calendar.Day_Duration (C_Time.wMinute) * 60));
      C_Time.wSecond :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      return C_Time;
   end Calendar_To_SYSTEMTIME;

   ----------------------------
   -- SYSTEMTIME_To_Calendar --
   ----------------------------

   function SYSTEMTIME_To_Calendar (Time : SYSTEMTIME)
                                   return Ada.Calendar.Time
   is
      use Ada.Calendar;
   begin
      return Time_Of (Year_Number (Time.wYear),
                      Month_Number (Time.wMonth),
                      Day_Number (Time.wDay),
                      Day_Duration
                      ((Day_Duration (Time.wHour) * 60 * 60) +
                       (Day_Duration (Time.wMinute) * 60) +
                       (Day_Duration (Time.wSecond))));
   end SYSTEMTIME_To_Calendar;

   --------------------
   -- None_Date_Time --
   --------------------

   procedure None_Date_Time (Control : in out Date_Time_Picker_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_SETSYSTEMTIME;
         wParam : GWindows.Types.Wparam := GDT_NONE;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end None_Date_Time;

   function None_Date_Time (Control : in Date_Time_Picker_Type)
                           return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := DTM_GETSYSTEMTIME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in SYSTEMTIME)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      C_Time : constant SYSTEMTIME := (0, 0, 0, 0, 0, 0, 0, 0);
   begin
      return (SendMessage (lParam => C_Time) = GDT_NONE);
   end None_Date_Time;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out IP_Address_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_Control (Control, Parent,
                      "SysIPAddress32",
                      "",
                      Left, Top, Width, Height,
                      0, 0,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;

      Tab_Stop (Control);
   end Create;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (Control : in out IP_Address_Control_Type) is
   begin
      Fire_On_Change (Control);
   end On_Change;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out IP_Address_Control_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);

      EN_SETFOCUS                : constant := 256;
      EN_KILLFOCUS               : constant := 512;
      EN_CHANGE                  : constant := 768;
   begin
      case Code is
         when EN_SETFOCUS =>
            On_Focus (IP_Address_Control_Type'Class (Window));
         when EN_KILLFOCUS =>
            On_Lost_Focus (IP_Address_Control_Type'Class (Window));
         when EN_CHANGE =>
            On_Change (IP_Address_Control_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   -----------------------
   -- On_Change_Handler --
   -----------------------

   procedure On_Change_Handler (Control : in out IP_Address_Control_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Change_Event := Handler;
   end On_Change_Handler;

   --------------------
   -- Fire_On_Change --
   --------------------

   procedure Fire_On_Change (Control : in out IP_Address_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Change_Event /= null then
         Control.On_Change_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Change;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out IP_Address_Control_Type)
   is
   begin
      Tab_Stop (Control);
   end On_Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Progress_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Direction  : in     Progress_Control_Direction_Type      := Vertical;
      Smooth     : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      PBS_SMOOTH       : constant := 16#01#;
      PBS_VERTICAL     : constant := 16#04#;
      --  PBS_MARQUEE      : constant := 16#08#;
      --  PBS_SMOOTHREVERSE: constant := 16#10#;
      --
      Styles     : Interfaces.C.unsigned := 0;
   begin
      if Direction = Vertical then
         Styles := Styles or PBS_VERTICAL;
      end if;
      if Smooth then
         Styles := Styles or PBS_SMOOTH;
      end if;
      Create_Control (Control, Parent,
                      "msctls_progress32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   --------------
   -- Position --
   --------------

   procedure Position (Control : in out Progress_Control_Type;
                       Where   : in     Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_SETPOS;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Position;

   function Position (Control : in Progress_Control_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_GETPOS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Position;

   --------------------
   -- Progress_Range --
   --------------------

   procedure Progress_Range (Control : in out Progress_Control_Type;
                             Low     : in     Natural;
                             High    : in     Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_SETRANGE32;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Low);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (High));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Progress_Range;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Control : in out Progress_Control_Type;
                        Amount  : in     Natural               := 1)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_DELTAPOS;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Amount);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Increment;

   ---------------
   -- Step_Size --
   ---------------

   procedure Step_Size (Control : in out Progress_Control_Type;
                        Size    : in     Natural               := 10)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_SETSTEP;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Size);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Step_Size;

   ----------
   -- Step --
   ----------

   procedure Step (Control : in out Progress_Control_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := PBM_STEPIT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Step;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out List_View_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Selection  : in     List_View_Control_Select_Type        := Single;
      View       : in     List_View_Control_View_Type          := List_View;
      Sort       : in     List_View_Control_Sort_Type          := No_Sorting;
      Arrange    : in     Boolean                              := True;
      Align      : in     List_View_Control_Alignment_Type     := Align_Left;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is

      LVS_ICON                : constant := 16#0000#;
      LVS_REPORT              : constant := 16#0001#;
      LVS_SMALLICON           : constant := 16#0002#;
      LVS_LIST                : constant := 16#0003#;
      --  LVS_TYPEMASK            : constant := 16#0003#;
      LVS_SINGLESEL           : constant := 16#0004#;
      LVS_SHOWSELALWAYS       : constant := 16#0008#;
      LVS_SORTASCENDING       : constant := 16#0010#;
      LVS_SORTDESCENDING      : constant := 16#0020#;
      --  LVS_SHAREIMAGELISTS     : constant := 16#0040#;
      --  LVS_NOLABELWRAP         : constant := 16#0080#;
      LVS_AUTOARRANGE         : constant := 16#0100#;
      --  LVS_EDITLABELS          : constant := 16#0200#;
      --  LVS_NOSCROLL            : constant := 16#2000#;
      --  LVS_TYPESTYLEMASK       : constant := 16#Fc00#;
      LVS_ALIGNTOP            : constant := 16#0000#;
      LVS_ALIGNLEFT           : constant := 16#0800#;
      --  LVS_ALIGNMASK           : constant := 16#0c00#;
      --  LVS_NOCOLUMNHEADER      : constant := 16#4000#;
      LVS_NOSORTHEADER        : constant := 16#8000#;

      Styles     : Interfaces.C.unsigned := LVS_SHOWSELALWAYS;
   begin
      if Selection = Single then
         Styles := Styles or LVS_SINGLESEL;
      end if;

      case View is
         when Icon_View =>
            Styles := Styles or LVS_ICON;
         when Small_Icon_View =>
            Styles := Styles or LVS_SMALLICON;
         when List_View =>
            Styles := Styles or LVS_LIST;
         when Report_View =>
            Styles := Styles or LVS_REPORT;
      end case;

      if Sort = No_Sorting then
         Styles := Styles or LVS_NOSORTHEADER;
      else
         if Sort = Sort_Ascending then
            Styles := Styles or LVS_SORTASCENDING;
         elsif Sort = Sort_Descending then
            Styles := Styles or LVS_SORTDESCENDING;
         end if;
      end if;

      if Arrange then
         Styles := Styles or LVS_AUTOARRANGE;
      end if;

      if Align = Align_Left then
         Styles := Styles or LVS_ALIGNLEFT;
      elsif Align = Align_Top then
         Styles := Styles or LVS_ALIGNTOP;
      end if;

      Create_Control (Control, Parent,
                      "SysListView32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;

   end Create;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item (Control : in out List_View_Control_Type;
                       Text    : in GString;
                       Index   : in Integer;
                       Icon    : in Integer := 0)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : LVITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM            := Item);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM                := Item);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVIF_TEXT or LVIF_IMAGE;
      Item.Item := Index;
      Item.Image := Icon;
      Item.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Set_Item;

   ------------------
   -- Set_Sub_Item --
   ------------------

   procedure Set_Sub_Item (Control   : in out List_View_Control_Type;
                           Text      : in     GString;
                           Index     : in     Integer;
                           Sub_Index : in     Integer)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : LVITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM                := Item);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM                := Item);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVIF_TEXT;
      Item.Item := Index;
      Item.SubItem := Sub_Index;
      Item.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Set_Sub_Item;

   -----------------
   -- Insert_Item --
   -----------------

   procedure Insert_Item (Control      : in out List_View_Control_Type;
                          Text         : in  GString;
                          Index        : in  Integer;
                          Sorted_Index : out Integer;
                          Icon         : in  Integer := 0)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : LVITEM;

      function SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_INSERTITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM                := Item)
      return GWindows.Types.Lparam;
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      function SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_INSERTITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : LVITEM                := Item)
      return GWindows.Types.Lparam;
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVIF_TEXT or LVIF_IMAGE;
      Item.Item := Index;
      Item.Image := Icon;
      Item.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            Sorted_Index := Integer (SendMessageW);
         when ANSI =>
            Sorted_Index := Integer (SendMessageA);
      end case;
   end Insert_Item;

   procedure Insert_Item (Control : in out List_View_Control_Type;
                          Text    : in GString;
                          Index   : in Integer;
                          Icon    : in Integer := 0)
   is
      Sorted_Index : Integer; -- will be ignored
   begin
      Insert_Item (
         Control      => Control,
         Text         => Text,
         Index        => Index,
         Sorted_Index => Sorted_Index,
         Icon         => Icon
      );
   end Insert_Item;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column (Control : in out List_View_Control_Type;
                         Text    : in     GString;
                         Index   : in     Integer;
                         Width   : in     Integer)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : LVCOLUMN;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETCOLUMNA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN          := Item);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETCOLUMNW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN              := Item);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVCF_TEXT or LVCF_WIDTH;
      Item.Text := C_Text (0)'Unchecked_Access;
      Item.Width := Width;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Set_Column;

   -------------------
   -- Insert_Column --
   -------------------

   procedure Insert_Column (Control : in out List_View_Control_Type;
                            Text    : in     GString;
                            Index   : in     Integer;
                            Width   : in     Integer)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : LVCOLUMN;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_INSERTCOLUMNA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN              := Item);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_INSERTCOLUMNW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN              := Item);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVCF_TEXT or LVCF_WIDTH;
      Item.Text := C_Text (0)'Unchecked_Access;
      Item.Width := Width;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Insert_Column;

   ----------------
   -- Item_Count --
   ----------------

   function Item_Count (Control : in List_View_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETITEMCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Item_Count;

   -------------------------
   -- Selected_Item_Count --
   -------------------------

   function Selected_Item_Count (Control : in List_View_Control_Type)
                                return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETSELECTEDCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Selected_Item_Count;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected (Control : in List_View_Control_Type;
                         Index   : in Integer)
                        return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETITEMSTATE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : GWindows.Types.Lparam := LVIS_SELECTED)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return (SendMessage /= 0);
   end Is_Selected;

   -----------
   -- Clear --
   -----------

   procedure Clear (Control : in out List_View_Control_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_DELETEALLITEMS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear;

   ------------------
   -- Clicked_Item --
   ------------------

   procedure Item_At_Position
     (Control  : in     List_View_Control_Type;
      Position : in     GWindows.Types.Point_Type;
      Item     : in out Integer;
      SubItem  : in out Integer)
   is

      type LVHITTESTINFO is
         record
            Pt      : GWindows.Types.Point_Type;
            Flags   : Natural;
            Item    : Integer;
            SubItem : Integer;
         end record;

      LVM_SUBITEMHITTEST : constant := LVM_FIRST + 57;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Common_Controls.Handle (Control);
         uMsg   : Interfaces.C.int  := LVM_SUBITEMHITTEST;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out LVHITTESTINFO);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      HitTestInfo : LVHITTESTINFO := (Position, 0, 0, 0);
   begin
      SendMessage (lParam => HitTestInfo);

      Item := HitTestInfo.Item;
      SubItem := HitTestInfo.SubItem;
   end Item_At_Position;

   ----------
   -- Text --
   ----------

   function Text
     (Control : in List_View_Control_Type;
      Item    : in Integer;
      SubItem : in Integer)
     return GString
   is
      LVM_GETITEMA : constant := LVM_FIRST + 5;
      LVM_GETITEMW : constant := LVM_FIRST + 75;
      LVIF_TEXT    : constant := 16#0001#;

      Max_Text : constant := 255;
      type Buffer is new GString_C (0 .. Max_Text);
      type PBuffer is access all Buffer;

      function To_PBuffer is
         new Ada.Unchecked_Conversion (LPTSTR, PBuffer);

      C_Text   : Buffer;
      LVI      : LVITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Common_Controls.Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out LVITEM);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Common_Controls.Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out LVITEM);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      LVI.Mask := LVIF_TEXT;
      LVI.Item := Item;
      LVI.SubItem := SubItem;
      LVI.Text := C_Text (0)'Unchecked_Access;
      LVI.TextMax := Max_Text;

      case Character_Mode is
         when Unicode =>
            SendMessageW (lParam => LVI);
         when ANSI =>
            SendMessageA (lParam => LVI);
      end case;

      return GWindows.GStrings.To_GString_From_C
        (GString_C (To_PBuffer (LVI.Text).all));
   end Text;

   --------------
   -- Selected --
   --------------

   procedure Selected
     (Control : in out List_View_Control_Type;
      Item    : in     Integer;
      State   : in     Boolean)
   is

      LVM_SETITEMSTATE : constant := LVM_FIRST + 43;
      LVIS_SELECTED    : constant := 16#0002#;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Common_Controls.Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETITEMSTATE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item);
         lParam : LVITEM);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      LVI : LVITEM;
   begin
      LVI.StateMask := LVIS_SELECTED;
      if State then
         LVI.State := LVIS_SELECTED;
      else
         LVI.State := 0;
      end if;
      SendMessage (lParam => LVI);
   end Selected;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
     (Control : in out List_View_Control_Type;
      Index   : in     Integer;
      Width   : in     Integer)
   is
      type LVCOLUMN is
         record
            Mask      : Interfaces.C.unsigned := 0;
            Format    : Interfaces.C.unsigned := 0;
            Width     : Integer               := 0;
            Text      : LPTSTR                := null;
            TextMax   : Integer               := 0;
            SubItem   : Integer               := 0;
            Image     : Integer               := 0;
            Order     : Integer               := 0;
         end record;

      LVM_FIRST               : constant := 16#1000#;
      LVM_SETCOLUMNA          : constant := LVM_FIRST + 26;
      LVM_SETCOLUMNW          : constant := LVM_FIRST + 96;
      LVCF_WIDTH              : constant := 16#0002#;

      Item : LVCOLUMN;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETCOLUMNA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN              := Item);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETCOLUMNW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : LVCOLUMN              := Item);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := LVCF_WIDTH;
      Item.Text := null;
      Item.Width := Width;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Set_Column_Width;

   ------------------
   -- Column_Width --
   ------------------

   function Column_Width (
      Control : in List_View_Control_Type;
      Index   : in Integer
   )
   return Integer
   is
      LVM_FIRST               : constant := 16#1000#;
      LVM_GETCOLUMNWIDTH      : constant := LVM_FIRST + 29;

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_GETCOLUMNWIDTH;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Column_Width;

   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Control : in out List_View_Control_Type;
      Index   : in     Integer)
   is
      LVM_DELETEITEM       : constant := LVM_FIRST + 8;
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_DELETEITEM;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete_Item;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out List_View_Control_Type)
   is
   begin
      Border (Control);
      Tab_Stop (Control);
   end On_Create;

   ---------------------
   -- On_Item_Changed --
   ---------------------

   procedure On_Item_Changed (Control : in out List_View_Control_Type) is
   begin
      Fire_On_Item_Changed (Control);
   end On_Item_Changed;

   -----------------------------
   -- On_Item_Changed_Handler --
   -----------------------------

   procedure On_Item_Changed_Handler
      (Control : in out List_View_Control_Type;
       Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Item_Changed_Event := Handler;
   end On_Item_Changed_Handler;

   ---------------------
   -- On_Item_Changed --
   ---------------------

   procedure Fire_On_Item_Changed (Control : in out List_View_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Item_Changed_Event /= null then
         Control.On_Item_Changed_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Item_Changed;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out List_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      LVN_FIRST       : constant := -100;
      --  LVN_ITEMCHANGING : constant := LVN_FIRST - 0;
      LVN_ITEMCHANGED : constant := LVN_FIRST - 1;
   begin
      case Message.Code is
         when LVN_ITEMCHANGED =>
            On_Item_Changed (List_View_Control_Type'Class (Window));
         when others =>
            On_Notify (Common_Control_Type (Window),
                       Message, Control, Return_Value);
      end case;
   end On_Notify;

   --------------
   -- AnSp: Next List_View_Control_Type functions are added --
   --------------

   procedure Set_Image_List
     (Control   : in out List_View_Control_Type;
      ImageType : in     List_View_Image_Type;
      List      : in     GWindows.Image_Lists.Image_List_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := LVM_SETIMAGELIST;
         wParam : GWindows.Types.Wparam;
         lParam : GWindows.Types.Handle);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
      ImgType : GWindows.Types.Wparam;
   begin
      case ImageType is
         when Normal => ImgType := LVSIL_NORMAL;
         when Small  => ImgType := LVSIL_SMALL;
         when State  => ImgType := LVSIL_STATE;
      end case;
      SendMessage (wParam => ImgType,
                   lParam => GWindows.Image_Lists.Handle (List));
   end Set_Image_List;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control       : in out Tree_View_Control_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left          : in     Integer;
      Top           : in     Integer;
      Width         : in     Integer;
      Height        : in     Integer;
      Buttons       : in     Boolean                              := True;
      Lines         : in     Boolean                              := True;
      Lines_At_Root : in     Boolean                              := True;
      Single_Expand : in     Boolean                              := False;
      Show          : in     Boolean                              := True;
      Is_Dynamic    : in     Boolean                              := False)
   is

      TVS_HASBUTTONS          : constant := 16#0001#;
      TVS_HASLINES            : constant := 16#0002#;
      TVS_LINESATROOT         : constant := 16#0004#;
      --  TVS_EDITLABELS          : constant := 16#0008#;
      --  TVS_DISABLEDRAGDROP     : constant := 16#0010#;
      TVS_SHOWSELALWAYS       : constant := 16#0020#;
      --  TVS_RTLREADING          : constant := 16#0040#;
      --  TVS_NOTOOLTIPS          : constant := 16#0080#;
      --  TVS_CHECKBOXES          : constant := 16#0100#;
      --  TVS_TRACKSELECT         : constant := 16#0200#;
      TVS_SINGLEEXPAND        : constant := 16#0400#;
      --  TVS_INFOTIP             : constant := 16#0800#;
      --  TVS_FULLROWSELECT       : constant := 16#1000#;
      --  TVS_NOSCROLL            : constant := 16#2000#;
      --  TVS_NONEVENHEIGHT       : constant := 16#4000#;

      Styles     : Interfaces.C.unsigned := TVS_SHOWSELALWAYS;
   begin
      if Lines then
         Styles := Styles or TVS_HASLINES;
      end if;

      if Single_Expand then
         Styles := Styles or TVS_SINGLEEXPAND;
      end if;

      if Buttons then
         Styles := Styles or TVS_HASBUTTONS;
      end if;

      if Lines_At_Root then
         Styles := Styles or TVS_LINESATROOT;
      end if;

      Create_Control (Control, Parent,
                      "SysTreeView32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   -----------------
   -- Insert_Item --
   -----------------

   procedure Insert_Item
     (Control     : in out Tree_View_Control_Type;
      Text        : in     GString;
      Parent_Node : in     Tree_Item_Node;
      New_Node    :    out Tree_Item_Node;
      Where       : in     Tree_Item_Node)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      type TVINSERTSTRUCT is
         record
            HParent : Tree_Item_Node := Parent_Node;
            HAfter  : Tree_Item_Node := Where;
            Item    : TVITEM;
         end record;

      TS : TVINSERTSTRUCT;

      function SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_INSERTITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TVINSERTSTRUCT        := TS)
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      function SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_INSERTITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TVINSERTSTRUCT        := TS)
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TS.Item.Mask := TVIF_TEXT;
      TS.Item.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            New_Node := SendMessageW;
         when ANSI =>
            New_Node := SendMessageA;
      end case;
   end Insert_Item;

   procedure Insert_Item
     (Control     : in out Tree_View_Control_Type;
      Text        : in     GString;
      Parent_Node : in     Tree_Item_Node;
      New_Node    :    out Tree_Item_Node;
      Where       : in     Tree_View_List_Location_Type := Sort)
   is
      TVI_ROOT                : constant := 16#FFFF0000#;
      TVI_FIRST               : constant := 16#FFFF0001#;
      TVI_LAST                : constant := 16#FFFF0002#;
      TVI_SORT                : constant := 16#FFFF0003#;

      type Where_Value is
        array (Tree_View_List_Location_Type) of Tree_Item_Node;

      Values : Where_Value :=
        (First     => TVI_FIRST,
         Last      => TVI_LAST,
         Sort      => TVI_SORT,
         As_A_Root => TVI_ROOT);
      pragma Warnings (Off, Values);
   begin
      Insert_Item (Control, Text, Parent_Node, New_Node, Values (Where));
   end Insert_Item;

   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Control : in out Tree_View_Control_Type;
      Where   : in     Tree_Item_Node)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_DELETEITEM;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Where));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete_Item;

   -------------------
   -- Selected_Item --
   -------------------

   function Selected_Item (Control : in Tree_View_Control_Type)
                          return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_CARET;
         lParam : GWindows.Types.Lparam := 0)
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Selected_Item;

   function Selected_Item (Control : in Tree_View_Control_Type)
                          return GString
   is
   begin
      return Text (Control, Selected_Item (Control));
   end Selected_Item;

   -------------------
   -- Get_Root_Item --
   -------------------

   function Get_Root_Item (Control : in Tree_View_Control_Type)
                          return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_ROOT;
         lParam : GWindows.Types.Lparam := 0)
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Root_Item;

   ---------------------
   -- Get_Parent_Item --
   ---------------------

   function Get_Parent_Item (Control : in Tree_View_Control_Type;
                             From    : in Tree_Item_Node)
                            return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_PARENT;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (From))
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Parent_Item;

   --------------------------
   -- Get_First_Child_Item --
   --------------------------

   function Get_First_Child_Item (Control : in Tree_View_Control_Type;
                                  From    : in Tree_Item_Node)
                                 return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int  := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_CHILD;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (From))
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_First_Child_Item;

   -------------------
   -- Get_Next_Item --
   -------------------

   function Get_Next_Item (Control : in Tree_View_Control_Type;
                           From    : in Tree_Item_Node)
                          return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_NEXT;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (From))
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Next_Item;

   -----------------------
   -- Get_Previous_Item --
   -----------------------

   function Get_Previous_Item (Control : in Tree_View_Control_Type;
                               From    : in Tree_Item_Node)
                              return Tree_Item_Node
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETNEXTITEM;
         wParam : GWindows.Types.Wparam := TVGN_PREVIOUS;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (From))
        return Tree_Item_Node;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Previous_Item;

   ----------
   -- Text --
   ----------

   function Text (Control : in Tree_View_Control_Type;
                  Where   : in Tree_Item_Node)
                 return GString
   is
      Max_Text : constant := 255;
      type Buffer is new GString_C (0 .. Max_Text);
      type PBuffer is access all Buffer;

      function To_PBuffer is
         new Ada.Unchecked_Conversion (LPTSTR, PBuffer);

      C_Text   : Buffer;
      TV       : TVITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out TVITEM);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out TVITEM);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TV.Mask := TVIF_TEXT;
      TV.HItem := Where;
      TV.Text := C_Text (0)'Unchecked_Access;
      TV.TextMax := Max_Text;

      case Character_Mode is
         when Unicode =>
            SendMessageW (lParam => TV);
         when ANSI =>
            SendMessageA (lParam => TV);
      end case;

      return GWindows.GStrings.To_GString_From_C
        (GString_C (To_PBuffer (TV.Text).all));
   end Text;

   --  GdM: procedure Text (with node) added 2-Jun-2009, uses AnSp's Set_Item
   procedure Text (Control : in out Tree_View_Control_Type;
                   Where   : in     Tree_Item_Node;
                   Text    : in     GString)
   is
   begin
      Set_Item (Control, Where, TVIF_TEXT, Text, 0, 0, 0, 0, 0);
   end Text;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Control     : in out Tree_View_Control_Type;
      At_Node     : in     Tree_Item_Node)
   is
      TVE_EXPAND : constant := 2;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_EXPAND;
         wParam : GWindows.Types.Wparam := TVE_EXPAND;
         lParam : Tree_Item_Node        := At_Node);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Expand;

   --------------
   -- Collapse --
   --------------

   procedure Collapse
     (Control     : in out Tree_View_Control_Type;
      At_Node     : in     Tree_Item_Node)
   is
      TVE_COLLAPSE : constant := 1;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_EXPAND;
         wParam : GWindows.Types.Wparam := TVE_COLLAPSE;
         lParam : Tree_Item_Node        := At_Node);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Collapse;

   ------------------
   --  Select_Item --
   ------------------
   --  GdM: added 21-May-2009

   procedure Select_Item
     (Control     : in out Tree_View_Control_Type;
      Node        : in     Tree_Item_Node)
   is

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_SELECTITEM;
         wParam : GWindows.Types.Wparam := TVGN_CARET;
         lParam : Tree_Item_Node        := Node);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Select_Item;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Tree_View_Control_Type)
   is
   begin
      Border (Control);
      Tab_Stop (Control);
   end On_Create;

   --------------
   -- AnSp: Next Tree_View_Control_Type functions are added --
   --------------

   function Get_Count
      (Control : in Tree_View_Control_Type)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_GETCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Count;

   procedure Set_Item
     (Control       : in out Tree_View_Control_Type;
      Where         : in     Tree_Item_Node;
      Mask          : in     Integer;
      Text          : in     GString;
      Image         : in     Integer;
      SelectedImage : in     Integer;
      State         : in     Integer;
      StateMask     : in     Integer;
      Param         : in     Integer)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);
      TV     : TVITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_SETITEMA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TVITEM                := TV);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_SETITEMW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TVITEM                := TV);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TV.Mask := Interfaces.C.unsigned (Mask);
      TV.HItem := Where;
      TV.Text := C_Text (0)'Unchecked_Access;
      TV.Image := Image;
      TV.Selected_Image := SelectedImage;
      TV.State := Interfaces.C.unsigned (State);
      TV.State_Mask := Interfaces.C.unsigned (StateMask);
      TV.LPARAM := GWindows.Types.Lparam (Param);

      pragma Warnings (Off);
      if Character_Mode = Unicode then
         SendMessageW;
      else
         SendMessageA;
      end if;
      pragma Warnings (On);
   end Set_Item;

   procedure Set_Image_List
     (Control : in out Tree_View_Control_Type;
      List    : in     GWindows.Image_Lists.Image_List_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TVM_SETIMAGELIST;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Handle);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage (lParam => GWindows.Image_Lists.Handle (List));
   end Set_Image_List;

   procedure Set_Image
     (Control     : in out Tree_View_Control_Type;
      Where       : in     Tree_Item_Node;
      Image       : in     Integer;
      ImageSelect : in     Integer)
   is
   begin
      Set_Item (Control, Where, TVIF_IMAGE + TVIF_SELECTEDIMAGE, "", Image,
         ImageSelect, 0, 0, 0);
   end Set_Image;

   -------------------------
   -- On_Selection_Change --
   -------------------------

   procedure On_Selection_Change (Control : in out Tree_View_Control_Type) is
   begin
      Fire_On_Selection_Change (Control);
   end On_Selection_Change;

   ---------------------------------
   -- On_Selection_Change_Handler --
   ---------------------------------

   procedure On_Selection_Change_Handler
      (Control : in out Tree_View_Control_Type;
       Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Selection_Change_Event := Handler;
   end On_Selection_Change_Handler;

   ------------------------------
   -- Fire_On_Selection_Change --
   ------------------------------

   procedure Fire_On_Selection_Change (Control : in out Tree_View_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Selection_Change_Event /= null then
         Control.On_Selection_Change_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Selection_Change;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Tree_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      TVN_FIRST       : constant := -400;
      TVN_SELCHANGEDA : constant := TVN_FIRST - 2;
      TVN_SELCHANGEDW : constant := TVN_FIRST - 51;
   begin
      case Message.Code is
         when TVN_SELCHANGEDA | TVN_SELCHANGEDW =>
            On_Selection_Change (Tree_View_Control_Type'Class (Window));
         when others =>
            On_Notify (Common_Control_Type (Window),
                       Message, Control, Return_Value);
      end case;
   end On_Notify;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Trackbar_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Where      : in     Trackbar_Control_Ticks_Type          := No_Ticks;
      Direction  : in     Trackbar_Control_Direction_Type      := Horizontal;
      Thumb      : in     Boolean                              := True;
      Tips       : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is

      TBS_AUTOTICKS           : constant := 16#0001#;
      TBS_VERT                : constant := 16#0002#;
      --  TBS_HORZ                : constant := 16#0000#;
      TBS_TOP                 : constant := 16#0004#;
      --  TBS_BOTTOM              : constant := 16#0000#;
      --  TBS_LEFT                : constant := 16#0004#;
      --  TBS_RIGHT               : constant := 16#0000#;
      TBS_BOTH                : constant := 16#0008#;
      --  TBS_NOTICKS             : constant := 16#0010#;
      --  TBS_ENABLESELRANGE      : constant := 16#0020#;
      --  TBS_FIXEDLENGTH         : constant := 16#0040#;
      TBS_NOTHUMB             : constant := 16#0080#;
      TBS_TOOLTIPS            : constant := 16#0100#;

      Styles     : Interfaces.C.unsigned := 0;
   begin
      if Where /= No_Ticks then
         Styles := Styles or TBS_AUTOTICKS;

         if Where = Top_Ticks then
            Styles := Styles or TBS_TOP;
         elsif Where = Both_Ticks then
            Styles := Styles or TBS_BOTH;
         end if;
      end if;

      if Direction = Vertical then
         Styles := Styles or TBS_VERT;
      end if;

      if not Thumb then
         Styles := Styles or TBS_NOTHUMB;
      end if;

      if Tips then
         Styles := Styles or TBS_TOOLTIPS;
      end if;

      Create_Control (Control, Parent,
                      "msctls_trackbar32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   --------------
   -- Position --
   --------------

   procedure Position
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_SETPOS;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Where));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Position;

   function Position (Control : in Trackbar_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_GETPOS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Position;

   -------------
   -- Minimum --
   -------------

   procedure Minimum
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_SETRANGEMIN;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Where));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Minimum;

   function Minimum (Control : in Trackbar_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_GETRANGEMIN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Minimum;

   -------------
   -- Maximum --
   -------------

   procedure Maximum
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_SETRANGEMAX;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Where));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Maximum;

   function Maximum (Control : in Trackbar_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TBM_GETRANGEMAX;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Maximum;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Trackbar_Control_Type)
   is
   begin
      Tab_Stop (Control);
   end On_Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Up_Down_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Keyboard   : in     Boolean                              := True;
      Direction  : in     Up_Down_Control_Direction_Type       := Vertical;
      Wrap       : in     Boolean                              := False;
      Auto_Buddy : in     Boolean                              := True;
      Send_Int   : in     Boolean                              := True;
      Thousands  : in     Boolean                              := True;
      Align      : in     Up_Down_Control_Align_Type           := Align_Right;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is

      UDS_WRAP                : constant := 16#0001#;
      UDS_SETBUDDYINT         : constant := 16#0002#;
      UDS_ALIGNRIGHT          : constant := 16#0004#;
      UDS_ALIGNLEFT           : constant := 16#0008#;
      UDS_AUTOBUDDY           : constant := 16#0010#;
      UDS_ARROWKEYS           : constant := 16#0020#;
      UDS_HORZ                : constant := 16#0040#;
      UDS_NOTHOUSANDS         : constant := 16#0080#;

      Styles     : Interfaces.C.unsigned := 0;
   begin
      if Wrap then
         Styles := Styles or UDS_WRAP;
      end if;

      if Auto_Buddy then
         Styles := Styles or UDS_AUTOBUDDY;

         if Align = Align_Left then
            Styles := Styles or UDS_ALIGNLEFT;
         else
            Styles := Styles or UDS_ALIGNRIGHT;
         end if;

         if Send_Int then
            Styles := Styles or UDS_SETBUDDYINT;
         end if;

         if not Thousands then
            Styles := Styles or UDS_NOTHOUSANDS;
         end if;
      end if;

      if Direction = Horizontal then
         Styles := Styles or UDS_HORZ;
      end if;

      if Keyboard then
         Styles := Styles or UDS_ARROWKEYS;
      end if;

      Create_Control (Control, Parent,
                      "msctls_updown32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   --------------
   -- Position --
   --------------

   procedure Position
     (Control : in out Up_Down_Control_Type;
      Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := UDM_SETPOS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Where));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Position;

   function Position (Control : in Up_Down_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := UDM_SETPOS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Position;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Control  : in out Up_Down_Control_Type;
      Min, Max : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := UDM_SETRANGE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Utilities.Make_Long
           (Interfaces.C.short (Min), Interfaces.C.short (Max)));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Range;

   ----------------------------------
   -- On_Position_Changing_Handler --
   ----------------------------------

   procedure On_Position_Changing_Handler
     (Control : in out Up_Down_Control_Type;
      Handler : in     Up_Down_Changing_Event)
   is
   begin
      Control.On_Position_Changing_Event := Handler;
   end On_Position_Changing_Handler;

   -------------------------------
   -- Fire_On_Position_Changing --
   -------------------------------

   procedure Fire_On_Position_Changing
     (Control        : in out Up_Down_Control_Type;
      Position       : in     Integer;
      Delta_Position : in     Integer)
   is
      use GWindows.Base;
   begin
      if Control.On_Position_Changing_Event /= null then
         Control.On_Position_Changing_Event (Base_Window_Type'Class (Control),
                                             Position,
                                             Delta_Position);
      end if;
   end Fire_On_Position_Changing;

   --------------------------
   -- On_Position_Changing --
   --------------------------

   procedure On_Position_Changing
     (Window         : in out Up_Down_Control_Type;
      Position       : in     Integer;
      Delta_Position : in     Integer)
   is
   begin
      Fire_On_Position_Changing (Window, Position, Delta_Position);
   end On_Position_Changing;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Up_Down_Control_Type)
   is
   begin
      Tab_Stop (Control);
   end On_Create;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Up_Down_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      UDN_FIRST          : constant := -721;
      UDN_DELTAPOS       : constant := UDN_FIRST - 1;

      type NMUPDOWN is
         record
            Header : GWindows.Base.Notification;
            Pos    : Integer;
            Delt   : Integer;
         end record;

      type PNMUPDOWN is access all NMUPDOWN;

      function To_PNMUPDOWN is new
        Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                                  PNMUPDOWN);
   begin
      case Message.Code is
         when UDN_DELTAPOS =>
            declare
               NM : constant PNMUPDOWN := To_PNMUPDOWN (Message);
            begin
               On_Position_Changing (Up_Down_Control_Type'Class (Window),
                                     NM.Pos,
                                     NM.Delt);
            end;
         when others =>
            On_Notify (Common_Control_Type (Window),
                       Message, Control, Return_Value);
      end case;

   end On_Notify;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Tab_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Fixed_Tabs : in     Boolean                              := False;
      Multi_Line : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is

      --  TCS_SCROLLOPPOSITE      : constant := 16#0001#;
      --  TCS_BOTTOM              : constant := 16#0002#;
      --  TCS_RIGHT               : constant := 16#0002#;
      --  TCS_MULTISELECT         : constant := 16#0004#;
      --  TCS_FLATBUTTONS         : constant := 16#0008#;
      --  TCS_FORCEICONLEFT       : constant := 16#0010#;
      --  TCS_FORCELABELLEFT      : constant := 16#0020#;
      --  TCS_HOTTRACK            : constant := 16#0040#;
      --  TCS_VERTICAL            : constant := 16#0080#;
      --  TCS_TABS                : constant := 16#0000#;
      --  TCS_BUTTONS             : constant := 16#0100#;
      --  TCS_SINGLELINE          : constant := 16#0000#;
      TCS_MULTILINE           : constant := 16#0200#;
      --  TCS_RIGHTJUSTIFY        : constant := 16#0000#;
      TCS_FIXEDWIDTH          : constant := 16#0400#;
      --  TCS_RAGGEDRIGHT         : constant := 16#0800#;
      --  TCS_FOCUSONBUTTONDOWN   : constant := 16#1000#;
      --  TCS_OWNERDRAWFIXED      : constant := 16#2000#;
      TCS_TOOLTIPS            : constant := 16#4000#;
      --  TCS_FOCUSNEVER          : constant := 16#8000#;

      Styles     : Interfaces.C.unsigned := TCS_TOOLTIPS;
   begin
      if Fixed_Tabs then
         Styles := Styles or TCS_FIXEDWIDTH;
      end if;

      if Multi_Line then
         Styles := Styles or TCS_MULTILINE;
      end if;

      Create_Control (Control, Parent,
                      "SysTabControl32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (Control : in out Tab_Control_Type) is
   begin
      Fire_On_Change (Control);
   end On_Change;

   -----------------------
   -- On_Change_Handler --
   -----------------------

   procedure On_Change_Handler (Control : in out Tab_Control_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Change_Event := Handler;
   end On_Change_Handler;

   --------------------
   -- Fire_On_Change --
   --------------------

   procedure Fire_On_Change (Control : in out Tab_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Change_Event /= null then
         Control.On_Change_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Change;

   -----------------
   -- On_Changing --
   -----------------

   procedure On_Changing (Control : in out Tab_Control_Type) is
   begin
      Fire_On_Changing (Control);
   end On_Changing;

   -------------------------
   -- On_Changing_Handler --
   -------------------------

   procedure On_Changing_Handler (Control : in out Tab_Control_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Changing_Event := Handler;
   end On_Changing_Handler;

   ----------------------
   -- Fire_On_Changing --
   ----------------------

   procedure Fire_On_Changing (Control : in out Tab_Control_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Changing_Event /= null then
         Control.On_Changing_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Changing;

   ----------------
   -- Insert_Tab --
   ----------------

   procedure Insert_Tab (Control : in out Tab_Control_Type;
                         Where   : in     Integer;
                         Value   : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Value);

      TC : TCITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_INSERTITEMA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);
      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_INSERTITEMW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TC.Mask := TCIF_TEXT or TCIF_PARAM;
      TC.Text := C_Text (0)'Unchecked_Access;
      TC.LPARAM := null;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Insert_Tab;

   ----------
   -- Text --
   ----------

   procedure Text (Control : in out Tab_Control_Type;
                   Where   : in     Integer;
                   Value   : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Value);

      TC : TCITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_SETITEMA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_SETITEMW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      TC.Mask := TCIF_TEXT;
      TC.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Text;

   function Text (Control : in Tab_Control_Type;
                  Where   : in Integer)
                 return GString
   is
      Max_Text : constant := 255;
      type Buffer is new GString_C (0 .. Max_Text);
      type PBuffer is access all Buffer;

      function To_PBuffer is
         new Ada.Unchecked_Conversion (LPTSTR, PBuffer);

      C_Text   : Buffer;
      TC       : TCITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETITEMA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : in out TCITEM);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETITEMW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : in out TCITEM);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      TC.Mask := TCIF_TEXT;
      TC.Text := C_Text (0)'Unchecked_Access;
      TC.TextMax := Max_Text;

      case Character_Mode is
         when Unicode =>
            SendMessageW (lParam => TC);
         when ANSI =>
            SendMessageA (lParam => TC);
      end case;

      return Interfaces.C.To_Ada
        (GString_C (To_PBuffer (TC.Text).all));
   end Text;

   ---------------
   -- Tab_Count --
   ---------------

   function Tab_Count (Control : in Tab_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int  := TCM_GETITEMCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return Integer (SendMessage);
   end Tab_Count;

   -------------------
   -- Tab_Row_Count --
   -------------------

   function Tab_Row_Count (Control : in Tab_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETROWCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return Integer (SendMessage);
   end Tab_Row_Count;

   ------------------
   -- Selected_Tab --
   ------------------

   procedure Selected_Tab (Control : in out Tab_Control_Type;
                           Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_SETCURSEL;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Selected_Tab;

   function Selected_Tab (Control : in Tab_Control_Type) return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETCURSEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return Integer (SendMessage);
   end Selected_Tab;

   ----------------
   -- Delete_Tab --
   ----------------

   procedure Delete_Tab (Control : in out Tab_Control_Type;
                         Where   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_DELETEITEM;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete_Tab;

   ---------------------
   -- Delete_All_Tabs --
   ---------------------

   procedure Delete_All_Tabs (Control : in out Tab_Control_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_DELETEALLITEMS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete_All_Tabs;

   ------------------
   -- Display_Area --
   ------------------

   function Display_Area (Control : in Tab_Control_Type)
                         return GWindows.Types.Rectangle_Type is
      RT : GWindows.Types.Rectangle_Type := (0, 0,
                                             Width (Control),
                                             Height (Control));

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_ADJUSTRECT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : in out GWindows.Types.Rectangle_Type);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage (lParam => RT);
      return RT;
   end Display_Area;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Tab_Control_Type)
   is
   begin
      Tab_Stop (Control);
   end On_Create;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Tab_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      TCN_FIRST       : constant := -550;
      TCN_SELCHANGE   : constant := TCN_FIRST - 1;
      TCN_SELCHANGING : constant := TCN_FIRST - 2;
   begin
      case Message.Code is
         when TCN_SELCHANGE =>
            On_Change (Tab_Control_Type'Class (Window));
         when TCN_SELCHANGING =>
            On_Changing (Tab_Control_Type'Class (Window));
         when others =>
            On_Notify (Common_Control_Type (Window),
                       Message, Control, Return_Value);
      end case;

   end On_Notify;

   ----------------
   -- Tab_Window --
   ----------------

   procedure Tab_Window
     (Control : in out Tab_Window_Control_Type;
      Where   : in     Integer;
      Window  : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      TC : TCITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_SETITEMA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_SETITEMW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : TCITEM                := TC);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TC.Mask := TCIF_PARAM;
      TC.LPARAM := Window;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;

      if Where = Selected_Tab (Control) then
         On_Change (Tab_Window_Control_Type'Class (Control));
      end if;
   end Tab_Window;

   function Tab_Window (Control : in Tab_Window_Control_Type;
                        Where   : in Integer)
                       return GWindows.Base.Pointer_To_Base_Window_Class
   is
      TC       : TCITEM;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETITEMA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : in out TCITEM);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TCM_GETITEMW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Where);
         lParam : in out TCITEM);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TC.Mask := TCIF_PARAM;

      case Character_Mode is
         when Unicode =>
            SendMessageW (lParam => TC);
         when ANSI =>
            SendMessageA (lParam => TC);
      end case;

      return TC.LPARAM;
   end Tab_Window;

   -----------------
   -- On_Changing --
   -----------------

   procedure On_Changing (Control : in out Tab_Window_Control_Type)
   is
      use GWindows.Base;

      Current : constant Pointer_To_Base_Window_Class :=
        Tab_Window (Control, Selected_Tab (Control));
   begin
      On_Changing (Tab_Control_Type (Control));                      --  *AnSp
      if Current /= null then
         Hide (Current.all);
      end if;
   end On_Changing;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (Control : in out Tab_Window_Control_Type)
   is
      use GWindows.Base;

      Current : constant Pointer_To_Base_Window_Class :=
        Tab_Window (Control, Selected_Tab (Control));
      Area    : constant GWindows.Types.Rectangle_Type :=
        Display_Area (Control);
   begin
      if Current /= null then
         Left (Current.all, Area.Left);
         Top (Current.all, Area.Top);
         Width (Current.all, Area.Right - Area.Left);
         Height (Current.all, Area.Bottom - Area.Top);
         Show (Current.all);
      end if;
      On_Change (Tab_Control_Type (Control));                        --  *AnSp
   end On_Change;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Toolbar_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles     : constant Interfaces.C.unsigned := 0;
   begin
      Create_Control (Control, Parent,
                      "ToolbarWindow32",
                      "",
                      Left, Top, Width, Height,
                      0, Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Control);
      end if;
   end Create;

   ---------------------
   -- Load_Image_List --
   ---------------------

   procedure Load_Image_List
     (Control   : in out Toolbar_Control_Type;
      Bitmap_ID : in     Integer)
   is
      use GWindows.Types;
      HINST_COMMCTRL : constant GWindows.Types.Lparam := 0 - 1;
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_LOADIMAGES;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Bitmap_ID);
         lParam : GWindows.Types.Lparam := HINST_COMMCTRL);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      SendMessage;
   end Load_Image_List;

   --------------------
   -- Set_Image_List --
   --------------------

   procedure Set_Image_List
     (Control : in out Toolbar_Control_Type;
      List    : in     GWindows.Image_Lists.Image_List_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETIMAGELIST;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Handle);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      SendMessage (lParam => GWindows.Image_Lists.Handle (List));
   end Set_Image_List;

   --------------------
   -- Add_Image_List --
   --------------------

   procedure Add_Image_List
     (Control   : in out Toolbar_Control_Type;
      Bitmap_ID : in     Integer;
      Index     :    out Integer)
   is
      type TBADDBITMAP is
         record
            hInst     : Integer;
            Bitmap_ID : Integer;
         end record;
      HINST_COMMCTRL : constant := -1;
      Add_Bitmap     : TBADDBITMAP := (HINST_COMMCTRL, Bitmap_ID);
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_ADDBITMAP;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := Add_Bitmap'Address)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Index := SendMessage;
   end Add_Image_List;

   ----------------
   -- Add_String --
   ----------------

   procedure Add_String
     (Control     : in out Toolbar_Control_Type;
      Text        : in     GString)
   is
      use GWindows.GStrings;
      TB_ADDSTRINGA : constant := WM_USER + 28;
      TB_ADDSTRINGW : constant := WM_USER + 77;
      TB_ADDSTRING : constant array (Character_Mode_Type) of
         Interfaces.C.int :=
            (ANSI    => TB_ADDSTRINGA,
             Unicode => TB_ADDSTRINGW);
      C_Text : GString_C := To_GString_C (Text & GCharacter'Val (0));
      --  C_Text has to have a double NUL at the end, since it is a list
      --  of NUL-separated C strings.

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_ADDSTRING (Character_Mode);
         wParam : GWindows.Types.Lparam := 0;
         lParam : System.Address        := C_Text'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Add_String;

   ----------------
   -- Add_Button --
   ----------------

   procedure Add_Button
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer)
   is

      type BUTTON_ARRAY is array (1 .. 1) of TBBUTTON;
      TB : BUTTON_ARRAY;
      TBSTYLE_AUTOSIZE : constant := 16#10#;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_ADDBUTTONS;
         wParam : GWindows.Types.Wparam := 1;
         lParam : BUTTON_ARRAY          := TB);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TB (1).Image := Image_Index;
      TB (1).Command := Command;
      TB (1).Style := TBSTYLE_AUTOSIZE;
      TB (1).State := TBSTATE_ENABLED;
      SendMessage;
   end Add_Button;

   procedure Add_Button
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer;
      IString     : in     Integer)
   is

      type BUTTON_ARRAY is array (1 .. 1) of TBBUTTON;
      TB : BUTTON_ARRAY;
      TBSTYLE_AUTOSIZE : constant := 16#10#;
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_ADDBUTTONS;
         wParam : GWindows.Types.Wparam := 1;
         lParam : BUTTON_ARRAY          := TB);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      TB (1).Image := Image_Index;
      TB (1).Command := Command;
      TB (1).Style := TBSTYLE_AUTOSIZE;
      TB (1).State := TBSTATE_ENABLED;
      TB (1).IString := IString;
      SendMessage;
   end Add_Button;

   ----------------------
   -- Set_Button_Image --
   ----------------------

   procedure Set_Button_Image
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer)
   is
      TB_CHANGEBITMAP : constant := WM_USER + 43;
      procedure SendMessage
       (hwnd   : GWindows.Types.Handle := Handle (Control);
        uMsg   : Interfaces.C.int      := TB_CHANGEBITMAP;
        wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Command);
        lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Image_Index));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Button_Image;

   ----------------------
   -- Set_Button_State --
   ----------------------

   procedure Set_Button_State
     (Control     : in out Toolbar_Control_Type;
      Command     : in     Integer;
      State       : in     Toolbar_Button_State)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETSTATE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Command);
         lParam : GWindows.Types.Lparam);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
      High : Interfaces.C.short;
   begin
      case State is
         when Normal =>
            High := TBSTATE_ENABLED;
         when Hidden =>
            High := TBSTATE_HIDDEN;
         when Disabled =>
            High := TBSTATE_INDETERMINATE;
      end case;
      SendMessage (lParam => GWindows.Utilities.Make_Long (High, 0));
   end Set_Button_State;

   -------------
   -- Visible --
   -------------

   function Button_State (Control : in Toolbar_Control_Type;
                          Command : in Integer) return Interfaces.C.unsigned is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_GETSTATE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Command);
         lParam : GWindows.Types.Lparam := 0) return Interfaces.C.unsigned;
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Button_State;

   procedure Button_State (Control : in Toolbar_Control_Type;
                           Command : in Integer;
                           State   : in Interfaces.C.unsigned) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETSTATE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Command);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (State));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Button_State;

   procedure Visible (Control : in out Toolbar_Control_Type;
                      Command : in     Integer;
                      State   : in     Boolean := True) is
   begin
      if State then
         Button_State (Control, Command,
                       Button_State (Control, Command) and not TBSTATE_HIDDEN);
      else
         Button_State (Control, Command,
                       Button_State (Control, Command) or TBSTATE_HIDDEN);
      end if;
   end Visible;

   function Visible (Control : in Toolbar_Control_Type;
                     Command : in Integer) return Boolean is
   begin
      return (Button_State (Control, Command) and TBSTATE_HIDDEN) = 0;
   end Visible;

   -------------
   -- Enabled --
   -------------

   procedure Enabled (Control : in out Toolbar_Control_Type;
                      Command : in     Integer;
                      State   : in     Boolean := True) is
   begin
      if State then
         Button_State (Control, Command,
                       Button_State (Control, Command) or TBSTATE_ENABLED);
      else
         Button_State
            (Control, Command,
             Button_State (Control, Command) and not TBSTATE_ENABLED);
      end if;
   end Enabled;

   function Enabled (Control : in Toolbar_Control_Type;
                     Command : in Integer) return Boolean is
   begin
      return (Button_State (Control, Command) and TBSTATE_ENABLED) =
             TBSTATE_ENABLED;
   end Enabled;

   -----------------
   -- Button_Size --
   -----------------

   procedure Button_Size (Control : in     Toolbar_Control_Type;
                          Width   :    out Integer;
                          Height  :    out Integer) is
      use type GWindows.Types.Lresult;
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_GETBUTTONSIZE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0) return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
      Result : GWindows.Types.Lresult;
   begin
      Result := SendMessage;
      Width := Integer (Result mod 65_536);
      Height := Integer (Result / 65_536);
   end Button_Size;

   -------------------
   -- Add_Separator --
   -------------------

   procedure Add_Separator
     (Control : in out Toolbar_Control_Type;
      Width   : in     Integer)
   is
      type BUTTON_ARRAY is array (1 .. 1) of TBBUTTON;
      TB : BUTTON_ARRAY;
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_ADDBUTTONS;
         wParam : GWindows.Types.Wparam := 1;
         lParam : BUTTON_ARRAY          := TB);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      TB (1).Image := Width;
      TB (1).Style := TBSTYLE_SEP;
      SendMessage;
   end Add_Separator;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Control    : in Toolbar_Control_Type)
     return Interfaces.C.unsigned
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_GETSTYLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return Interfaces.C.unsigned (SendMessage);
   end Get_Style;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Control    : in out Toolbar_Control_Type;
      Style      : in     Interfaces.C.unsigned)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETSTYLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Style));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Style;

   procedure Set_Extended_Style
     (Control    : in out Toolbar_Control_Type;
      Style      : in     Interfaces.C.unsigned)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETEXTENDEDSTYLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Style));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Extended_Style;

   --  * AnSp: Added a new record, 2 helper functions and 2 public functions
   ----------------------
   -- TBBUTTONONFO --
   ----------------------
   type TBBUTTONINFO is
   record
      Size    : Interfaces.C.int := TBBUTTONINFO'Size / 8;
      Mask    : Interfaces.C.unsigned := 0;
      Command : Interfaces.C.int := 0;
      Image   : Interfaces.C.int := 0;
      State   : Interfaces.C.unsigned_char := 0;
      Style   : Interfaces.C.unsigned_char := 0;
      Cx      : Interfaces.C.short := 0;
      Param   : GWindows.Types.Lparam := 0;
      Text    : LPTSTR := null;
      TxtLen  : Interfaces.C.int := 0;
   end record;

   procedure Get_Button_Info
     (Control    : in     Toolbar_Control_Type;
      Button     : in     Integer;
      Info       : in out TBBUTTONINFO)
   is
      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_GETBUTTONINFOA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Button);
         lParam : in out TBBUTTONINFO);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_GETBUTTONINFOW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Button);
         lParam : in out TBBUTTONINFO);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      pragma Warnings (Off);
      if Character_Mode = Unicode then
         SendMessageW (lParam => Info);
      else
         SendMessageA (lParam => Info);
      end if;
      pragma Warnings (On);
   end Get_Button_Info;

   procedure Set_Button_Info
     (Control    : in out Toolbar_Control_Type;
      Button     : in     Integer;
      Info       : in     TBBUTTONINFO)
   is
      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETBUTTONINFOA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Button);
         lParam : in     TBBUTTONINFO);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_SETBUTTONINFOW;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Button);
         lParam : in     TBBUTTONINFO);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      pragma Warnings (Off);
      if Character_Mode = Unicode then
         SendMessageW (lParam => Info);
      else
         SendMessageA (lParam => Info);
      end if;
      pragma Warnings (On);
   end Set_Button_Info;

   ----------------------
   -- Get_Button_Style --
   ----------------------

   function Get_Button_Style
     (Control    : in Toolbar_Control_Type;
      Button     : in Integer)
     return Integer
   is
      Info : TBBUTTONINFO;
   begin
      Info.Mask := TBIF_STYLE;
      Get_Button_Info (Control, Button, Info);
      return Integer (Info.Style);
   end Get_Button_Style;

   ----------------------
   -- Set_Button_Style --
   ----------------------

   procedure Set_Button_Style
     (Control    : in out Toolbar_Control_Type;
      Button     : in     Integer;
      Style      : in     Integer)
   is
      Info : TBBUTTONINFO;
   begin
      Info.Mask := TBIF_STYLE;
      Info.Style := Interfaces.C.unsigned_char (Style);
      Set_Button_Info (Control, Button, Info);
   end Set_Button_Style;

   ----------------------
   -- Get_Button_State --
   ----------------------

   function Get_Button_State
     (Control    : in Toolbar_Control_Type;
      Button     : in Integer)
     return Integer
   is
      Info : TBBUTTONINFO;
   begin
      Info.Mask := TBIF_STATE;
      Get_Button_Info (Control, Button, Info);
      return Integer (Info.State);
   end Get_Button_State;

   ----------------------
   -- Set_Button_State --
   ----------------------

   procedure Set_Button_State
     (Control    : in out Toolbar_Control_Type;
      Button     : in     Integer;
      State      : in     Integer)
   is
      Info : TBBUTTONINFO;
   begin
      Info.Mask := TBIF_STATE;
      Info.State := Interfaces.C.unsigned_char (State);
      Set_Button_Info (Control, Button, Info);
   end Set_Button_State;
   --  * AnSp: Up to here

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Toolbar_Control_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TB_BUTTONSTRUCTSIZE;
         wParam : GWindows.Types.Wparam := 20;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end On_Create;

   procedure On_Command
     (Window  : in out Toolbar_Control_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, Control);
   begin
      if Code = 0 then
         On_Button_Select (Toolbar_Control_Type'Class (Window), ID);
      end if;
   end On_Command;

   ------------------------------
   -- On_Button_Select_Handler --
   ------------------------------

   procedure On_Button_Select_Handler
     (Control  : in out Toolbar_Control_Type;
      Handler : in GWindows.Windows.Select_Event)
   is
   begin
      Control.On_Button_Select_Event := Handler;
   end On_Button_Select_Handler;

   ---------------------------
   -- Fire_On_Button_Select --
   ---------------------------

   procedure Fire_On_Button_Select (Control : in out Toolbar_Control_Type;
                                    Item    : in     Integer)
   is
      use GWindows.Base;
      use GWindows.Windows;

   begin
      if Control.On_Button_Select_Event /= null then
         Control.On_Button_Select_Event (Base_Window_Type'Class (Control),
                                         Item);
      end if;
   end Fire_On_Button_Select;

   ----------------------
   -- On_Button_Select --
   ----------------------

   procedure On_Button_Select (Control : in out Toolbar_Control_Type;
                               Item    : in     Integer)
   is
   begin
      Fire_On_Button_Select (Control, Item);
   end On_Button_Select;

   ------------
   -- Create --
   ------------

   procedure Create
     (Control    : in out Tool_Tip_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Is_Dynamic : in     Boolean                              := False)
   is
      WS_POPUP            : constant := 2147483648;
      TTS_ALWAYS_TIP      : constant := 1;
      TTS_NO_PREFIX       : constant := 2;
   begin
      Create_Control (Control, Parent,
                      "tooltips_class32", "",
                      0, 0, 0, 0, 0,
                      WS_POPUP or TTS_ALWAYS_TIP or TTS_NO_PREFIX,
                      Is_Dynamic => Is_Dynamic);
   end Create;

   -----------------
   -- Add_Tooltip --
   -----------------

   procedure Add_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class;
      Tip     : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Tip);

      Info : TOOLINFO;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_ADDTOOLA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_ADDTOOLW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
      use GWindows.Base;
   begin
      Info.Flags := TTF_IDISHWND or TTF_SUBCLASS;
      if Parent (Control) /= null then
         Info.HWND := GWindows.Base.Handle (Parent (Control).all);
      end if;
      Info.UID := GWindows.Base.Handle (Window);
      Info.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
      end case;
   end Add_Tool_Tip;

   ---------------------
   -- Update_Tool_Tip --
   ---------------------

   procedure Update_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class;
      Tip     : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Tip);
      Info   : TOOLINFO;

      WM_USER            : constant := 16#400#;
      TTM_UPDATETIPTEXTA : constant := WM_USER + 12;
      TTM_UPDATETIPTEXTW : constant := WM_USER + 57;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_UPDATETIPTEXTA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_UPDATETIPTEXTW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Info.Flags := TTF_IDISHWND or TTF_SUBCLASS;
      Info.HWND := GWindows.Base.Handle (Parent (Control).all);
      Info.UID := GWindows.Base.Handle (Window);
      Info.Text := C_Text (0)'Unchecked_Access;
      pragma Warnings (Off);
      if Character_Mode = Unicode then
         SendMessageW;
      else
         SendMessageA;
      end if;
      pragma Warnings (On);
   end Update_Tool_Tip;

   ---------------------
   -- Delete_Tool_Tip --
   ---------------------

   procedure Delete_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class) is

      Info : TOOLINFO;

      procedure SendMessageA
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_DELTOOLA;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure SendMessageW
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_DELTOOLW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : TOOLINFO              := Info);
      pragma Import (StdCall, SendMessageW,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Info.HWND := GWindows.Base.Handle (Parent (Control).all);
      Info.UID := GWindows.Base.Handle (Window);
      pragma Warnings (Off);
      if Character_Mode = Unicode then
         SendMessageW;
      else
         SendMessageA;
      end if;
      pragma Warnings (On);
   end Delete_Tool_Tip;

   -------------------
   -- Maximum_Width --
   -------------------

   procedure Maximum_Width (Control : in out Tool_Tip_Type;
                            Width   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_SETMAXTIPWIDTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Width));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Maximum_Width;

   -------------------
   -- Get_Durations --
   -------------------

   procedure Get_Durations
     (Control  : in out Tool_Tip_Type;
      Initial  : out Duration;
      Reshow   : out Duration;
      Til_Hide : out Duration)
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_GETDELAYTIME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0) return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
      TTDT_RESHOW  : constant := 1;
      TTDT_AUTOPOP : constant := 2;
      TTDT_INITIAL : constant := 3;
   begin
      Initial  := Duration (SendMessage (wParam => TTDT_INITIAL)) / 1000.0;
      Reshow   := Duration (SendMessage (wParam => TTDT_RESHOW)) / 1000.0;
      Til_Hide := Duration (SendMessage (wParam => TTDT_AUTOPOP)) / 1000.0;
   end Get_Durations;

   -------------------
   -- Set_Durations --
   -------------------

   procedure Set_Durations
     (Control  : in out Tool_Tip_Type;
      Initial  : in Duration;
      Reshow   : in Duration;
      Til_Hide : in Duration)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := TTM_SETDELAYTIME;
         wParam : GWindows.Types.Wparam;
         lParam : GWindows.Types.Lparam);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
      TTDT_RESHOW  : constant := 1;
      TTDT_AUTOPOP : constant := 2;
      TTDT_INITIAL : constant := 3;
   begin
      SendMessage (wParam => TTDT_INITIAL,
                   lParam => GWindows.Types.Lparam (Initial * 1000.0));
      SendMessage (wParam => TTDT_RESHOW,
                   lParam => GWindows.Types.Lparam (Reshow * 1000.0));
      SendMessage (wParam => TTDT_AUTOPOP,
                   lParam => GWindows.Types.Lparam (Til_Hide * 1000.0));
   end Set_Durations;

end GWindows.Common_Controls;
