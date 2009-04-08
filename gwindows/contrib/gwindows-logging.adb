with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO;  use Ada.Text_IO;

package body GWindows.Logging is

   WM_APP                    : constant := 32768;
   WM_ACTIVATE               : constant := 6;
   WM_ACTIVATEAPP            : constant := 28;
   WM_AFXFIRST               : constant := 864;
   WM_AFXLAST                : constant := 895;
   WM_ASKCBFORMATNAME        : constant := 780;
   WM_CANCELJOURNAL          : constant := 75;
   WM_CANCELMODE             : constant := 31;
   WM_CAPTURECHANGED         : constant := 533;
   WM_CHANGECBCHAIN          : constant := 781;
   WM_CHAR                   : constant := 258;
   WM_CHARTOITEM             : constant := 47;
   WM_CHILDACTIVATE          : constant := 34;
   WM_CLEAR                  : constant := 771;
   WM_CLOSE                  : constant := 16;
   WM_COMMAND                : constant := 273;
   WM_COMMNOTIFY             : constant := 68;
   WM_COMPACTING             : constant := 65;
   WM_COMPAREITEM            : constant := 57;
   WM_CONTEXTMENU            : constant := 123;
   WM_COPY                   : constant := 769;
   WM_COPYDATA               : constant := 74;
   WM_CREATE                 : constant := 1;
   WM_CTLCOLORBTN            : constant := 309;
   WM_CTLCOLORDLG            : constant := 310;
   WM_CTLCOLOREDIT           : constant := 307;
   WM_CTLCOLORLISTBOX        : constant := 308;
   WM_CTLCOLORMSGBOX         : constant := 306;
   WM_CTLCOLORSCROLLBAR      : constant := 311;
   WM_CTLCOLORSTATIC         : constant := 312;
   WM_CUT                    : constant := 768;
   WM_DEADCHAR               : constant := 259;
   WM_DELETEITEM             : constant := 45;
   WM_DESTROY                : constant := 2;
   WM_DESTROYCLIPBOARD       : constant := 775;
   WM_DEVICECHANGE           : constant := 537;
   WM_DEVMODECHANGE          : constant :=  27;
   WM_DISPLAYCHANGE          : constant := 126;
   WM_DRAWCLIPBOARD          : constant := 776;
   WM_DRAWITEM               : constant :=  43;
   WM_DROPFILES              : constant := 563;
   WM_ENABLE                 : constant :=  10;
   WM_ENDSESSION             : constant :=  22;
   WM_ENTERIDLE              : constant := 289;
   WM_ENTERMENULOOP          : constant := 529;
   WM_ENTERSIZEMOVE          : constant := 561;
   WM_ERASEBKGND             : constant := 20;
   WM_EXITMENULOOP           : constant := 530;
   WM_EXITSIZEMOVE           : constant := 562;
   WM_FONTCHANGE             : constant := 29;
   WM_GETDLGCODE             : constant := 135;
   WM_GETFONT                : constant :=  49;
   WM_GETHOTKEY              : constant :=  51;
   WM_GETICON                : constant := 127;
   WM_GETMINMAXINFO          : constant :=  36;
   WM_GETTEXT                : constant :=  13;
   WM_GETTEXTLENGTH          : constant :=  14;
   WM_HANDHELDFIRST          : constant := 856;
   WM_HANDHELDLAST           : constant := 863;
   WM_HELP                   : constant :=  83;
   WM_HOTKEY                 : constant := 786;
   WM_HSCROLL                : constant := 276;
   WM_HSCROLLCLIPBOARD       : constant := 782;
   WM_ICONERASEBKGND         : constant := 39;
   WM_INITDIALOG             : constant := 272;
   WM_INITMENU               : constant := 278;
   WM_INITMENUPOPUP          : constant := 279;
   WM_INPUTLANGCHANGE        : constant := 81;
   WM_INPUTLANGCHANGEREQUEST : constant := 80;
   WM_KEYDOWN                : constant := 256;
   WM_KEYUP                  : constant := 257;
   WM_KILLFOCUS              : constant :=  8;
   WM_MDIACTIVATE            : constant := 546;
   WM_MDICASCADE             : constant := 551;
   WM_MDICREATE              : constant := 544;
   WM_MDIDESTROY             : constant := 545;
   WM_MDIGETACTIVE           : constant := 553;
   WM_MDIICONARRANGE         : constant := 552;
   WM_MDIMAXIMIZE            : constant := 549;
   WM_MDINEXT                : constant := 548;
   WM_MDIREFRESHMENU         : constant := 564;
   WM_MDIRESTORE             : constant := 547;
   WM_MDISETMENU             : constant := 560;
   WM_MDITILE                : constant := 550;
   WM_MEASUREITEM            : constant := 44;
   WM_UNINITMENUPOPUP        : constant := 16#0125#;
   WM_MENURBUTTONUP          : constant := 290;
   WM_MENUCOMMAND            : constant := 16#0126#;
   WM_MENUGETOBJECT          : constant := 16#0124#;
   WM_MENUDRAG               : constant := 16#0123#;
   WM_MENUCHAR               : constant := 288;
   WM_MENUSELECT             : constant := 287;
   WM_NEXTMENU               : constant := 531;
   WM_MOVE                   : constant :=  3;
   WM_MOVING                 : constant := 534;
   WM_NCACTIVATE             : constant := 134;
   WM_NCCALCSIZE             : constant := 131;
   WM_NCCREATE               : constant := 129;
   WM_NCDESTROY              : constant := 130;
   WM_NCHITTEST              : constant := 132;
   WM_NCLBUTTONDBLCLK        : constant := 163;
   WM_NCLBUTTONDOWN          : constant := 161;
   WM_NCLBUTTONUP            : constant := 162;
   WM_NCMBUTTONDBLCLK        : constant := 169;
   WM_NCMBUTTONDOWN          : constant := 167;
   WM_NCMBUTTONUP            : constant := 168;
   WM_NCXBUTTONDOWN          : constant := 171;
   WM_NCXBUTTONUP            : constant := 172;
   WM_NCXBUTTONDBLCLK        : constant := 173;
   WM_NCMOUSEHOVER           : constant := 16#02A0#;
   WM_NCMOUSELEAVE           : constant := 16#02A2#;
   WM_NCMOUSEMOVE            : constant := 160;
   WM_NCPAINT                : constant := 133;
   WM_NCRBUTTONDBLCLK        : constant := 166;
   WM_NCRBUTTONDOWN          : constant := 164;
   WM_NCRBUTTONUP            : constant := 165;
   WM_NEXTDLGCTL             : constant :=  40;
   WM_NOTIFY                 : constant :=  78;
   WM_NOTIFYFORMAT           : constant :=  85;
   WM_NULL                   : constant := 0;
   WM_PAINT                  : constant :=  15;
   WM_PAINTCLIPBOARD         : constant := 777;
   WM_PAINTICON              : constant :=  38;
   WM_PALETTECHANGED         : constant := 785;
   WM_PALETTEISCHANGING      : constant := 784;
   WM_PARENTNOTIFY           : constant := 528;
   WM_PASTE                  : constant := 770;
   WM_PENWINFIRST            : constant := 896;
   WM_PENWINLAST             : constant := 911;
   WM_POWER                  : constant :=  72;
   WM_POWERBROADCAST         : constant := 536;
   WM_PRINT                  : constant := 791;
   WM_PRINTCLIENT            : constant := 792;
   WM_QUERYDRAGICON          : constant :=  55;
   WM_QUERYENDSESSION        : constant :=  17;
   WM_QUERYNEWPALETTE        : constant := 783;
   WM_QUERYOPEN              : constant :=  19;
   WM_QUEUESYNC              : constant :=  35;
   WM_QUIT                   : constant :=  18;
   WM_RENDERALLFORMATS       : constant := 774;
   WM_RENDERFORMAT           : constant := 773;
   WM_SETCURSOR              : constant :=  32;
   WM_SETFOCUS               : constant := 7;
   WM_SETFONT                : constant :=  48;
   WM_SETHOTKEY              : constant :=  50;
   WM_SETICON                : constant := 128;
   WM_SETREDRAW              : constant :=  11;
   WM_SETTEXT                : constant :=  12;
   WM_SETTINGCHANGE          : constant :=  26;
   WM_SHOWWINDOW             : constant :=  24;
   WM_SIZE                   : constant := 5;
   WM_SIZECLIPBOARD          : constant := 779;
   WM_SIZING                 : constant := 532;
   WM_SPOOLERSTATUS          : constant :=  42;
   WM_STYLECHANGED           : constant := 125;
   WM_STYLECHANGING          : constant := 124;
   WM_SYSCHAR                : constant := 262;
   WM_SYSCOLORCHANGE         : constant :=  21;
   WM_SYSCOMMAND             : constant := 274;
   WM_SYSDEADCHAR            : constant := 263;
   WM_SYSKEYDOWN             : constant := 260;
   WM_SYSKEYUP               : constant := 261;
   WM_TCARD                  : constant :=  82;
   WM_THEMECHANGED           : constant := 794;
   WM_TIMECHANGE             : constant :=  30;
   WM_TIMER                  : constant := 275;
   WM_UNDO                   : constant := 772;
   WM_USER                   : constant := 1024;
   WM_USERCHANGED            : constant :=  84;
   WM_VKEYTOITEM             : constant :=  46;
   WM_VSCROLL                : constant := 277;
   WM_VSCROLLCLIPBOARD       : constant := 778;
   WM_WINDOWPOSCHANGED       : constant :=  71;
   WM_WINDOWPOSCHANGING      : constant :=  70;
   WM_KEYLAST                : constant := 264;
   WM_SYNCPAINT              : constant := 136;
   WM_MOUSEACTIVATE          : constant :=  33;
   WM_MOUSEMOVE              : constant := 512;
   WM_LBUTTONDOWN            : constant := 513;
   WM_LBUTTONUP              : constant := 514;
   WM_LBUTTONDBLCLK          : constant := 515;
   WM_RBUTTONDOWN            : constant := 516;
   WM_RBUTTONUP              : constant := 517;
   WM_RBUTTONDBLCLK          : constant := 518;
   WM_MBUTTONDOWN            : constant := 519;
   WM_MBUTTONUP              : constant := 520;
   WM_MBUTTONDBLCLK          : constant := 521;
   WM_MOUSEWHEEL             : constant := 522;
   WM_XBUTTONDOWN            : constant := 523;
   WM_XBUTTONUP              : constant := 524;
   WM_XBUTTONDBLCLK          : constant := 525;
   WM_MOUSEHOVER             : constant := 16#2A1#;
   WM_MOUSELEAVE             : constant := 16#2A3#;

   Log_File : File_Type;

   function Itos (I : Integer; Length : Integer) return String is
      S     : String (1 .. Length);
      Hlp_I : Integer := I;
   begin
      for J in 1 .. Length loop
         S (Length - J + 1) :=
            Character'Val (Character'Pos ('0') + Hlp_I mod 10);
         Hlp_I := Hlp_I / 10;
      end loop;
      return S;
   end Itos;

   type Month_Names is (JAN, FEB, MAR, APR, MAY, JUN,
                        JUL, AUG, SEP, OCT, NOV, DEC);

   function Date_Time return String is
      Curr_Time : Time := Clock;
      Secs_100  : Long_Long_Integer :=
         Long_Long_Integer'Max
          (Long_Long_Integer (Duration (Seconds (Curr_Time) * 100 -
             Duration (0.5))), 0);
      Hour      : Integer;
      Minute    : Integer;
      Second    : Integer;
      Csecond   : Integer;
   begin
      Csecond := Integer (Secs_100 mod 100); Secs_100 := Secs_100 / 100;
      Second  := Integer (Secs_100 mod  60); Secs_100 := Secs_100 /  60;
      Minute  := Integer (Secs_100 mod  60); Secs_100 := Secs_100 /  60;
      Hour    := Integer (Secs_100 mod  24); Secs_100 := Secs_100 /  24;
      return Itos (Day (Curr_Time), 2) & "-" &
      Month_Names'Val (Month (Curr_Time))'Img & "-" &
      Itos (Year (Curr_Time), 4) & " " &
      Itos (Hour, 2) & ":" & Itos (Minute, 2) & ":" & Itos (Second, 2) & "." &
      Itos (Csecond, 2);
   end Date_Time;

   procedure Log (Line : String) is
   begin
      if not Is_Open (Log_File) then
         Create (Log_File, Out_File, "C:\Temp\GWindows.log");
      end if;
      Put_Line (Log_File, Date_Time & " " & Line);
      Flush (Log_File);
   end Log;

   function Message_Number_To_Name (Number : Integer) return String is
   begin
      case Number is
         when WM_APP =>
            return "WM_APP";
         when WM_ACTIVATE =>
            return "WM_ACTIVATE";
         when WM_ACTIVATEAPP =>
            return "WM_ACTIVATEAPP";
         when WM_AFXFIRST =>
            return "WM_AFXFIRST";
         when WM_AFXLAST =>
            return "WM_AFXLAST";
         when WM_ASKCBFORMATNAME =>
            return "WM_ASKCBFORMATNAME";
         when WM_CANCELJOURNAL =>
            return "WM_CANCELJOURNAL";
         when WM_CANCELMODE =>
            return "WM_CANCELMODE";
         when WM_CAPTURECHANGED =>
            return "WM_CAPTURECHANGED";
         when WM_CHANGECBCHAIN =>
            return "WM_CHANGECBCHAIN";
         when WM_CHAR =>
            return "WM_CHAR";
         when WM_CHARTOITEM =>
            return "WM_CHARTOITEM";
         when WM_CHILDACTIVATE =>
            return "WM_CHILDACTIVATE";
         when WM_CLEAR =>
            return "WM_CLEAR";
         when WM_CLOSE =>
            return "WM_CLOSE";
         when WM_COMMAND =>
            return "WM_COMMAND";
         when WM_COMMNOTIFY =>
            return "WM_COMMNOTIFY";
         when WM_COMPACTING =>
            return "WM_COMPACTING";
         when WM_COMPAREITEM =>
            return "WM_COMPAREITEM";
         when WM_CONTEXTMENU =>
            return "WM_CONTEXTMENU";
         when WM_COPY =>
            return "WM_COPY";
         when WM_COPYDATA =>
            return "WM_COPYDATA";
         when WM_CREATE =>
            return "WM_CREATE";
         when WM_CTLCOLORBTN =>
            return "WM_CTLCOLORBTN";
         when WM_CTLCOLORDLG =>
            return "WM_CTLCOLORDLG";
         when WM_CTLCOLOREDIT =>
            return "WM_CTLCOLOREDIT";
         when WM_CTLCOLORLISTBOX =>
            return "WM_CTLCOLORLISTBOX";
         when WM_CTLCOLORMSGBOX =>
            return "WM_CTLCOLORMSGBOX";
         when WM_CTLCOLORSCROLLBAR =>
            return "WM_CTLCOLORSCROLLBAR";
         when WM_CTLCOLORSTATIC =>
            return "WM_CTLCOLORSTATIC";
         when WM_CUT =>
            return "WM_CUT";
         when WM_DEADCHAR =>
            return "WM_DEADCHAR";
         when WM_DELETEITEM =>
            return "WM_DELETEITEM";
         when WM_DESTROY =>
            return "WM_DESTROY";
         when WM_DESTROYCLIPBOARD =>
            return "WM_DESTROYCLIPBOARD";
         when WM_DEVICECHANGE =>
            return "WM_DEVICECHANGE";
         when WM_DEVMODECHANGE =>
            return "WM_DEVMODECHANGE";
         when WM_DISPLAYCHANGE =>
            return "WM_DISPLAYCHANGE";
         when WM_DRAWCLIPBOARD =>
            return "WM_DRAWCLIPBOARD";
         when WM_DRAWITEM =>
            return "WM_DRAWITEM";
         when WM_DROPFILES =>
            return "WM_DROPFILES";
         when WM_ENABLE =>
            return "WM_ENABLE";
         when WM_ENDSESSION =>
            return "WM_ENDSESSION";
         when WM_ENTERIDLE =>
            return "WM_ENTERIDLE";
         when WM_ENTERMENULOOP =>
            return "WM_ENTERMENULOOP";
         when WM_ENTERSIZEMOVE =>
            return "WM_ENTERSIZEMOVE";
         when WM_ERASEBKGND =>
            return "WM_ERASEBKGND";
         when WM_EXITMENULOOP =>
            return "WM_EXITMENULOOP";
         when WM_EXITSIZEMOVE =>
            return "WM_EXITSIZEMOVE";
         when WM_FONTCHANGE =>
            return "WM_FONTCHANGE";
         when WM_GETDLGCODE =>
            return "WM_GETDLGCODE";
         when WM_GETFONT =>
            return "WM_GETFONT";
         when WM_GETHOTKEY =>
            return "WM_GETHOTKEY";
         when WM_GETICON =>
            return "WM_GETICON";
         when WM_GETMINMAXINFO =>
            return "WM_GETMINMAXINFO";
         when WM_GETTEXT =>
            return "WM_GETTEXT";
         when WM_GETTEXTLENGTH =>
            return "WM_GETTEXTLENGTH";
         when WM_HANDHELDFIRST =>
            return "WM_HANDHELDFIRST";
         when WM_HANDHELDLAST =>
            return "WM_HANDHELDLAST";
         when WM_HELP =>
            return "WM_HELP";
         when WM_HOTKEY =>
            return "WM_HOTKEY";
         when WM_HSCROLL =>
            return "WM_HSCROLL";
         when WM_HSCROLLCLIPBOARD =>
            return "WM_HSCROLLCLIPBOARD";
         when WM_ICONERASEBKGND =>
            return "WM_ICONERASEBKGND";
         when WM_INITDIALOG =>
            return "WM_INITDIALOG";
         when WM_INITMENU =>
            return "WM_INITMENU";
         when WM_INITMENUPOPUP =>
            return "WM_INITMENUPOPUP";
         when WM_INPUTLANGCHANGE =>
            return "WM_INPUTLANGCHANGE";
         when WM_INPUTLANGCHANGEREQUEST =>
            return "WM_INPUTLANGCHANGEREQUEST";
         when WM_KEYDOWN =>
            return "WM_KEYDOWN";
         when WM_KEYUP =>
            return "WM_KEYUP";
         when WM_KILLFOCUS =>
            return "WM_KILLFOCUS";
         when WM_MDIACTIVATE =>
            return "WM_MDIACTIVATE";
         when WM_MDICASCADE =>
            return "WM_MDICASCADE";
         when WM_MDICREATE =>
            return "WM_MDICREATE";
         when WM_MDIDESTROY =>
            return "WM_MDIDESTROY";
         when WM_MDIGETACTIVE =>
            return "WM_MDIGETACTIVE";
         when WM_MDIICONARRANGE =>
            return "WM_MDIICONARRANGE";
         when WM_MDIMAXIMIZE =>
            return "WM_MDIMAXIMIZE";
         when WM_MDINEXT =>
            return "WM_MDINEXT";
         when WM_MDIREFRESHMENU =>
            return "WM_MDIREFRESHMENU";
         when WM_MDIRESTORE =>
            return "WM_MDIRESTORE";
         when WM_MDISETMENU =>
            return "WM_MDISETMENU";
         when WM_MDITILE =>
            return "WM_MDITILE";
         when WM_MEASUREITEM =>
            return "WM_MEASUREITEM";
         when WM_UNINITMENUPOPUP =>
            return "WM_UNINITMENUPOPUP";
         when WM_MENURBUTTONUP =>
            return "WM_MENURBUTTONUP";
         when WM_MENUCOMMAND =>
            return "WM_MENUCOMMAND";
         when WM_MENUGETOBJECT =>
            return "WM_MENUGETOBJECT";
         when WM_MENUDRAG =>
            return "WM_MENUDRAG";
         when WM_MENUCHAR =>
            return "WM_MENUCHAR";
         when WM_MENUSELECT =>
            return "WM_MENUSELECT";
         when WM_NEXTMENU =>
            return "WM_NEXTMENU";
         when WM_MOVE =>
            return "WM_MOVE";
         when WM_MOVING =>
            return "WM_MOVING";
         when WM_NCACTIVATE =>
            return "WM_NCACTIVATE";
         when WM_NCCALCSIZE =>
            return "WM_NCCALCSIZE";
         when WM_NCCREATE =>
            return "WM_NCCREATE";
         when WM_NCDESTROY =>
            return "WM_NCDESTROY";
         when WM_NCHITTEST =>
            return "WM_NCHITTEST";
         when WM_NCLBUTTONDBLCLK =>
            return "WM_NCLBUTTONDBLCLK";
         when WM_NCLBUTTONDOWN =>
            return "WM_NCLBUTTONDOWN";
         when WM_NCLBUTTONUP =>
            return "WM_NCLBUTTONUP";
         when WM_NCMBUTTONDBLCLK =>
            return "WM_NCMBUTTONDBLCLK";
         when WM_NCMBUTTONDOWN =>
            return "WM_NCMBUTTONDOWN";
         when WM_NCMBUTTONUP =>
            return "WM_NCMBUTTONUP";
         when WM_NCXBUTTONDOWN =>
            return "WM_NCXBUTTONDOWN";
         when WM_NCXBUTTONUP =>
            return "WM_NCXBUTTONUP";
         when WM_NCXBUTTONDBLCLK =>
            return "WM_NCXBUTTONDBLCLK";
         when WM_NCMOUSEHOVER =>
            return "WM_NCMOUSEHOVER";
         when WM_NCMOUSELEAVE =>
            return "WM_NCMOUSELEAVE";
         when WM_NCMOUSEMOVE =>
            return "WM_NCMOUSEMOVE";
         when WM_NCPAINT =>
            return "WM_NCPAINT";
         when WM_NCRBUTTONDBLCLK =>
            return "WM_NCRBUTTONDBLCLK";
         when WM_NCRBUTTONDOWN =>
            return "WM_NCRBUTTONDOWN";
         when WM_NCRBUTTONUP =>
            return "WM_NCRBUTTONUP";
         when WM_NEXTDLGCTL =>
            return "WM_NEXTDLGCTL";
         when WM_NOTIFY =>
            return "WM_NOTIFY";
         when WM_NOTIFYFORMAT =>
            return "WM_NOTIFYFORMAT";
         when WM_NULL =>
            return "WM_NULL";
         when WM_PAINT =>
            return "WM_PAINT";
         when WM_PAINTCLIPBOARD =>
            return "WM_PAINTCLIPBOARD";
         when WM_PAINTICON =>
            return "WM_PAINTICON";
         when WM_PALETTECHANGED =>
            return "WM_PALETTECHANGED";
         when WM_PALETTEISCHANGING =>
            return "WM_PALETTEISCHANGING";
         when WM_PARENTNOTIFY =>
            return "WM_PARENTNOTIFY";
         when WM_PASTE =>
            return "WM_PASTE";
         when WM_PENWINFIRST =>
            return "WM_PENWINFIRST";
         when WM_PENWINLAST =>
            return "WM_PENWINLAST";
         when WM_POWER =>
            return "WM_POWER";
         when WM_POWERBROADCAST =>
            return "WM_POWERBROADCAST";
         when WM_PRINT =>
            return "WM_PRINT";
         when WM_PRINTCLIENT =>
            return "WM_PRINTCLIENT";
         when WM_QUERYDRAGICON =>
            return "WM_QUERYDRAGICON";
         when WM_QUERYENDSESSION =>
            return "WM_QUERYENDSESSION";
         when WM_QUERYNEWPALETTE =>
            return "WM_QUERYNEWPALETTE";
         when WM_QUERYOPEN =>
            return "WM_QUERYOPEN";
         when WM_QUEUESYNC =>
            return "WM_QUEUESYNC";
         when WM_QUIT =>
            return "WM_QUIT";
         when WM_RENDERALLFORMATS =>
            return "WM_RENDERALLFORMATS";
         when WM_RENDERFORMAT =>
            return "WM_RENDERFORMAT";
         when WM_SETCURSOR =>
            return "WM_SETCURSOR";
         when WM_SETFOCUS =>
            return "WM_SETFOCUS";
         when WM_SETFONT =>
            return "WM_SETFONT";
         when WM_SETHOTKEY =>
            return "WM_SETHOTKEY";
         when WM_SETICON =>
            return "WM_SETICON";
         when WM_SETREDRAW =>
            return "WM_SETREDRAW";
         when WM_SETTEXT =>
            return "WM_SETTEXT";
         when WM_SETTINGCHANGE =>
            return "WM_SETTINGCHANGE";
         when WM_SHOWWINDOW =>
            return "WM_SHOWWINDOW";
         when WM_SIZE =>
            return "WM_SIZE";
         when WM_SIZECLIPBOARD =>
            return "WM_SIZECLIPBOARD";
         when WM_SIZING =>
            return "WM_SIZING";
         when WM_SPOOLERSTATUS =>
            return "WM_SPOOLERSTATUS";
         when WM_STYLECHANGED =>
            return "WM_STYLECHANGED";
         when WM_STYLECHANGING =>
            return "WM_STYLECHANGING";
         when WM_SYSCHAR =>
            return "WM_SYSCHAR";
         when WM_SYSCOLORCHANGE =>
            return "WM_SYSCOLORCHANGE";
         when WM_SYSCOMMAND =>
            return "WM_SYSCOMMAND";
         when WM_SYSDEADCHAR =>
            return "WM_SYSDEADCHAR";
         when WM_SYSKEYDOWN =>
            return "WM_SYSKEYDOWN";
         when WM_SYSKEYUP =>
            return "WM_SYSKEYUP";
         when WM_TCARD =>
            return "WM_TCARD";
         when WM_THEMECHANGED =>
            return "WM_THEMECHANGED";
         when WM_TIMECHANGE =>
            return "WM_TIMECHANGE";
         when WM_TIMER =>
            return "WM_TIMER";
         when WM_UNDO =>
            return "WM_UNDO";
         when WM_USER =>
            return "WM_USER";
         when WM_USERCHANGED =>
            return "WM_USERCHANGED";
         when WM_VKEYTOITEM =>
            return "WM_VKEYTOITEM";
         when WM_VSCROLL =>
            return "WM_VSCROLL";
         when WM_VSCROLLCLIPBOARD =>
            return "WM_VSCROLLCLIPBOARD";
         when WM_WINDOWPOSCHANGED =>
            return "WM_WINDOWPOSCHANGED";
         when WM_WINDOWPOSCHANGING =>
            return "WM_WINDOWPOSCHANGING";
         when WM_KEYLAST =>
            return "WM_KEYLAST";
         when WM_SYNCPAINT =>
            return "WM_SYNCPAINT";
         when WM_MOUSEACTIVATE =>
            return "WM_MOUSEACTIVATE";
         when WM_MOUSEMOVE =>
            return "WM_MOUSEMOVE";
         when WM_LBUTTONDOWN =>
            return "WM_LBUTTONDOWN";
         when WM_LBUTTONUP =>
            return "WM_LBUTTONUP";
         when WM_LBUTTONDBLCLK =>
            return "WM_LBUTTONDBLCLK";
         when WM_RBUTTONDOWN =>
            return "WM_RBUTTONDOWN";
         when WM_RBUTTONUP =>
            return "WM_RBUTTONUP";
         when WM_RBUTTONDBLCLK =>
            return "WM_RBUTTONDBLCLK";
         when WM_MBUTTONDOWN =>
            return "WM_MBUTTONDOWN";
         when WM_MBUTTONUP =>
            return "WM_MBUTTONUP";
         when WM_MBUTTONDBLCLK =>
            return "WM_MBUTTONDBLCLK";
         when WM_MOUSEWHEEL =>
            return "WM_MOUSEWHEEL";
         when WM_XBUTTONDOWN =>
            return "WM_XBUTTONDOWN";
         when WM_XBUTTONUP =>
            return "WM_XBUTTONUP";
         when WM_XBUTTONDBLCLK =>
            return "WM_XBUTTONDBLCLK";
         when WM_MOUSEHOVER =>
            return "WM_MOUSEHOVER";
         when WM_MOUSELEAVE =>
            return "WM_MOUSELEAVE";
         when others =>
            return Number'Img;
      end case;
   end Message_Number_To_Name;

end GWindows.Logging;
