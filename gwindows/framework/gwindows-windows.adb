------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                    G W I N D O W S . W I N D O W S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
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

with GWindows.Cursors;
with GWindows.GStrings;
with GWindows.Utilities;
with GWindows.Internal;
with GWindows.Packing_Boxes;

pragma Elaborate_All (GWindows.Cursors);

package body GWindows.Windows is
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use GWindows.Types;

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   procedure Translate_Key
     (wParam      : GWindows.Types.Wparam;
      Special_Key : out Special_Key_Type;
      Key         : out GCharacter);
   --  Translates Windows key information to GWindows key information

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

--   WM_MDICREATE               : constant := 544;
--   WM_MDIDESTROY              : constant := 545;
--   WM_MDIRESTORE              : constant := 547;
--   WM_MDINEXT                 : constant := 548;
--   WM_MDIMAXIMIZE             : constant := 549;
   WM_MDITILE                 : constant := 550;
   WM_MDICASCADE              : constant := 551;
   WM_MDIICONARRANGE          : constant := 552;
   WM_MDIGETACTIVE            : constant := 553;
   WM_MDISETMENU              : constant := 560;
   WM_MDIACTIVATE             : constant := 546;
   MDITILE_VERTICAL           : constant := 0;
   MDITILE_HORIZONTAL         : constant := 1;
   MDITILE_SKIPDISABLED       : constant := 2;

   SB_HORZ                    : constant := 0;
   SB_VERT                    : constant := 1;

   SIF_RANGE           : constant := 1;
   SIF_PAGE            : constant := 2;
   SIF_POS             : constant := 4;
--   SIF_DISABLENOSCROLL : constant := 8;
   SIF_TRACKPOS        : constant := 16;

   type SCROLLINFO is
      record
         cbSize    : Interfaces.C.unsigned := 28;
         fMask     : Natural;
         nMin      : Integer := 0;
         nMax      : Integer := 0;
         nPage     : Natural;
         nPos      : Integer := 0;
         nTrackPos : Integer := 0;
      end record;

   procedure SetScrollInfo
     (hwnd    : GWindows.Types.Handle;
      fnBar   : Interfaces.C.int;
      lpsi    : SCROLLINFO;
      fRedraw : Interfaces.C.long     := 1);
   pragma Import (StdCall, SetScrollInfo, "SetScrollInfo");

   procedure GetScrollInfo
     (hwnd    : GWindows.Types.Handle;
      fnBar   : Interfaces.C.int;
      lpsi    : SCROLLINFO);
   pragma Import (StdCall, GetScrollInfo, "GetScrollInfo");

   WM_SETICON                 : constant := 128;

   ICON_SMALL                 : constant := 0;
   ICON_BIG                   : constant := 1;

   IMAGE_ICON                 : constant := 1;

   LR_DEFAULTSIZE             : constant := 64;

   function LoadImage
     (hInst     : GWindows.Types.Handle := GWindows.Internal.Current_hInstance;
      lpszName  : GString_C;
      uType     : Interfaces.C.int := IMAGE_ICON;
      cxDesired : Interfaces.C.int := LR_DEFAULTSIZE;
      cyDesired : Interfaces.C.int := LR_DEFAULTSIZE;
      fuLoad    : Interfaces.C.int := 0)
     return GWindows.Types.Handle;
   pragma Import (StdCall, LoadImage, "LoadImage" & Character_Mode_Identifier);

   TPM_RIGHTBUTTON            : constant := 2;
   TPM_LEFTALIGN              : constant := 0;

   procedure TrackPopupMenu
     (hMenu     : GWindows.Menus.Menu_Type;
      fuFlags   : Interfaces.C.unsigned;
      x         : Integer;
      y         : Integer;
      nReserved : Interfaces.C.int;
      hwnd      : GWindows.Types.Handle;
      lprc      : Interfaces.C.long := 0);
   pragma Import (StdCall, TrackPopupMenu, "TrackPopupMenu");

   type PS_RESERVE_TYPE is array (1 .. 32) of Character;
   type PAINTSTRUCT_Type is
      record
         HDC         : GWindows.Types.Handle;
         fErase      : Interfaces.C.long;
         rcPaint     : GWindows.Types.Rectangle_Type;
         fRestore    : Interfaces.C.long;
         fIncUpdate  : Interfaces.C.long;
         rgbReserved : PS_RESERVE_TYPE;
      end record;

   procedure BeginPaint (HWND    : GWindows.Types.Handle;
                         lpPaint : out PAINTSTRUCT_Type);
   pragma Import (StdCall, BeginPaint, "BeginPaint");

   procedure EndPaint (HWND    : GWindows.Types.Handle;
                       lpPaint : in PAINTSTRUCT_Type);
   pragma Import (StdCall, EndPaint, "EndPaint");

   SW_MAXIMIZE        : constant := 3;
   SW_MINIMIZE        : constant := 6;
   SW_RESTORE         : constant := 9;

   procedure ShowWindow (hwnd     : GWindows.Types.Handle;
                         nCmdShow : Interfaces.C.long);
   pragma Import (StdCall, ShowWindow, "ShowWindow");

   WS_EX_CONTEXTHELP   : constant := 1024;
   WS_EX_TOOLWINDOW    : constant := 128;
   WS_EX_CONTROLPARENT : constant := 65536;
   WS_EX_MDICHILD      : constant := 64;
   WS_OVERLAPPEDWINDOW : constant := 13565952;
   WS_CLIPCHILDREN     : constant := 33554432;
   WS_CHILDWINDOW      : constant := 1073741824;
--   WS_GROUP            : constant := 131072;
--   WS_TABSTOP          : constant := 65536;
--   WS_EX_CLIENTEDGE    : constant := 512;
--   WS_BORDER           : constant := 8388608;
   WS_VSCROLL          : constant := 2097152;
   WS_HSCROLL          : constant := 1048576;
   WS_VISIBLE          : constant := 268435456;
--   WS_CHILD            : constant := 1073741824;
--   WS_POPUP            : constant := 16#80000000#;
--   WS_CAPTION          : constant := 12582912;
   WS_SYSMENU          : constant := 524288;
   WS_DLGFRAME         : constant := 4194304;
--     WS_MAXIMIZE         : constant := 16#1000000#;
--     WS_MINIMIZE         : constant := 16#20000000#;

   function CreateWindowEx
     (dwExStyle    : Interfaces.C.unsigned;
      lpClassName  : GString_C :=
        GWindows.Internal.Window_Class_Name;
      lpWindowName : GString_C;
      dwStyle      : Interfaces.C.unsigned;
      x            : Integer;
      y            : Integer;
      nWidth       : Integer;
      nHeight      : Integer;
      hwndParent   : GWindows.Types.Handle   := GWindows.Types.Null_Handle;
      hMenu        : GWindows.Types.Handle   := GWindows.Types.Null_Handle;
      hInst        : GWindows.Types.Handle   :=
        GWindows.Internal.Current_hInstance;
      lpParam      : Interfaces.C.long       := 0)
     return GWindows.Types.Handle;
   pragma Import (StdCall, CreateWindowEx,
                    "CreateWindowEx" & Character_Mode_Identifier);

   type CLIENTCREATESTRUCT is
      record
         hWindowMenu  : Interfaces.C.long := 0;
         idFirstChild : Interfaces.C.int  := 5000;
      end record;

   Client_Create_Struct : CLIENTCREATESTRUCT;

   function CreateWindowEx_MDI_CLIENT
     (dwExStyle    : Interfaces.C.unsigned   := 0;
      lpClassName  : GString_C :=
        GWindows.GStrings.To_GString_C ("MDICLIENT");
      lpWindowName : GString_C :=
        GWindows.GStrings.To_GString_C ("");
      dwStyle      : Interfaces.C.unsigned   :=
          WS_CHILDWINDOW or
          WS_CLIPCHILDREN or
          WS_VISIBLE or
          WS_HSCROLL or
          WS_VSCROLL;
      x            : Integer                 := 0;
      y            : Integer                 := 0;
      nWidth       : Integer                 := 0;
      nHeight      : Integer                 := 0;
      hwndParent   : GWindows.Types.Handle;
      hMenu        : GWindows.Types.Handle   := GWindows.Types.Null_Handle;
      hInst        : GWindows.Types.Handle   :=
        GWindows.Internal.Current_hInstance;
      lpParam      : CLIENTCREATESTRUCT      := Client_Create_Struct)
     return GWindows.Types.Handle;
   pragma Import (StdCall, CreateWindowEx_MDI_CLIENT,
                    "CreateWindowEx" & Character_Mode_Identifier);

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out Window_Type;
      Title      : in     GString     := "";
      Left       : in     Integer     := GWindows.Constants.Use_Default;
      Top        : in     Integer     := GWindows.Constants.Use_Default;
      Width      : in     Integer     := GWindows.Constants.Use_Default;
      Height     : in     Integer     := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean     := False;
      CClass     : in     GString     := "")
   is
      C_Title  : constant GString_C := GWindows.GStrings.To_GString_C (Title);
      Win_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned := WS_OVERLAPPEDWINDOW;
      ExStyle  : Interfaces.C.unsigned := 0;
      CClass_C : constant GString_C := GWindows.GStrings.To_GString_C (CClass);
   begin
      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      if CClass = "" then
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height);
      else
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     lpClassName  => CClass_C,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height);
      end if;

      GWindows.Base.Link (Window, Win_HWND, Is_Dynamic);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create;

   ------------------
   -- Create_Child --
   ------------------

   procedure Create_Child
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean                              :=
        False;
      CClass     : in     GString                              := "")
   is
      C_Title  : constant GString_C := GWindows.GStrings.To_GString_C (Title);
      PHWND    : constant GWindows.Types.Handle :=
        GWindows.Base.Handle (Parent);
      Win_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned := WS_OVERLAPPEDWINDOW;
      ExStyle  : Interfaces.C.unsigned := 0;
      CClass_C : constant GString_C := GWindows.GStrings.To_GString_C (CClass);
   begin
      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      if CClass /= "" then
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     lpClassName  => CClass_C,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height,
                                     hwndParent   => PHWND);
      else
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height,
                                     hwndParent   => PHWND);
      end if;

      GWindows.Base.Link (Window, Win_HWND, Is_Dynamic);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_Child;

   ----------------------
   -- Create_As_Dialog --
   ----------------------

   procedure Create_As_Dialog
     (Window      : in out Window_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString                              := "";
      Left        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top         : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Help_Button : in     Boolean                              := False;
      Is_Dynamic  : in     Boolean                              := False)
   is
      C_Title  : constant GString_C := GWindows.GStrings.To_GString_C (Title);
      PHWND    : constant GWindows.Types.Handle :=
        GWindows.Base.Handle (Parent);
      Win_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned := WS_DLGFRAME or WS_SYSMENU;
      ExStyle  : Interfaces.C.unsigned := 0;
   begin
      if Help_Button then
         ExStyle := WS_EX_CONTEXTHELP;
      end if;

      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                  dwExStyle    => ExStyle,
                                  dwStyle      => Style,
                                  x            => Left,
                                  y            => Top,
                                  nWidth       => Width,
                                  nHeight      => Height,
                                  hwndParent   => PHWND);

      GWindows.Base.Link (Window, Win_HWND, Is_Dynamic);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_As_Dialog;

   ----------------------
   -- Create_As_Dialog --
   ----------------------

   procedure Create_As_Dialog
     (Window      : in out Window_Type;
      Title       : in     GString     := "";
      Left        : in     Integer     :=
        GWindows.Constants.Use_Default;
      Top         : in     Integer     :=
        GWindows.Constants.Use_Default;
      Width       : in     Integer     :=
        GWindows.Constants.Use_Default;
      Height      : in     Integer     :=
        GWindows.Constants.Use_Default;
      Help_Button : in     Boolean     := False;
      Is_Dynamic  : in     Boolean     := False)
   is
      No_Parent : Window_Type;
   begin
      Create_As_Dialog (Window, No_Parent, Title, Left, Top, Width, Height,
                        Help_Button, Is_Dynamic);
   end Create_As_Dialog;

   ---------------------------
   -- Create_As_Tool_Window --
   ---------------------------

   procedure Create_As_Tool_Window
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean                              := False)
   is
      C_Title  : constant GString_C := GWindows.GStrings.To_GString_C (Title);
      PHWND    : constant GWindows.Types.Handle :=
        GWindows.Base.Handle (Parent);
      Win_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned := WS_OVERLAPPEDWINDOW;
      ExStyle  : Interfaces.C.unsigned := WS_EX_TOOLWINDOW;
   begin
      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                  dwExStyle    => ExStyle,
                                  dwStyle      => Style,
                                  x            => Left,
                                  y            => Top,
                                  nWidth       => Width,
                                  nHeight      => Height,
                                  hwndParent   => PHWND);

      GWindows.Base.Link (Window, Win_HWND, Is_Dynamic);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_As_Tool_Window;

   -----------------------
   -- Create_As_Control --
   -----------------------

   procedure Create_As_Control
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      All_Keys   : in     Boolean                              := False;
      Container  : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Styles     : in     Interfaces.C.unsigned                := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      use GWindows.Drawing_Objects;

      C_Title     : constant GString_C :=
        GWindows.GStrings.To_GString_C (Title);
      PHWND       : constant GWindows.Types.Handle :=
        GWindows.Base.Handle (Parent);
      Win_HWND    : GWindows.Types.Handle;
      Parent_Font : Font_Type;
      Style       : Interfaces.C.unsigned := WS_CHILDWINDOW or Styles;
      ExStyle     : Interfaces.C.unsigned := WS_EX_CONTROLPARENT;
   begin
      Window.All_Keys := All_Keys;

      if not Container then
         ExStyle := 0;
      end if;

      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                  dwExStyle    => ExStyle,
                                  dwStyle      => Style,
                                  x            => Left,
                                  y            => Top,
                                  nWidth       => Width,
                                  nHeight      => Height,
                                  hwndParent   => PHWND,
                                  hMenu        =>
                                    GWindows.Types.To_Handle (ID));

      GWindows.Base.Link
        (Window, Win_HWND, Is_Dynamic, GWindows.Base.Control_Link);

      GWindows.Base.Get_Font (Parent, Parent_Font);
      Set_Font (Window, Parent_Font);

      Tab_Stop (Window);
      Group (Window);

      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);

      if Show then
         GWindows.Windows.Visible (Window);
      end if;
   end Create_As_Control;

   --------------------
   -- Create_MDI_Top --
   --------------------

   procedure Create_MDI_Top
     (Window     : in out Window_Type;
      Title      : in     GString     := "";
      Left       : in     Integer     := GWindows.Constants.Use_Default;
      Top        : in     Integer     := GWindows.Constants.Use_Default;
      Width      : in     Integer     := GWindows.Constants.Use_Default;
      Height     : in     Integer     := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean     := False;
      CClass     : in     GString     := "")
   is
      C_Title     : constant GString_C :=
        GWindows.GStrings.To_GString_C (Title);
      Win_HWND    : GWindows.Types.Handle;
      Client_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned :=
        WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN;
      ExStyle  : Interfaces.C.unsigned := 0;
      CClass_C : constant GString_C := GWindows.GStrings.To_GString_C (CClass);
   begin
      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      if CClass = "" then
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height);
      else
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     lpClassName  => CClass_C,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height);
      end if;

      GWindows.Base.Link (Window, Win_HWND, Is_Dynamic);

      Client_HWND := CreateWindowEx_MDI_CLIENT
        (hwndParent => Win_HWND);

      MDI_Client_Window (Window, Client_HWND);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_MDI_Top;

   ----------------------
   -- Create_MDI_Child --
   ----------------------

   procedure Create_MDI_Child
     (Window     : in out Window_Type;
      Parent     : in     Window_Type'Class;
      Title      : in     GString           := "";
      Left       : in     Integer           := GWindows.Constants.Use_Default;
      Top        : in     Integer           := GWindows.Constants.Use_Default;
      Width      : in     Integer           := GWindows.Constants.Use_Default;
      Height     : in     Integer           := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean           := False;
      CClass     : in     GString           := "")
   is
      C_Title  : constant GString_C := GWindows.GStrings.To_GString_C (Title);
      Win_HWND : GWindows.Types.Handle;
      Style    : Interfaces.C.unsigned := 0;
      ExStyle  : Interfaces.C.unsigned := WS_EX_MDICHILD;
      CClass_C : constant GString_C := GWindows.GStrings.To_GString_C (CClass);
   begin
      On_Pre_Create (Window_Type'Class (Window), Style, ExStyle);

      if CClass = "" then
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height,
                                     hwndParent   =>
                                    GWindows.Base.Handle
                                     (MDI_Client_Window (Parent).all));
      else
         Win_HWND := CreateWindowEx (lpWindowName => C_Title,
                                     lpClassName  => CClass_C,
                                     dwExStyle    => ExStyle,
                                     dwStyle      => Style,
                                     x            => Left,
                                     y            => Top,
                                     nWidth       => Width,
                                     nHeight      => Height,
                                     hwndParent   =>
                                    GWindows.Base.Handle
                                     (MDI_Client_Window (Parent).all));
      end if;

      GWindows.Base.Link
        (Window, Win_HWND, Is_Dynamic, GWindows.Base.MDI_Child_Link);
      Hide (Window);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_MDI_Child;

   ----------------
   -- Large_Icon --
   ----------------

   procedure Large_Icon (Window  : in Window_Type;
                         Name    : in GString)
   is
      C_Name : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := WM_SETICON;
         wParam : GWindows.Types.Wparam := ICON_BIG;
         lParam : GWindows.Types.Handle := LoadImage (lpszName => C_Name));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Large_Icon;

   ----------------
   -- Large_Icon --
   ----------------

   procedure Large_Icon (Window  : in Window_Type;
                         Icon    : in Drawing_Objects.Icon_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int     := WM_SETICON;
         wParam : GWindows.Types.Wparam := ICON_BIG;
         lParam : GWindows.Types.Handle :=
            GWindows.Drawing_Objects.Handle (Icon));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Large_Icon;

   ----------------
   -- Small_Icon --
   ----------------

   procedure Small_Icon (Window  : in Window_Type;
                         Name    : in GString)
   is
      C_Name : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := WM_SETICON;
         wParam : GWindows.Types.Wparam := ICON_SMALL;
         lParam : GWindows.Types.Handle := LoadImage (lpszName => C_Name));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Small_Icon;

   ----------------
   -- Small_Icon --
   ----------------

   procedure Small_Icon (Window  : in Window_Type;
                         Icon    : in Drawing_Objects.Icon_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := WM_SETICON;
         wParam : GWindows.Types.Wparam := ICON_SMALL;
         lParam : GWindows.Types.Handle :=
            GWindows.Drawing_Objects.Handle (Icon));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Small_Icon;

   ----------
   -- Menu --
   ----------

   procedure Menu (Window      : in Window_Type;
                   Name        : in GString;
                   Destroy_Old : in Boolean           := True)
   is
   begin
      Menu (Window, GWindows.Menus.Load_Menu (Name), Destroy_Old);
   end Menu;

   procedure Menu (Window      : in Window_Type;
                   Menu        : in GWindows.Menus.Menu_Type;
                   Destroy_Old : in Boolean                  := True)
   is
      use GWindows.Menus;

      procedure SetMenu
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         hmenu  : GWindows.Menus.Menu_Type := Menu);
      pragma Import (StdCall, SetMenu, "SetMenu");

      Old_Menu : Menu_Type := GWindows.Windows.Menu (Window);
   begin
      SetMenu;

      if (Old_Menu /= Null_Menu) and Destroy_Old then
         Destroy_Menu (Old_Menu);
      end if;
   end Menu;

   function Menu (Window : in Window_Type)
                 return GWindows.Menus.Menu_Type
   is
      function GetMenu
        (hwnd : GWindows.Types.Handle := Handle (Window))
        return GWindows.Menus.Menu_Type;
      pragma Import (StdCall, GetMenu, "GetMenu");
   begin
      return GetMenu;
   end Menu;

   --------------
   -- MDI_Menu --
   --------------

   procedure MDI_Menu (Window      : in out Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive)
   is
      pragma Warnings (Off, Window);
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle        :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int         := WM_MDISETMENU;
         wParam : GWindows.Menus.Menu_Type := Menu;
         lParam : GWindows.Menus.Menu_Type :=
         GWindows.Menus.Get_Sub_Menu (Menu, Window_Menu));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
      Menu_Refresh (Window);
   end MDI_Menu;

   ------------------
   -- Menu_Refresh --
   ------------------

   procedure Menu_Refresh (Window : in Window_Type) is
      procedure DrawMenuBar
        (hwnd   : GWindows.Types.Handle := Handle (Window));
      pragma Import (StdCall, DrawMenuBar, "DrawMenuBar");
   begin
      DrawMenuBar;
   end Menu_Refresh;

   --------------------------
   -- Display_Context_Menu --
   --------------------------

   procedure Display_Context_Menu (Window   : in Window_Type;
                                   Name     : in GString;
                                   Sub_Menu : in Natural;
                                   X        : in Integer;
                                   Y        : in Integer)
   is
      use GWindows.Menus;
      Menu : Menu_Type := Load_Menu (Name);
   begin
      Display_Context_Menu  (Window, Menu, Sub_Menu, X, Y);
      Destroy_Menu (Menu);
   end Display_Context_Menu;

   procedure Display_Context_Menu (Window   : in Window_Type;
                                   Menu     :    GWindows.Menus.Menu_Type;
                                   Sub_Menu : in Natural;
                                   X        : in Integer;
                                   Y        : in Integer)
   is
      use GWindows.Menus;

      Screen_Points : GWindows.Types.Point_Type := (X, Y);
   begin
      if X < 0 then
         Screen_Points.X := 0;
      end if;

      if Y < 0 then
         Screen_Points.Y := 0;
      end if;

      if Sub_Menu > 0 then
         TrackPopupMenu
           (Get_Sub_Menu (Menu, Sub_Menu),
            TPM_LEFTALIGN or TPM_RIGHTBUTTON,
            Screen_Points.X, Screen_Points.Y, 0, Handle (Window));
      else
         TrackPopupMenu
           (Menu,
            TPM_LEFTALIGN or TPM_RIGHTBUTTON,
            Screen_Points.X, Screen_Points.Y, 0, Handle (Window));
      end if;
   end Display_Context_Menu;

   ------------------
   -- Scroll_Range --
   ------------------

   procedure Scroll_Range
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Minimum : in Integer;
      Maximum : in Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Minimum;
      Info.nMax := Maximum;
      if Bar = Horizontal then
         SetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         SetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;
   end Scroll_Range;

   --------------------
   -- Scroll_Maximum --
   --------------------

   procedure Scroll_Maximum
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Maximum : in Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Scroll_Minimum (Window, Bar);
      Info.nMax := Maximum;
      if Bar = Horizontal then
         SetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         SetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;
   end Scroll_Maximum;

   function Scroll_Maximum
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;

      if Bar = Horizontal then
         GetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         GetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;

      return Info.nMax;
   end Scroll_Maximum;

   --------------------
   -- Scroll_Minimum --
   --------------------

   procedure Scroll_Minimum
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Minimum : in Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Minimum;
      Info.nMax := Scroll_Maximum (Window, Bar);
      if Bar = Horizontal then
         SetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         SetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;
   end Scroll_Minimum;

   function Scroll_Minimum
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;

      if Bar = Horizontal then
         GetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         GetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;

      return Info.nMin;
   end Scroll_Minimum;

   ---------------------
   -- Scroll_Position --
   ---------------------

   procedure Scroll_Position
     (Window   : in Window_Type;
      Bar      : in Scroll_Bar_Type;
      Position : in Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_POS;
      Info.nPos := Position;

      if Bar = Horizontal then
         SetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         SetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;
   end Scroll_Position;

   function Scroll_Position
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_POS;

      if Bar = Horizontal then
         GetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         GetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;

      return Info.nPos;
   end Scroll_Position;

   ----------------------
   -- Scroll_Page_Size --
   ----------------------

   procedure Scroll_Page_Size
     (Window   : in Window_Type;
      Bar      : in Scroll_Bar_Type;
      Size     : in Natural)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_PAGE;
      Info.nPage := Size;

      if Bar = Horizontal then
         SetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         SetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;
   end Scroll_Page_Size;

   function Scroll_Page_Size
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Natural
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_PAGE;

      if Bar = Horizontal then
         GetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         GetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;

      return Info.nPage;
   end Scroll_Page_Size;

   --------------------------
   -- Scroll_Drag_Position --
   --------------------------

   function Scroll_Drag_Position
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_TRACKPOS;

      if Bar = Horizontal then
         GetScrollInfo (Handle (Window), SB_HORZ, Info);
      else
         GetScrollInfo (Handle (Window), SB_VERT, Info);
      end if;

      return Info.nTrackPos;
   end Scroll_Drag_Position;

   -----------------------
   -- MDI_Active_Window --
   -----------------------

   procedure MDI_Active_Window
     (Window : in Window_Type;
      Child  : in GWindows.Base.Base_Window_Type'Class)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int  := WM_MDIACTIVATE;
         wParam : GWindows.Types.Handle := GWindows.Base.Handle (Child);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MDI_Active_Window;

   function MDI_Active_Window (Window : in Window_Type)
                       return GWindows.Base.Pointer_To_Base_Window_Class
   is
      use type GWindows.Base.Base_Window_Access;

      function SendMessage
        (hwnd   : GWindows.Types.Handle;
         uMsg   : Interfaces.C.int  := WM_MDIGETACTIVE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Handle;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      Client_Window : constant GWindows.Base.Base_Window_Access :=
        MDI_Client_Window (Window);

   begin
      if Client_Window = null then
         return null;
      else
         return GWindows.Base.Window_From_Handle
           (SendMessage (GWindows.Base.Handle (Client_Window.all)));
      end if;
   end MDI_Active_Window;

   -------------------------
   -- MDI_Tile_Horizontal --
   -------------------------

   procedure MDI_Tile_Horizontal (Window : in Window_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int      := WM_MDITILE;
         wParam : GWindows.Types.Wparam := MDITILE_HORIZONTAL;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MDI_Tile_Horizontal;

   -----------------------
   -- MDI_Tile_Vertical --
   -----------------------

   procedure MDI_Tile_Vertical (Window : in Window_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int  := WM_MDITILE;
         wParam : GWindows.Types.Wparam := MDITILE_VERTICAL;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MDI_Tile_Vertical;

   -----------------
   -- MDI_Cascade --
   -----------------

   procedure MDI_Cascade (Window : in Window_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int  := WM_MDICASCADE;
         wParam : GWindows.Types.Wparam := MDITILE_SKIPDISABLED;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MDI_Cascade;

   -----------------------
   -- MDI_Arrange_Icons --
   -----------------------

   procedure MDI_Arrange_Icons (Window : in Window_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle :=
           GWindows.Base.Handle (MDI_Client_Window (Window).all);
         uMsg   : Interfaces.C.int  := WM_MDIICONARRANGE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MDI_Arrange_Icons;

   -------------------
   -- MDI_Close_All --
   -------------------

   procedure Close_Win (Window : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Enumeration call back to close MDI child windows

   procedure Close_Win (Window : GWindows.Base.Pointer_To_Base_Window_Class)
   is
   begin
      GWindows.Base.Close (Window.all);
   end Close_Win;

   procedure MDI_Close_All (Window : in out Window_Type) is
      pragma Warnings (Off, Window);
   begin
      GWindows.Base.Enumerate_Children (MDI_Client_Window (Window).all,
                                        Close_Win'Access);
   end MDI_Close_All;

   --------------------
   -- Default_Cursor --
   --------------------

   procedure Default_Cursor (Window : in out Window_Type;
                             Cursor : in     GWindows.Cursors.Cursor_Type)
   is
   begin
      Window.Default_Cursor := Cursor;
   end Default_Cursor;

   -----------------------------
   -- Default_Standard_Cursor --
   -----------------------------

   procedure Default_Standard_Cursor (Window : in out Window_Type;
                                      Cursor : in     Integer)
   is
   begin
      Default_Cursor (Window, GWindows.Cursors.Load_System_Cursor (Cursor));
   end Default_Standard_Cursor;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Window : in out Window_Type) is
      use type GWindows.Base.Pointer_To_Base_Window_Class;
   begin
      if Focus (Window) /= null then
         GWindows.Base.Focus (Focus (Window).all);
      end if;
      Fire_On_Focus (Window);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Window : in out Window_Type) is
   begin
      Fire_On_Lost_Focus (Window);
   end On_Lost_Focus;

   --------------
   -- On_Show --
   --------------

   procedure On_Show (Window : in out Window_Type) is
   begin
      Fire_On_Show (Window);
   end On_Show;

   --------------
   -- On_Hide --
   --------------

   procedure On_Hide (Window : in out Window_Type) is
   begin
      Fire_On_Hide (Window);
   end On_Hide;

   -------------
   -- On_Size --
   -------------

   procedure On_Size (Window : in out Window_Type;
                      Width  : in     Integer;
                      Height : in     Integer)
   is
   begin
      if Window.On_Size_Event /= null then
         Fire_On_Size (Window, Width, Height);
      end if;

      Dock_Children (Window);
   end On_Size;

   -------------
   -- On_Move --
   -------------

   procedure On_Move (Window : in out Window_Type;
                      Left   : in     Integer;
                      Top    : in     Integer)
   is
   begin
      pragma Warnings (Off);
      Fire_On_Move (Window, Left, Top);
      pragma Warnings (On);
   end On_Move;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move (Window : in out Window_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Mouse_Move (Window, X, Y, Keys);
   end On_Mouse_Move;

   -------------------
   -- On_Mouse_Wheel --
   -------------------

   procedure On_Mouse_Wheel (Window  : in out Window_Type;
                             X       : in     Integer;
                             Y       : in     Integer;
                             Keys    : in     Mouse_Key_States;
                             Z_Delta : in     Integer)
   is
   begin
      Fire_On_Mouse_Wheel (Window, X, Y, Keys, Z_Delta);
   end On_Mouse_Wheel;

   -------------------------------
   -- On_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Left_Mouse_Button_Down (Window, X, Y, Keys);
   end On_Left_Mouse_Button_Down;

   --------------------------------
   -- On_Right_Mouse_Button_Down --
   --------------------------------

   procedure On_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Right_Mouse_Button_Down (Window, X, Y, Keys);
   end On_Right_Mouse_Button_Down;

   ---------------------------------
   -- On_Middle_Mouse_Button_Down --
   ---------------------------------

   procedure On_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Middle_Mouse_Button_Down (Window, X, Y, Keys);
   end On_Middle_Mouse_Button_Down;

   -----------------------------
   -- On_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Left_Mouse_Button_Up (Window, X, Y, Keys);
   end On_Left_Mouse_Button_Up;

   ------------------------------
   -- On_Right_Mouse_Button_Up --
   ------------------------------

   procedure On_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Right_Mouse_Button_Up (Window, X, Y, Keys);
   end On_Right_Mouse_Button_Up;

   -------------------------------
   -- On_Middle_Mouse_Button_Up --
   -------------------------------

   procedure On_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Middle_Mouse_Button_Up (Window, X, Y, Keys);
   end On_Middle_Mouse_Button_Up;

   ---------------------------------------
   -- On_Left_Mouse_Button_Double_Click --
   ---------------------------------------

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Left_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_Left_Mouse_Button_Double_Click;

   ---------------------------------------
   -- On_Right_Mouse_Button_Double_Click --
   ---------------------------------------

   procedure On_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Right_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_Right_Mouse_Button_Double_Click;

   -----------------------------------------
   -- On_Middle_Mouse_Button_Double_Click --
   -----------------------------------------

   procedure On_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_Middle_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_Middle_Mouse_Button_Double_Click;

   -------------------------------
   -- On_NC_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_NC_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Left_Mouse_Button_Down (Window, X, Y, Keys);
   end On_NC_Left_Mouse_Button_Down;

   --------------------------------
   -- On_NC_Right_Mouse_Button_Down --
   --------------------------------

   procedure On_NC_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Right_Mouse_Button_Down (Window, X, Y, Keys);
   end On_NC_Right_Mouse_Button_Down;

   ---------------------------------
   -- On_NC_Middle_Mouse_Button_Down --
   ---------------------------------

   procedure On_NC_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Middle_Mouse_Button_Down (Window, X, Y, Keys);
   end On_NC_Middle_Mouse_Button_Down;

   -----------------------------
   -- On_NC_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_NC_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Left_Mouse_Button_Up (Window, X, Y, Keys);
   end On_NC_Left_Mouse_Button_Up;

   ------------------------------
   -- On_NC_Right_Mouse_Button_Up --
   ------------------------------

   procedure On_NC_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Right_Mouse_Button_Up (Window, X, Y, Keys);
   end On_NC_Right_Mouse_Button_Up;

   -------------------------------
   -- On_NC_Middle_Mouse_Button_Up --
   -------------------------------

   procedure On_NC_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Middle_Mouse_Button_Up (Window, X, Y, Keys);
   end On_NC_Middle_Mouse_Button_Up;

   ---------------------------------------
   -- On_NC_Left_Mouse_Button_Double_Click --
   ---------------------------------------

   procedure On_NC_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Left_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_NC_Left_Mouse_Button_Double_Click;

   ---------------------------------------
   -- On_NC_Right_Mouse_Button_Double_Click --
   ---------------------------------------

   procedure On_NC_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Right_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_NC_Right_Mouse_Button_Double_Click;

   -----------------------------------------
   -- On_NC_Middle_Mouse_Button_Double_Click --
   -----------------------------------------

   procedure On_NC_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      Fire_On_NC_Middle_Mouse_Button_Double_Click (Window, X, Y, Keys);
   end On_NC_Middle_Mouse_Button_Double_Click;

   -----------------------
   -- On_Character_Down --
   -----------------------

   procedure On_Character_Down
     (Window      : in out Window_Type;
      Special_Key : in     Special_Key_Type;
      Value       : in     GCharacter)
   is
   begin
      Fire_On_Character_Down (Window, Special_Key, Value);
   end On_Character_Down;

   ---------------------
   -- On_Character_Up --
   ---------------------

   procedure On_Character_Up
     (Window      : in out Window_Type;
      Special_Key : in     Special_Key_Type;
      Value       : in     GCharacter)
   is
   begin
      Fire_On_Character_Up (Window, Special_Key, Value);
   end On_Character_Up;

   -------------------
   -- On_Menu_Hover --
   -------------------

   procedure On_Menu_Hover (Window  : in out Window_Type;
                            Item    : in     Integer;
                            Kind    : in     Hover_Item_Type)
   is
      use type GWindows.Base.Pointer_To_Base_Window_Class;

      MDI_Window : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        MDI_Active_Window (Window);
   begin
      if MDI_Window /= null then
         On_Menu_Hover (Window_Type'Class (MDI_Window.all), Item, Kind);
      end if;
      Fire_On_Menu_Hover (Window, Item, Kind);
   end On_Menu_Hover;

   --------------------
   -- On_Menu_Select --
   --------------------

   procedure On_Menu_Select (Window : in out Window_Type;
                             Item   : in     Integer)
   is
      use type GWindows.Base.Pointer_To_Base_Window_Class;

      MDI_Window : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        MDI_Active_Window (Window);
   begin
      if MDI_Window /= null then
         On_Menu_Select (Window_Type'Class (MDI_Window.all), Item);
      end if;
      Fire_On_Menu_Select (Window, Item);
   end On_Menu_Select;

   ---------------------------
   -- On_Accelerator_Select --
   ---------------------------

   procedure On_Accelerator_Select (Window : in out Window_Type;
                                    Item   : in     Integer)
   is
   begin
      if Window.On_Accelerator_Select_Event /= null then
         Fire_On_Accelerator_Select (Window, Item);
      else
         On_Menu_Select (Window_Type'Class (Window), Item);
      end if;
   end On_Accelerator_Select;

   ---------------------
   -- On_MDI_Activate --
   ---------------------

   procedure On_MDI_Activate (Window : in out Window_Type) is
      use GWindows.Base;
      use GWindows.GStrings;
   begin
      if Window.On_MDI_Activate_Event /= null then
         Fire_On_MDI_Activate (Window);
      end if;
   end On_MDI_Activate;

   -----------------------
   -- On_MDI_Deactivate --
   -----------------------

   procedure On_MDI_Deactivate (Window : in out Window_Type) is
   begin
      Fire_On_MDI_Deactivate (Window);
   end On_MDI_Deactivate;

   ----------------------
   -- On_Change_Cursor --
   ----------------------

   System_Default_Cursor : constant GWindows.Cursors.Cursor_Type :=
     GWindows.Cursors.Load_System_Cursor (GWindows.Cursors.IDC_ARROW);

   procedure On_Change_Cursor (Window : in out Window_Type)
   is
      use type GWindows.Base.Action_Event;
      use type GWindows.Cursors.Cursor_Type;
   begin
      if Window.On_Change_Cursor_Event = null then
         if Window.Default_Cursor = 0 then
            GWindows.Cursors.Set_Cursor (System_Default_Cursor);
         else
            GWindows.Cursors.Set_Cursor (Window.Default_Cursor);
         end if;
      else
         Fire_On_Change_Cursor (Window);
      end if;
   end On_Change_Cursor;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint (Window : in out Window_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
   begin
      Fire_On_Paint (Window, Canvas, Area);
   end On_Paint;

   -------------------------
   -- On_Erase_Background --
   -------------------------

   procedure On_Erase_Background
     (Window : in out Window_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Drawing;
   begin
      if Window.On_Erase_Background_Event = null then
         if Window.Background_Color_Sys then
            Fill_Rectangle (Canvas,
                            Area,
                            GWindows.Colors.COLOR_BTNFACE);
         else
            declare
               use GWindows.Drawing_Objects;

               B : Brush_Type;
            begin
               Create_Solid_Brush (B, Window.Background_Color);
               Fill_Rectangle (Canvas, Area, B);
            end;
         end if;
      else
         Fire_On_Erase_Background (Window, Canvas, Area);
      end if;
   end On_Erase_Background;

   --------------
   -- On_Close --
   --------------

   procedure On_Close (Window    : in out Window_Type;
                       Can_Close :    out Boolean)
   is
   begin
      if Window.On_Close_Event = null then
         Can_Close := True;
      else
         Fire_On_Close (Window, Can_Close);
      end if;
   end On_Close;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Window       : in out Window_Type;
                         message      : Interfaces.C.unsigned;
                         wParam       : GWindows.Types.Wparam;
                         lParam       : GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult) is
      use GWindows.Base;

      WM_SETFOCUS                : constant := 7;
      WM_KILLFOCUS               : constant := 8;
      WM_SIZE                    : constant := 5;
      WM_MOVE                    : constant := 3;
      WM_SHOWWINDOW              : constant := 24;
      WM_NCLBUTTONDOWN           : constant := 161;
      WM_NCLBUTTONUP             : constant := 162;
      WM_NCLBUTTONDBLCLK         : constant := 163;
      WM_NCRBUTTONDOWN           : constant := 164;
      WM_NCRBUTTONUP             : constant := 165;
      WM_NCRBUTTONDBLCLK         : constant := 166;
      WM_NCMBUTTONDOWN           : constant := 167;
      WM_NCMBUTTONUP             : constant := 168;
      WM_NCMBUTTONDBLCLK         : constant := 169;
      WM_KEYDOWN                 : constant := 256;
      WM_KEYUP                   : constant := 257;
      WM_MOUSEMOVE               : constant := 512;
      WM_LBUTTONDOWN             : constant := 513;
      WM_LBUTTONUP               : constant := 514;
      WM_LBUTTONDBLCLK           : constant := 515;
      WM_RBUTTONDOWN             : constant := 516;
      WM_RBUTTONUP               : constant := 517;
      WM_RBUTTONDBLCLK           : constant := 518;
      WM_MBUTTONDOWN             : constant := 519;
      WM_MBUTTONUP               : constant := 520;
      WM_MBUTTONDBLCLK           : constant := 521;
      WM_MOUSEWHEEL              : constant := 522;
      WM_PAINT                   : constant := 15;
      WM_ERASEBKGND              : constant := 20;
      WM_CLOSE                   : constant := 16;
      WM_MENUSELECT              : constant := 287;
      WM_SETCURSOR               : constant := 32;
      WM_GETDLGCODE              : constant := 135;
      WM_SETFONT                 : constant := 48;
      WM_GETFONT                 : constant := 49;
      WM_DROPFILES               : constant := 563;

      --  DLGC_WANTARROWS            : constant := 16#0001#;
      --  DLGC_WANTTAB               : constant := 16#0002#;
      DLGC_WANTALLKEYS           : constant := 16#0004#;
      --  DLGC_WANTMESSAGE           : constant := 16#0004#;
      --  DLGC_HASSETSEL             : constant := 16#0008#;
      --  DLGC_DEFPUSHBUTTON         : constant := 16#0010#;
      --  DLGC_UNDEFPUSHBUTTON       : constant := 16#0020#;
      --  DLGC_RADIOBUTTON           : constant := 16#0040#;
      DLGC_WANTCHARS             : constant := 16#0080#;
      --  DLGC_STATIC                : constant := 16#0100#;
      --  DLGC_BUTTON                : constant := 16#2000#;

      function Get_Mouse_Key_States return Mouse_Key_States;
      --  Sets up the key states of a mouse event

      function Get_Mouse_Key_States return Mouse_Key_States is
         MK_LBUTTON                 : constant := 1;
         MK_RBUTTON                 : constant := 2;
         MK_SHIFT                   : constant := 4;
         MK_CONTROL                 : constant := 8;
         MK_MBUTTON                 : constant := 16;

         State : constant Interfaces.C.unsigned :=
           Interfaces.C.unsigned (GWindows.Utilities.Low_Word (wParam));
         Keys  : Mouse_Key_States := (others => False);
      begin
         if (State and MK_LBUTTON) = MK_LBUTTON then
            Keys (Left_Button) := True;
         end if;

         if (State and MK_MBUTTON) = MK_MBUTTON then
            Keys (Middle_Button) := True;
         end if;

         if (State and MK_RBUTTON) = MK_RBUTTON then
            Keys (Right_Button) := True;
         end if;

         if (State and MK_CONTROL) = MK_CONTROL then
            Keys (Control) := True;
         end if;

         if (State and MK_SHIFT) = MK_SHIFT then
            Keys (Shift) := True;
         end if;

         return Keys;
      end Get_Mouse_Key_States;

   begin
      case message is
         when WM_DROPFILES =>
            declare
               C_File_Name : GString_C (1 .. 4096) :=
                 (others => GString_C_Null);

               function Number_Of_Files
                 (HDROP : GWindows.Types.Handle := To_Handle (wParam);
                  Index : Interfaces.C.unsigned := 16#FFFFFFFF#;
                  Lpsz  : Integer               := 0;
                  Cch   : Integer               := 0)
               return Natural;
               pragma Import (StdCall, Number_Of_Files,
                              "DragQueryFile" & Character_Mode_Identifier);

               File_Count : constant Natural := Number_Of_Files;

               procedure DragQueryFile
                 (HDROP : GWindows.Types.Handle := To_Handle (wParam);
                  Index : Natural;
                  Lpsz  : access GChar_C :=
                    C_File_Name (C_File_Name'First)'Access;
                  Cch   : Integer           := C_File_Name'Length);
               pragma Import (StdCall, DragQueryFile,
                              "DragQueryFile" & Character_Mode_Identifier);

               procedure DragFinish
                 (HDROP : GWindows.Types.Handle := To_Handle (wParam));
               pragma Import (StdCall, DragFinish, "DragFinish");

            begin
               if File_Count > 0 then
                  declare
                     use GWindows.GStrings;

                     Names : Array_Of_File_Names (1 .. File_Count);
                  begin
                     for N in 1 .. File_Count loop
                        DragQueryFile (Index => N - 1); -- Zero based index
                        Names (N) :=
                          To_GString_Unbounded (
                            To_GString_From_C (C_File_Name)
                          );
                     end loop;
                     DragFinish;

                     On_File_Drop (Window_Type'Class (Window),
                                   Names);
                  end;
               else
                  DragFinish;
               end if;
            end;

            Return_Value := 0;

         when WM_GETFONT =>
            Return_Value := To_Lresult (Window.Font_Handle);

         when WM_SETFONT =>
            Window.Font_Handle := GWindows.Types.To_Handle (wParam);

         when WM_GETDLGCODE =>
            if Window.All_Keys then
               Return_Value := DLGC_WANTALLKEYS;
            else
               Return_Value := DLGC_WANTCHARS;
            end if;

         when WM_PAINT =>
            declare
               PS : PAINTSTRUCT_Type;
               CV : GWindows.Drawing.Canvas_Type;
               ST : GWindows.Drawing.Canvas_State_Type;
            begin
               BeginPaint (Handle (Window), PS);
               GWindows.Drawing.Handle (CV, PS.HDC);
               ST := GWindows.Drawing.Save_State (CV);
               On_Paint (Window_Type'Class (Window),
                         CV,
                         PS.rcPaint);
               GWindows.Drawing.Load_State (CV, ST);
               GWindows.Drawing.Handle (CV, Null_Handle);
               EndPaint (Handle (Window), PS);

               Return_Value := 0;
            end;

         when WM_ERASEBKGND =>
            declare
               CV : GWindows.Drawing.Canvas_Type;
            begin
               GWindows.Drawing.Handle (CV, To_Handle (wParam));

               On_Erase_Background (Window_Type'Class (Window),
                                    CV,
                                    (0, 0,
                                     Client_Area_Width (Window),
                                     Client_Area_Height (Window)));

               Return_Value := 0;
            end;

         when WM_SETCURSOR =>
            declare
               HTCLIENT : constant         := 1;
               Low      : constant Integer :=
                 GWindows.Utilities.Low_Word (lParam);
            begin
               if
                 Low = HTCLIENT
                 and
                 GWindows.Types.To_Handle (wParam) = Handle (Window)
               then
                  On_Change_Cursor (Window_Type'Class (Window));

                  Return_Value := 1;
               else
                  On_Message (GWindows.Base.Base_Window_Type (Window),
                              message,
                              wParam,
                              lParam,
                              Return_Value);
               end if;
            end;
         when WM_CLOSE =>
            declare
               Can_Close : Boolean;
            begin
               On_Close (Window_Type'Class (Window),
                         Can_Close);

               if Can_Close then
                  On_Message (GWindows.Base.Base_Window_Type (Window),
                              message,
                              wParam,
                              lParam,
                              Return_Value);
               end if;

               Return_Value := 0;
            end;

         when WM_SETFOCUS =>
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

            On_Focus (Window_Type'Class (Window));

         when WM_KILLFOCUS =>
            On_Lost_Focus (Window_Type'Class (Window));

         when WM_SHOWWINDOW =>
            if wParam = 0 then
               On_Hide (Window_Type'Class (Window));
            else
               On_Show (Window_Type'Class (Window));
            end if;
            Return_Value := 0;

         when WM_KEYDOWN =>
            declare
               Special : Special_Key_Type;
               Key     : GCharacter;
            begin
               Translate_Key (wParam, Special, Key);
               On_Character_Down (Window_Type'Class (Window),
                                  Special,
                                  Key);
               Return_Value := 0;
            end;

         when WM_KEYUP =>
            declare
               Special : Special_Key_Type;
               Key     : GCharacter;
            begin
               Translate_Key (wParam, Special, Key);
               On_Character_Up (Window_Type'Class (Window),
                                Special,
                                Key);
               Return_Value := 0;
            end;

         when WM_SIZE =>
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

            On_Size (Window_Type'Class (Window),
                     GWindows.Utilities.Low_Word (lParam),
                     GWindows.Utilities.High_Word (lParam));

            Return_Value := 0;

         when WM_MOVE =>
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

            On_Move (Window_Type'Class (Window),
                     GWindows.Utilities.Low_Word (lParam),
                     GWindows.Utilities.High_Word (lParam));

            Return_Value := 0;

         when WM_MOUSEMOVE =>
            On_Mouse_Move
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_MOUSEWHEEL =>
            On_Mouse_Wheel
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States,
               GWindows.Utilities.High_Word (wParam));

            Return_Value := 0;

         when WM_LBUTTONDOWN =>
            On_Left_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_RBUTTONDOWN =>
            On_Right_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_MBUTTONDOWN =>
            On_Middle_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_LBUTTONUP =>
            On_Left_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_RBUTTONUP =>
            On_Right_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            --  Pass on WM_RBUTTONUP for context menu use
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

         when WM_MBUTTONUP =>
            On_Middle_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_LBUTTONDBLCLK =>
            On_Left_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_RBUTTONDBLCLK =>
            On_Right_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_MBUTTONDBLCLK =>
            On_Middle_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCLBUTTONDOWN =>
            On_NC_Left_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            --  Pass on WM_NCLBUTTONDOWN for system menu use
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

         when WM_NCRBUTTONDOWN =>
            On_NC_Right_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCMBUTTONDOWN =>
            On_NC_Middle_Mouse_Button_Down
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCLBUTTONUP =>
            On_NC_Left_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCRBUTTONUP =>
            On_NC_Right_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCMBUTTONUP =>
            On_NC_Middle_Mouse_Button_Up
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCLBUTTONDBLCLK =>
            On_NC_Left_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            --  Pass on WM_NCLBUTTONDBLCLK for maximize use
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

         when WM_NCRBUTTONDBLCLK =>
            On_NC_Right_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_NCMBUTTONDBLCLK =>
            On_NC_Middle_Mouse_Button_Double_Click
              (Window_Type'Class (Window),
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam),
               Get_Mouse_Key_States);

            Return_Value := 0;

         when WM_MDIACTIVATE =>
            if To_Handle (lParam) = Handle (Window) then
               On_MDI_Activate (Window_Type'Class (Window));
            else
               On_MDI_Deactivate (Window_Type'Class (Window));
            end if;

            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);

         when WM_MENUSELECT =>
            declare
               use Interfaces.C;

               High    : constant unsigned :=
                 GWindows.Utilities.Unsigned_High_Word (wParam);
               Low     : constant unsigned :=
                 GWindows.Utilities.Unsigned_Low_Word (wParam);
               Item_Is : Hover_Item_Type := Menu_Item;

               MF_POPUP    : constant := 16;
               MF_SYSMENU  : constant := 8192;
               MF_CLOSED   : constant := 16#FFFF#;
            begin
               if (High and MF_POPUP) = MF_POPUP then
                  Item_Is := Menu;
               elsif (High and MF_SYSMENU) = MF_SYSMENU then
                  Item_Is := System_Menu;
               end if;

               if High = MF_CLOSED then
                  Item_Is := Closed;
               end if;

               On_Menu_Hover
                 (Window_Type'Class (Window),
                  Integer (Low),
                  Item_Is);

               Return_Value := 0;
            end;

         when others =>
            On_Message (GWindows.Base.Base_Window_Type (Window),
                        message,
                        wParam,
                        lParam,
                        Return_Value);
      end case;

   end On_Message;

   --------------------------
   -- On_Horizontal_Scroll --
   --------------------------

   procedure On_Horizontal_Scroll
     (Window  : in out Window_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use GWindows.Base;
   begin
      if Control /= null then
         On_Horizontal_Scroll
           (GWindows.Base.Base_Window_Type (Window), Request, Control);
      else
         case Request is
            when First =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Minimum (Window, Horizontal));
            when Last =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Maximum (Window, Horizontal));
            when Previous_Unit =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Position (Window, Horizontal) - 1);
            when Next_Unit =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Position (Window, Horizontal) + 1);
            when Previous_Page =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Position (Window, Horizontal) -
                                Scroll_Page_Size (Window, Horizontal));
            when Next_Page =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Position (Window, Horizontal) +
                                Scroll_Page_Size (Window, Horizontal));
            when Thumb_Set =>
               Scroll_Position (Window,
                                Horizontal,
                                Scroll_Drag_Position (Window, Horizontal));
            when others =>
               null;
         end case;
      end if;
   end On_Horizontal_Scroll;

   ------------------------
   -- On_Vertical_Scroll --
   ------------------------

   procedure On_Vertical_Scroll
     (Window  : in out Window_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use GWindows.Base;
   begin
      if Control /= null then
         On_Vertical_Scroll
           (GWindows.Base.Base_Window_Type (Window), Request, Control);
      else
         case Request is
            when First =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Minimum (Window, Vertical));
            when Last =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Maximum (Window, Vertical));
            when Previous_Unit =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Position (Window, Vertical) - 1);
            when Next_Unit =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Position (Window, Vertical) + 1);
            when Previous_Page =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Position (Window, Vertical) -
                                Scroll_Page_Size (Window, Vertical));
            when Next_Page =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Position (Window, Vertical) +
                                Scroll_Page_Size (Window, Vertical));
            when Thumb_Set =>
               Scroll_Position (Window,
                                Vertical,
                                Scroll_Drag_Position (Window, Vertical));
            when others =>
               null;
         end case;
      end if;
   end On_Vertical_Scroll;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command
     (Window  : in out Window_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use GWindows.Base;
   begin
      On_Command (GWindows.Base.Base_Window_Type (Window),
                  Code, ID, Control);

      if Control = null then
         if Code /= 1 then
            On_Menu_Select (Window_Type'Class (Window), ID);
         else
            On_Accelerator_Select (Window_Type'Class (Window), ID);
         end if;
      end if;
   end On_Command;

   ----------
   -- Zoom --
   ----------

   procedure Zoom (Window : in Window_Type;
                   State  : in Boolean     := True)
   is
   begin
      if State then
         ShowWindow (Handle (Window), SW_MAXIMIZE);
      else
         ShowWindow (Handle (Window), SW_RESTORE);
      end if;
   end Zoom;

   function Zoom (Window : in Window_Type) return Boolean is

      function IsZoomed
        (hwnd : GWindows.Types.Handle   := Handle (Window))
        return Interfaces.C.long;
      pragma Import (StdCall, IsZoomed, "IsZoomed");
   begin
      return IsZoomed /= 0;
   end Zoom;

   ------------
   -- Iconic --
   ------------

   procedure Iconic (Window : in out Window_Type;
                     State  : in     Boolean     := True)
   is
   begin
      if State then
         ShowWindow (Handle (Window), SW_MINIMIZE);
      else
         Visible (Window);
      end if;
   end Iconic;

   function Iconic (Window : in Window_Type) return Boolean is

      function IsIconic
        (hwnd : GWindows.Types.Handle   := Handle (Window))
        return Interfaces.C.long;
      pragma Import (StdCall, IsIconic, "IsIconic");
   begin
      return IsIconic /= 0;
   end Iconic;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler (Window  : in out Window_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Focus_Event /= null then
         Window.On_Focus_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Window  : in out Window_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Lost_Focus_Event /= null then
         Window.On_Lost_Focus_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Lost_Focus;

   ----------------------
   -- On_Size_Handler --
   ----------------------

   procedure On_Size_Handler (Window  : in out Window_Type;
                              Handler : in     Size_Event)
   is
   begin
      Window.On_Size_Event := Handler;
   end On_Size_Handler;

   -------------------
   -- Fire_On_Size --
   -------------------

   procedure Fire_On_Size
     (Window : in out Window_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
      use GWindows.Base;
   begin
      if Window.On_Size_Event /= null then
         Window.On_Size_Event
           (Base_Window_Type'Class (Window), Width, Height);
      end if;
   end Fire_On_Size;

   ----------------------
   -- On_Move_Handler --
   ----------------------

   procedure On_Move_Handler (Window  : in out Window_Type;
                              Handler : in     Location_Event)
   is
   begin
      Window.On_Move_Event := Handler;
   end On_Move_Handler;

   -------------------
   -- Fire_On_Move --
   -------------------

   procedure Fire_On_Move
     (Window : in out Window_Type;
      Left   : in     Integer;
      Top    : in     Integer)
   is
      use GWindows.Base;
   begin
      if Window.On_Move_Event /= null then
         Window.On_Move_Event (Base_Window_Type'Class (Window), Left, Top);
      end if;
   end Fire_On_Move;

   ---------------------
   -- On_Show_Handler --
   ---------------------

   procedure On_Show_Handler (Window  : in out Window_Type;
                              Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_Show_Event := Handler;
   end On_Show_Handler;

   ------------------
   -- Fire_On_Show --
   ------------------

   procedure Fire_On_Show (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Show_Event /= null then
         Window.On_Show_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Show;

   ---------------------
   -- On_Hide_Handler --
   ---------------------

   procedure On_Hide_Handler (Window  : in out Window_Type;
                              Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_Hide_Event := Handler;
   end On_Hide_Handler;

   ------------------
   -- Fire_On_Hide --
   ------------------

   procedure Fire_On_Hide (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Hide_Event /= null then
         Window.On_Hide_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Hide;

   ----------------------
   -- On_Close_Handler --
   ----------------------

   procedure On_Close_Handler (Window  : in out Window_Type;
                               Handler : in     Close_Event)
   is
   begin
      Window.On_Close_Event := Handler;
   end On_Close_Handler;

   -------------------
   -- Fire_On_Close --
   -------------------

   procedure Fire_On_Close
     (Window    : in out Window_Type;
      Can_Close :    out Boolean)
   is
      use GWindows.Base;
   begin
      if Window.On_Close_Event /= null then
         Window.On_Close_Event (Base_Window_Type'Class (Window), Can_Close);
      end if;
   end Fire_On_Close;

   -----------------------------
   -- On_MDI_Activate_Handler --
   -----------------------------

   procedure On_MDI_Activate_Handler
     (Window  : in out Window_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_MDI_Activate_Event := Handler;
   end On_MDI_Activate_Handler;

   --------------------------
   -- Fire_On_MDI_Activate --
   --------------------------

   procedure Fire_On_MDI_Activate (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_MDI_Activate_Event /= null then
         Window.On_MDI_Activate_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_MDI_Activate;

   -----------------------------
   -- On_MDI_Deactivate_Handler --
   -----------------------------

   procedure On_MDI_Deactivate_Handler
     (Window  : in out Window_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_MDI_Deactivate_Event := Handler;
   end On_MDI_Deactivate_Handler;

   --------------------------
   -- Fire_On_MDI_Deactivate --
   --------------------------

   procedure Fire_On_MDI_Deactivate (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_MDI_Deactivate_Event /= null then
         Window.On_MDI_Deactivate_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_MDI_Deactivate;

   ---------------------------
   -- On_Mouse_Move_Handler --
   ---------------------------

   procedure On_Mouse_Move_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Mouse_Move_Event := Handler;
   end On_Mouse_Move_Handler;

   ------------------------
   -- Fire_On_Mouse_Move --
   ------------------------

   procedure Fire_On_Mouse_Move
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Mouse_Move_Event /= null then
         Window.On_Mouse_Move_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Mouse_Move;

   ---------------------------
   -- On_Mouse_Wheel_Handler --
   ---------------------------

   procedure On_Mouse_Wheel_Handler
     (Window  : in out Window_Type;
      Handler : in     Wheel_Event)
   is
   begin
      Window.On_Mouse_Wheel_Event := Handler;
      Use_Mouse_Wheel (Window);
   end On_Mouse_Wheel_Handler;

   ------------------------
   -- Fire_On_Mouse_Wheel --
   ------------------------

   procedure Fire_On_Mouse_Wheel
     (Window  : in out Window_Type;
      X       : in     Integer;
      Y       : in     Integer;
      Keys    : in     Mouse_Key_States;
      Z_Delta : in     Integer)
   is
      use GWindows.Base;
   begin
      if Window.On_Mouse_Wheel_Event /= null then
         Window.On_Mouse_Wheel_Event
           (Base_Window_Type'Class (Window), X, Y, Keys, Z_Delta);
      end if;
   end Fire_On_Mouse_Wheel;

   ---------------------------------------
   -- On_Left_Mouse_Button_Down_Handler --
   ---------------------------------------

   procedure On_Left_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Left_Mouse_Button_Down_Event := Handler;
   end On_Left_Mouse_Button_Down_Handler;

   ------------------------------------
   -- Fire_On_Left_Mouse_Button_Down --
   ------------------------------------

   procedure Fire_On_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Left_Mouse_Button_Down_Event /= null then
         Window.On_Left_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Left_Mouse_Button_Down;

   -------------------------------------
   -- On_Left_Mouse_Button_Up_Handler --
   -------------------------------------

   procedure On_Left_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Left_Mouse_Button_Up_Event := Handler;
   end On_Left_Mouse_Button_Up_Handler;

   ----------------------------------
   -- Fire_On_Left_Mouse_Button_Up --
   ----------------------------------

   procedure Fire_On_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Left_Mouse_Button_Up_Event /= null then
         Window.On_Left_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Left_Mouse_Button_Up;

   -----------------------------------------------
   -- On_Left_Mouse_Button_Double_Click_Handler --
   -----------------------------------------------

   procedure On_Left_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Left_Mouse_Button_Double_Click_Event := Handler;
   end On_Left_Mouse_Button_Double_Click_Handler;

   --------------------------------------------
   -- Fire_On_Left_Mouse_Button_Double_Click --
   --------------------------------------------

   procedure Fire_On_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Left_Mouse_Button_Double_Click_Event /= null then
         Window.On_Left_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Left_Mouse_Button_Double_Click;

   ----------------------------------------
   -- On_Right_Mouse_Button_Down_Handler --
   ----------------------------------------

   procedure On_Right_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Right_Mouse_Button_Down_Event := Handler;
   end On_Right_Mouse_Button_Down_Handler;

   -------------------------------------
   -- Fire_On_Right_Mouse_Button_Down --
   -------------------------------------

   procedure Fire_On_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Right_Mouse_Button_Down_Event /= null then
         Window.On_Right_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Right_Mouse_Button_Down;

   --------------------------------------
   -- On_Right_Mouse_Button_Up_Handler --
   --------------------------------------

   procedure On_Right_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Right_Mouse_Button_Up_Event := Handler;
   end On_Right_Mouse_Button_Up_Handler;

   -----------------------------------
   -- Fire_On_Right_Mouse_Button_Up --
   -----------------------------------

   procedure Fire_On_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Right_Mouse_Button_Up_Event /= null then
         Window.On_Right_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Right_Mouse_Button_Up;

   ------------------------------------------------
   -- On_Right_Mouse_Button_Double_Click_Handler --
   ------------------------------------------------

   procedure On_Right_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Right_Mouse_Button_Double_Click_Event := Handler;
   end On_Right_Mouse_Button_Double_Click_Handler;

   ---------------------------------------------
   -- Fire_On_Right_Mouse_Button_Double_Click --
   ---------------------------------------------

   procedure Fire_On_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Right_Mouse_Button_Double_Click_Event /= null then
         Window.On_Right_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Right_Mouse_Button_Double_Click;

   -----------------------------------------
   -- On_Middle_Mouse_Button_Down_Handler --
   -----------------------------------------

   procedure On_Middle_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Middle_Mouse_Button_Down_Event := Handler;
   end On_Middle_Mouse_Button_Down_Handler;

   --------------------------------------
   -- Fire_On_Middle_Mouse_Button_Down --
   --------------------------------------

   procedure Fire_On_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Middle_Mouse_Button_Down_Event /= null then
         Window.On_Middle_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Middle_Mouse_Button_Down;

   ---------------------------------------
   -- On_Middle_Mouse_Button_Up_Handler --
   ---------------------------------------

   procedure On_Middle_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Middle_Mouse_Button_Up_Event := Handler;
   end On_Middle_Mouse_Button_Up_Handler;

   ------------------------------------
   -- Fire_On_Middle_Mouse_Button_Up --
   ------------------------------------

   procedure Fire_On_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Middle_Mouse_Button_Up_Event /= null then
         Window.On_Middle_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Middle_Mouse_Button_Up;

   -------------------------------------------------
   -- On_Middle_Mouse_Button_Double_Click_Handler --
   -------------------------------------------------

   procedure On_Middle_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_Middle_Mouse_Button_Double_Click_Event := Handler;
   end On_Middle_Mouse_Button_Double_Click_Handler;

   ----------------------------------------------
   -- Fire_On_Middle_Mouse_Button_Double_Click --
   ----------------------------------------------

   procedure Fire_On_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_Middle_Mouse_Button_Double_Click_Event /= null then
         Window.On_Middle_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_Middle_Mouse_Button_Double_Click;

   ---------------------------------------
   -- On_NC_Left_Mouse_Button_Down_Handler --
   ---------------------------------------

   procedure On_NC_Left_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Left_Mouse_Button_Down_Event := Handler;
   end On_NC_Left_Mouse_Button_Down_Handler;

   ------------------------------------
   -- Fire_On_NC_Left_Mouse_Button_Down --
   ------------------------------------

   procedure Fire_On_NC_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Left_Mouse_Button_Down_Event /= null then
         Window.On_NC_Left_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Left_Mouse_Button_Down;

   -------------------------------------
   -- On_Left_Mouse_Button_Up_Handler --
   -------------------------------------

   procedure On_NC_Left_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Left_Mouse_Button_Up_Event := Handler;
   end On_NC_Left_Mouse_Button_Up_Handler;

   ----------------------------------
   -- Fire_On_NC_Left_Mouse_Button_Up --
   ----------------------------------

   procedure Fire_On_NC_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Left_Mouse_Button_Up_Event /= null then
         Window.On_NC_Left_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Left_Mouse_Button_Up;

   -----------------------------------------------
   -- On_NC_Left_Mouse_Button_Double_Click_Handler --
   -----------------------------------------------

   procedure On_NC_Left_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Left_Mouse_Button_Double_Click_Event := Handler;
   end On_NC_Left_Mouse_Button_Double_Click_Handler;

   --------------------------------------------
   -- Fire_On_NC_Left_Mouse_Button_Double_Click --
   --------------------------------------------

   procedure Fire_On_NC_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Left_Mouse_Button_Double_Click_Event /= null then
         Window.On_NC_Left_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Left_Mouse_Button_Double_Click;

   ----------------------------------------
   -- On_NC_Right_Mouse_Button_Down_Handler --
   ----------------------------------------

   procedure On_NC_Right_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Right_Mouse_Button_Down_Event := Handler;
   end On_NC_Right_Mouse_Button_Down_Handler;

   -------------------------------------
   -- Fire_On_NC_Right_Mouse_Button_Down --
   -------------------------------------

   procedure Fire_On_NC_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Right_Mouse_Button_Down_Event /= null then
         Window.On_NC_Right_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Right_Mouse_Button_Down;

   --------------------------------------
   -- On_NC_Right_Mouse_Button_Up_Handler --
   --------------------------------------

   procedure On_NC_Right_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Right_Mouse_Button_Up_Event := Handler;
   end On_NC_Right_Mouse_Button_Up_Handler;

   -----------------------------------
   -- Fire_On_NC_Right_Mouse_Button_Up --
   -----------------------------------

   procedure Fire_On_NC_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Right_Mouse_Button_Up_Event /= null then
         Window.On_NC_Right_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Right_Mouse_Button_Up;

   ------------------------------------------------
   -- On_NC_Right_Mouse_Button_Double_Click_Handler --
   ------------------------------------------------

   procedure On_NC_Right_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Right_Mouse_Button_Double_Click_Event := Handler;
   end On_NC_Right_Mouse_Button_Double_Click_Handler;

   ---------------------------------------------
   -- Fire_On_NC_Right_Mouse_Button_Double_Click --
   ---------------------------------------------

   procedure Fire_On_NC_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Right_Mouse_Button_Double_Click_Event /= null then
         Window.On_NC_Right_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Right_Mouse_Button_Double_Click;

   -----------------------------------------
   -- On_NC_Middle_Mouse_Button_Down_Handler --
   -----------------------------------------

   procedure On_NC_Middle_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Middle_Mouse_Button_Down_Event := Handler;
   end On_NC_Middle_Mouse_Button_Down_Handler;

   --------------------------------------
   -- Fire_On_NC_Middle_Mouse_Button_Down --
   --------------------------------------

   procedure Fire_On_NC_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Middle_Mouse_Button_Down_Event /= null then
         Window.On_NC_Middle_Mouse_Button_Down_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Middle_Mouse_Button_Down;

   ---------------------------------------
   -- On_NC_Middle_Mouse_Button_Up_Handler --
   ---------------------------------------

   procedure On_NC_Middle_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Middle_Mouse_Button_Up_Event := Handler;
   end On_NC_Middle_Mouse_Button_Up_Handler;

   ------------------------------------
   -- Fire_On_NC_Middle_Mouse_Button_Up --
   ------------------------------------

   procedure Fire_On_NC_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Middle_Mouse_Button_Up_Event /= null then
         Window.On_NC_Middle_Mouse_Button_Up_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Middle_Mouse_Button_Up;

   -------------------------------------------------
   -- On_NC_Middle_Mouse_Button_Double_Click_Handler --
   -------------------------------------------------

   procedure On_NC_Middle_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event)
   is
   begin
      Window.On_NC_Middle_Mouse_Button_Double_Click_Event := Handler;
   end On_NC_Middle_Mouse_Button_Double_Click_Handler;

   ----------------------------------------------
   -- Fire_On_NC_Middle_Mouse_Button_Double_Click --
   ----------------------------------------------

   procedure Fire_On_NC_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      use GWindows.Base;
   begin
      if Window.On_NC_Middle_Mouse_Button_Double_Click_Event /= null then
         Window.On_NC_Middle_Mouse_Button_Double_Click_Event
           (Base_Window_Type'Class (Window), X, Y, Keys);
      end if;
   end Fire_On_NC_Middle_Mouse_Button_Double_Click;

   -----------------------------
   -- On_Character_Up_Handler --
   -----------------------------

   procedure On_Character_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Character_Event)
   is
   begin
      Window.On_Character_Up_Event := Handler;
   end On_Character_Up_Handler;

   --------------------------
   -- Fire_On_Character_Up --
   --------------------------

   procedure Fire_On_Character_Up
     (Window      : in out Window_Type;
      Special_Key : in     Special_Key_Type;
      Value       : in     GCharacter)
   is
      use GWindows.Base;
   begin
      if Window.On_Character_Up_Event /= null then
         Window.On_Character_Up_Event
           (Base_Window_Type'Class (Window),
            Special_Key,
            Value);
      end if;
   end Fire_On_Character_Up;

   -------------------------------
   -- On_Character_Down_Handler --
   -------------------------------

   procedure On_Character_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Character_Event)
   is
   begin
      Window.On_Character_Down_Event := Handler;
   end On_Character_Down_Handler;

   ----------------------------
   -- Fire_On_Character_Down --
   ----------------------------

   procedure Fire_On_Character_Down
     (Window      : in out Window_Type;
      Special_Key : in     Special_Key_Type;
      Value       : in     GCharacter)
   is
      use GWindows.Base;
   begin
      if Window.On_Character_Down_Event /= null then
         Window.On_Character_Down_Event
           (Base_Window_Type'Class (Window),
            Special_Key,
            Value);
      end if;
   end Fire_On_Character_Down;

   ---------------------------
   -- On_Menu_Hover_Handler --
   ---------------------------

   procedure On_Menu_Hover_Handler
     (Window  : in out Window_Type;
      Handler : in     Hover_Event)
   is
   begin
      Window.On_Menu_Hover_Event := Handler;
   end On_Menu_Hover_Handler;

   ------------------------
   -- Fire_On_Menu_Hover --
   ------------------------

   procedure Fire_On_Menu_Hover
     (Window : in out Window_Type;
      Item   : in     Integer;
      Kind   : in     Hover_Item_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Menu_Hover_Event /= null then
         Window.On_Menu_Hover_Event
           (Base_Window_Type'Class (Window), Item, Kind);
      end if;
   end Fire_On_Menu_Hover;

   ----------------------------
   -- On_Menu_Select_Handler --
   ----------------------------

   procedure On_Menu_Select_Handler (Window  : in out Window_Type;
                                     Handler : in     Select_Event)
   is
   begin
      Window.On_Menu_Select_Event := Handler;
   end On_Menu_Select_Handler;

   -------------------------
   -- Fire_On_Menu_Select --
   -------------------------

   procedure Fire_On_Menu_Select
     (Window : in out Window_Type;
      Item   : in     Integer)
   is
      use GWindows.Base;
   begin
      if Window.On_Menu_Select_Event /= null then
         Window.On_Menu_Select_Event (Base_Window_Type'Class (Window), Item);
      end if;
   end Fire_On_Menu_Select;

   -----------------------------------
   -- On_Accelerator_Select_Handler --
   -----------------------------------

   procedure On_Accelerator_Select_Handler (Window  : in out Window_Type;
                                            Handler : in     Select_Event)
   is
   begin
      Window.On_Accelerator_Select_Event := Handler;
   end On_Accelerator_Select_Handler;

   --------------------------------
   -- Fire_On_Accelerator_Select --
   --------------------------------

   procedure Fire_On_Accelerator_Select
     (Window : in out Window_Type;
      Item   : in     Integer)
   is
      use GWindows.Base;
   begin
      if Window.On_Accelerator_Select_Event /= null then
         Window.On_Accelerator_Select_Event
           (Base_Window_Type'Class (Window), Item);
      end if;
   end Fire_On_Accelerator_Select;

   ----------------------
   -- On_Paint_Handler --
   ----------------------

   procedure On_Paint_Handler (Window  : in out Window_Type;
                               Handler : in     Paint_Event)
   is
   begin
      Window.On_Paint_Event := Handler;
   end On_Paint_Handler;

   -------------------
   -- Fire_On_Paint --
   -------------------

   procedure Fire_On_Paint
     (Window : in out Window_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Paint_Event /= null then
         Window.On_Paint_Event
           (Base_Window_Type'Class (Window), Canvas, Area);
      end if;
   end Fire_On_Paint;

   ---------------------------------
   -- On_Erase_Background_Handler --
   ---------------------------------

   procedure On_Erase_Background_Handler (Window  : in out Window_Type;
                                          Handler : in     Paint_Event)
   is
   begin
      Window.On_Erase_Background_Event := Handler;
   end On_Erase_Background_Handler;

   ------------------------------
   -- Fire_On_Erase_Background --
   ------------------------------

   procedure Fire_On_Erase_Background
     (Window : in out Window_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Erase_Background_Event /= null then
         Window.On_Erase_Background_Event
           (Base_Window_Type'Class (Window), Canvas, Area);
      end if;
   end Fire_On_Erase_Background;

   ------------------------------
   -- On_Change_Cursor_Handler --
   ------------------------------

   procedure On_Change_Cursor_Handler
     (Window  : in out Window_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Window.On_Change_Cursor_Event := Handler;
   end On_Change_Cursor_Handler;

   ---------------------------
   -- Fire_On_Change_Cursor --
   ---------------------------

   procedure Fire_On_Change_Cursor (Window : in out Window_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Change_Cursor_Event /= null then
         Window.On_Change_Cursor_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Change_Cursor;

   procedure On_File_Drop (Window     : in out Window_Type;
                           File_Names : in     Array_Of_File_Names)
   is
   begin
      Fire_On_File_Drop (Window, File_Names);
   end On_File_Drop;

   procedure On_File_Drop_Handler (Window  : in out Window_Type;
                                   Handler : in File_Drop_Event)
   is
   begin
      Window.On_File_Drop_Event := Handler;
   end On_File_Drop_Handler;

   procedure Fire_On_File_Drop (Window     : in out Window_Type;
                                File_Names : in     Array_Of_File_Names)
   is
      use GWindows.Base;
   begin
      if Window.On_File_Drop_Event /= null then
         Window.On_File_Drop_Event (Base_Window_Type'Class (Window),
                                    File_Names);
      end if;
   end Fire_On_File_Drop;

   -------------------
   -- Create_Dialog --
   -------------------

   procedure Create_Dialog
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Name       : in     GString;
      Is_Dynamic : in     Boolean                := False)
   is
      C_Text  : GString_C := GWindows.GStrings.To_GString_C (Name);

      function CreateDialog
        (hInst : GWindows.Types.Handle := GWindows.Internal.Current_hInstance;
         Name  : access GChar_C        := C_Text (C_Text'First)'Access;
         hPrnt : GWindows.Types.Handle := GWindows.Base.Handle (Parent);
         PROC  : GWindows.Types.Lparam := 0;
         PARM  : GWindows.Types.Lparam := 0)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateDialog,
                       "CreateDialogParam" & Character_Mode_Identifier);

   begin
      Attach_Dialog (Window, CreateDialog, Is_Dynamic);
      On_Create (Window_Type'Class (Window));
      Dock_Children (Window);
   end Create_Dialog;

   procedure Create_Dialog
     (Window     : in out Window_Type;
      Name       : in     GString;
      Is_Dynamic : in     Boolean                := False)
   is
      Parent : Window_Type;
   begin
      Create_Dialog (Window, Parent, Name, Is_Dynamic);
   end Create_Dialog;

   -------------------------------
   -- Accept_File_Drag_And_Drop --
   -------------------------------

   procedure Accept_File_Drag_And_Drop (Window : Window_Type;
                                        State  : Boolean     := True) is
      procedure DragAcceptFiles
        (hwnd  : GWindows.Types.Handle := Handle (Window);
         fAcpt : Boolean := State);
      pragma Import (StdCall, DragAcceptFiles, "DragAcceptFiles");
   begin
      DragAcceptFiles;
   end Accept_File_Drag_And_Drop;

   -------------------
   -- Dock_Children --
   -------------------

   procedure Dock_Children (Window : in Window_Type)
   is
      use GWindows.Base;
      use GWindows.GStrings;

      Fill_Window    : Pointer_To_Base_Window_Class := null;

      Current_Bounds : GWindows.Types.Rectangle_Type :=
        (0, 0,
         Client_Area_Width (Window),
         Client_Area_Height (Window));

      procedure Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for docking child windows

      procedure Children (Child : Pointer_To_Base_Window_Class) is
         use GWindows.Packing_Boxes;
      begin
         if
           Handle (Parent (Child.all).all) = Handle (Window)
         then
            case Dock (Child.all) is
               when At_Top =>
                  Move (Child.all,
                        Current_Bounds.Left,
                        Current_Bounds.Top);
                  Width (Child.all,
                         Current_Bounds.Right - Current_Bounds.Left);
                  Current_Bounds.Top := Current_Bounds.Top +
                    Height (Child.all);
               when At_Bottom =>
                  Current_Bounds.Bottom := Current_Bounds.Bottom -
                    Height (Child.all);
                  Move (Child.all,
                        Current_Bounds.Left,
                        Current_Bounds.Bottom);
                  Width (Child.all,
                         Current_Bounds.Right - Current_Bounds.Left);
               when At_Left =>
                  Move (Child.all,
                        Current_Bounds.Left,
                        Current_Bounds.Top);
                  Height (Child.all,
                          Current_Bounds.Bottom - Current_Bounds.Top);
                  Current_Bounds.Left := Current_Bounds.Left +
                    Width (Child.all);
               when At_Right =>
                  Current_Bounds.Right := Current_Bounds.Right -
                    Width (Child.all);
                  Move (Child.all,
                        Current_Bounds.Right,
                        Current_Bounds.Top);
                  Height (Child.all,
                          Current_Bounds.Bottom - Current_Bounds.Top);
               when Fill =>
                  Fill_Window := Child;
               when None =>
                  null;
            end case;

            if Child.all in Packing_Box_Type'Class then
               Pack (Packing_Box_Type (Child.all));
            end if;
         end if;

      exception
         when others =>
            null; -- out of bounds windows won't be positioned
      end Children;

   begin
      if not (Iconic (Window)) then
         Enumerate_Children (Window, Children'Unrestricted_Access);

         if Fill_Window /= null then
            Move (Fill_Window.all, Current_Bounds.Left, Current_Bounds.Top);
            Size (Fill_Window.all,
                  Current_Bounds.Right - Current_Bounds.Left,
                  Current_Bounds.Bottom - Current_Bounds.Top);
         end if;
      end if;
   exception
      when others =>
         null; -- No room for fill window, don't position
   end Dock_Children;

   -------------------------------------------------------------------------
   --  Local Body
   -------------------------------------------------------------------------

   -------------------
   -- Translate_Key --
   -------------------

   procedure Translate_Key (wParam      : GWindows.Types.Wparam;
                            Special_Key : out Special_Key_Type;
                            Key         : out GCharacter) is

      type State_Array is array (0 .. 255) of aliased Character;
      Keyboard_State : State_Array := (others => ' ');

      procedure GetKeyboardState
        (lpbKeyState : access Character := Keyboard_State (0)'Access);
      pragma Import (StdCall, GetKeyboardState, "GetKeyboardState");

      Key_Value : aliased Interfaces.C.unsigned := 0;

      procedure ToAscii
        (uVirtKey    : in     Interfaces.C.int := Interfaces.C.int (wParam);
         uScanCode   : in     Interfaces.C.int      := 0;
         lpbKeyState : in     State_Array           := Keyboard_State;
         lpChar      : access Interfaces.C.unsigned := Key_Value'Access;
         uFlags      : in     Interfaces.C.int      := 0);
      pragma Import (StdCall, ToAscii, "ToAscii");

      procedure ToUnicode
        (uVirtKey    : in     Interfaces.C.int :=  Interfaces.C.int (wParam);
         uScanCode   : in     Interfaces.C.int      := 0;
         lpbKeyState : in     State_Array           := Keyboard_State;
         lpChar      : access Interfaces.C.unsigned := Key_Value'Access;
         ccBuff      : in     Integer               := 1;
         uFlags      : in     Interfaces.C.int      := 0);
      pragma Import (StdCall, ToUnicode, "ToUnicode");

      VK_SHIFT                   : constant := 16;
      VK_CONTROL                 : constant := 17;
      VK_ESCAPE                  : constant := 27;
      VK_PAUSE                   : constant := 19;
      VK_CAPITAL                 : constant := 20;
      VK_PRIOR                   : constant := 33;
      VK_NEXT                    : constant := 34;
      VK_END                     : constant := 35;
      VK_HOME                    : constant := 36;
      VK_LEFT                    : constant := 37;
      VK_UP                      : constant := 38;
      VK_RIGHT                   : constant := 39;
      VK_DOWN                    : constant := 40;
      VK_SNAPSHOT                : constant := 44;
      VK_INSERT                  : constant := 45;
      VK_DELETE                  : constant := 46;
      VK_F1                      : constant := 112;
      VK_F2                      : constant := 113;
      VK_F3                      : constant := 114;
      VK_F4                      : constant := 115;
      VK_F5                      : constant := 116;
      VK_F6                      : constant := 117;
      VK_F7                      : constant := 118;
      VK_F8                      : constant := 119;
      VK_F9                      : constant := 120;
      VK_F10                     : constant := 121;
      VK_F11                     : constant := 122;
      VK_F12                     : constant := 123;
      VK_NUMLOCK                 : constant := 144;
      VK_SCROLL                  : constant := 145;
   begin
      Key := GCharacter'Val (0);

      case wParam is
         when VK_CONTROL =>
            Special_Key := Control;
         when VK_SHIFT =>
            Special_Key := Shift;
         when VK_ESCAPE =>
            Special_Key := Escape;
         when VK_PAUSE =>
            Special_Key := Pause;
         when VK_CAPITAL =>
            Special_Key := Caps_Lock;
         when VK_PRIOR =>
            Special_Key := Page_Up;
         when VK_NEXT =>
            Special_Key := Page_Down;
         when VK_END =>
            Special_Key := End_Key;
         when VK_HOME =>
            Special_Key := Home_Key;
         when VK_LEFT =>
            Special_Key := Left_Key;
         when VK_UP =>
            Special_Key := Up_Key;
         when VK_RIGHT =>
            Special_Key := Right_Key;
         when VK_DOWN =>
            Special_Key := Down_Key;
         when VK_SNAPSHOT =>
            Special_Key := Print_Screen;
         when VK_INSERT =>
            Special_Key := Insert;
         when VK_DELETE =>
            Special_Key := Delete;
         when VK_F1 =>
            Special_Key := F1;
         when VK_F2 =>
            Special_Key := F2;
         when VK_F3 =>
            Special_Key := F3;
         when VK_F4 =>
            Special_Key := F4;
         when VK_F5 =>
            Special_Key := F5;
         when VK_F6 =>
            Special_Key := F6;
         when VK_F7 =>
            Special_Key := F7;
         when VK_F8 =>
            Special_Key := F8;
         when VK_F9 =>
            Special_Key := F9;
         when VK_F10 =>
            Special_Key := F10;
         when VK_F11 =>
            Special_Key := F11;
         when VK_F12 =>
            Special_Key := F12;
         when VK_NUMLOCK =>
            Special_Key := Number_Lock;
         when VK_SCROLL =>
            Special_Key := Scroll_Lock;
         when others =>
            Special_Key := None;
            GetKeyboardState;
            case Character_Mode is
               when Unicode =>
                  ToUnicode;
               when ANSI =>
                  ToAscii;
            end case;

            if Key_Value > 255 then
               Key_Value := Key_Value - (Key_Value and 16#FF00#);
            end if;

            Key := GCharacter'Val (Key_Value);
      end case;

   end Translate_Key;

   --------------
   -- Run_Mode --
   --------------

   procedure Run_Mode (Window : in out Window_Type;
                       Value  : in     GWindows.Base.Run_Mode_Type)
   is
      use GWindows.Base;

      procedure Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for setting run_mode
      --  of all children

      procedure Children (Child : Pointer_To_Base_Window_Class)
      is
      begin
         Run_Mode (Child.all, Value);
      end Children;

   begin
      GWindows.Base.Run_Mode (Base_Window_Type (Window), Value);

      if
        Value = Development_Create_Complete or
        Value = Development_Running
      then
         Dock_Children (Window);
      end if;

      Enumerate_Children (Window, Children'Unrestricted_Access);

   end Run_Mode;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color (Window : in out Window_Type;
                               Color  : in     GWindows.Colors.Color_Type)
   is
   begin
      Window.Background_Color_Sys := False;
      Window.Background_Color     := Color;
   end Background_Color;

   function Background_Color (Window : in Window_Type)
                             return GWindows.Colors.Color_Type
   is
   begin
      return Window.Background_Color;
   end Background_Color;

end GWindows.Windows;
