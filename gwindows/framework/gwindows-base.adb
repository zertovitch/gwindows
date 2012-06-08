------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                       G W I N D O W S . B A S E                          --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GWindows.GStrings;
with GWindows.Internal;
with GWindows.Utilities;

package body GWindows.Base is
   pragma Linker_Options ("-lcomctl32");

   use type Interfaces.C.long;

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   procedure Set_GWindow_Object (HWND   : GWindows.Types.Handle;
                                 Object : Pointer_To_Base_Window_Class);

   procedure Free (This : in Pointer_To_Base_Window_Class);
   --  Deallocate a dynamicly created window

   procedure Destroy_Win (Window : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Enumeration call back to destroy GWindows data in a child window
   --  This is needed for dynamic memory allocated windows since WM_DESTROY
   --  (where the GWindows object is destroyed) comes first to the parent
   --  and then the children. The result is that the GWindows object may
   --  contain static controls that will have already been finalized when
   --  the WM_DESTROY messages reaches the children resulting in a access
   --  violation

   procedure Destroy_Children
     (Window : GWindows.Base.Base_Window_Type'Class);
   --  Perform the enumeration of child windows for destruction

   procedure DestroyMenu
     (hmenu : GWindows.Types.Handle);
   pragma Import (StdCall, DestroyMenu, "DestroyMenu");

   function GetMenu
     (hwnd : GWindows.Types.Handle)
     return GWindows.Types.Handle;
   pragma Import (StdCall, GetMenu, "GetMenu");

   IDI_APPLICATION            : constant := 32512;

   function LoadIcon
     (hInstance  : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      lpIconName : Integer := IDI_APPLICATION)
     return GWindows.Types.Handle;
   pragma Import (StdCall, LoadIcon,
                    "LoadIcon" & Character_Mode_Identifier);

   Window_Class : WNDCLASS;

   procedure RegisterClass (Name : WNDCLASS);
   pragma Import (StdCall, RegisterClass,
                    "RegisterClass" & Character_Mode_Identifier);

   type DRAWITEMSTRUCT is
      record
         CtlType    : Interfaces.C.int;
         CtlID      : Interfaces.C.int;
         itemID     : Integer;
         itemAction : Interfaces.C.unsigned;
         itemState  : Interfaces.C.unsigned;
         hwndItem   : GWindows.Types.Handle;
         hDC        : GWindows.Types.Handle;
         rcItem     : GWindows.Types.Rectangle_Type;
         itemData   : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, DRAWITEMSTRUCT);

   type LPDRAWITEMSTRUCT is access all DRAWITEMSTRUCT;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   type InitStruct is
      record
         Size  : Integer;
         Flags : Integer;
      end record;

   procedure InitCommonControlsEx
     (Settings : InitStruct := (8, 16#FFF#));
   pragma Import (StdCall, InitCommonControlsEx, "InitCommonControlsEx");

   function DefWindowProc
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam)
     return GWindows.Types.Lresult;
   pragma Import (StdCall, DefWindowProc,
                    "DefWindowProc" & Character_Mode_Identifier);

   function DefMDIChildProc
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam)
     return GWindows.Types.Lresult;
   pragma Import (StdCall, DefMDIChildProc,
                    "DefMDIChildProc" & Character_Mode_Identifier);

   procedure RemoveProp
     (hwnd     : GWindows.Types.Handle;
      lpString : Interfaces.C.unsigned_short);
   pragma Import (StdCall, RemoveProp,
                    "RemoveProp" & Character_Mode_Identifier);

   GWL_WNDPROC : constant := -4;

   function Get_Window_Procedure
     (hwnd   : GWindows.Types.Handle;
      nIndex : Interfaces.C.int := GWL_WNDPROC)
     return Windproc_Access;
   pragma Import (StdCall, Get_Window_Procedure,
                  "GetWindowLongPtr" & Character_Mode_Identifier);

   procedure Set_Window_Procedure
     (hwnd     : GWindows.Types.Handle;
      nIndex   : Interfaces.C.int  := GWL_WNDPROC;
      New_Proc : Windproc_Access);
   pragma Import (StdCall, Set_Window_Procedure,
                  "SetWindowLongPtr" & Character_Mode_Identifier);

   function GlobalAddAtom
     (lpString : GString_C := GWindows.Internal.GWindows_Object_Property_Name)
     return Interfaces.C.unsigned_short;
   pragma Import (StdCall, GlobalAddAtom,
                    "GlobalAddAtom" & Character_Mode_Identifier);

   procedure GlobalDeleteAtom
     (nAtom : Interfaces.C.unsigned_short);
   pragma Import (StdCall, GlobalDeleteAtom, "GlobalDeleteAtom");

   GWL_STYLE   : constant := -16;
   GWL_EXSTYLE : constant := -20;

   procedure SetWindowLong
     (hwnd : GWindows.Types.Handle;
      nIndex  : Interfaces.C.int := GWL_STYLE;
      newLong : Interfaces.C.unsigned);
   pragma Import (StdCall, SetWindowLong,
                    "SetWindowLong" & Character_Mode_Identifier);

   function GetWindowLong
     (hwnd : GWindows.Types.Handle;
      nIndex : Interfaces.C.int := GWL_STYLE)
     return Interfaces.C.unsigned;
   pragma Import (StdCall, GetWindowLong,
                    "GetWindowLong" & Character_Mode_Identifier);

   function gwlptr (hwnd   : GWindows.Types.Handle;
                    nIndex : Interfaces.C.int) return GWindows.Types.Handle;
   pragma Export (StdCall, gwlptr,
                  "GetWindowLongPt" &
                  Character'Val (Character'Pos ('r') +
                                 (Standard'Address_Size / 32) - 1) &
                  Character_Mode_Identifier);
   function gwlptr (hwnd   : GWindows.Types.Handle;
                    nIndex : Interfaces.C.int) return GWindows.Types.Handle is
   begin
      return GWindows.Types.To_Handle
         (GWindows.Types.Wparam (GetWindowLong (hwnd, nIndex)));
   end gwlptr;

   procedure swlptr (hwnd   : GWindows.Types.Handle;
                     nIndex : Interfaces.C.int;
                     newLong : GWindows.Types.Wparam);
   pragma Export (StdCall, swlptr,
                  "SetWindowLongPt" &
                  Character'Val (Character'Pos ('r') +
                                 (Standard'Address_Size / 32) - 1) &
                  Character_Mode_Identifier);
   procedure swlptr (hwnd   : GWindows.Types.Handle;
                     nIndex : Interfaces.C.int;
                     newLong : GWindows.Types.Wparam) is
   begin
      SetWindowLong (hwnd, nIndex, Interfaces.C.unsigned (newLong));
   end swlptr;

   SWP_NOSIZE                 : constant := 1;
   SWP_NOMOVE                 : constant := 2;
   SWP_NOZORDER               : constant := 4;
   SWP_FRAMECHANGED           : constant := 32;
--   SWP_NOREDRAW               : constant := 8;
--   SWP_NOACTIVATE             : constant := 16;
--   SWP_SHOWWINDOW             : constant := 64;
--   SWP_HIDEWINDOW             : constant := 128;
--   SWP_NOCOPYBITS             : constant := 256;
--   SWP_NOOWNERZORDER          : constant := 512;
--   SWP_NOSENDCHANGING         : constant := 1024;
--   SWP_DRAWFRAME              : constant := 32;
--   SWP_NOREPOSITION           : constant := 512;
--   SWP_DEFERERASE             : constant := 8192;
--   SWP_ASYNCWINDOWPOS         : constant := 16384;
   HWND_TOP                   : constant := 0;
   HWND_BOTTOM                : constant := 1;
   HWND_TOPMOST               : constant := -1;
   HWND_NOTOPMOST             : constant := -2;

   procedure SetWindowPos
     (hwnd            : GWindows.Types.Handle;
      hwndInsertAfter : GWindows.Types.Handle := GWindows.Types.Null_Handle;
      x               : Integer := 0;
      y               : Integer := 0;
      cx              : Integer := 0;
      cy              : Integer := 0;
      fuFlags         : Interfaces.C.unsigned :=
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED);
   pragma Import (StdCall, SetWindowPos, "SetWindowPos");

   GW_HWNDFIRST               : constant := 0;
   GW_HWNDLAST                : constant := 1;
   GW_HWNDNEXT                : constant := 2;
   GW_HWNDPREV                : constant := 3;

   function GetWindow
     (hwnd        : GWindows.Types.Handle;
      fuDirection : Integer)
     return GWindows.Types.Handle;
   pragma Import (StdCall, GetWindow, "GetWindow");

   WS_CHILDWINDOW      : constant := 1073741824;
   WS_POPUP            : constant := 2147483648;
   WS_GROUP            : constant := 131072;
   WS_TABSTOP          : constant := 65536;
   WS_EX_CLIENTEDGE    : constant := 512;
   WS_BORDER           : constant := 8388608;
   WS_VSCROLL          : constant := 2097152;
   WS_HSCROLL          : constant := 1048576;

   procedure GetClientRect
     (hwnd            : in  GWindows.Types.Handle;
      Rect            : out GWindows.Types.Rectangle_Type);
   pragma Import (StdCall, GetClientRect, "GetClientRect");

   procedure GetWindowRect
     (hwnd            : in  GWindows.Types.Handle;
      Rect            : out GWindows.Types.Rectangle_Type);
   pragma Import (StdCall, GetWindowRect, "GetWindowRect");

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   Exception_Handler : Exception_Event;

   --------------------------
   -- On_Exception_Handler --
   --------------------------

   procedure On_Exception_Handler (Handler : Exception_Event) is
   begin
      Exception_Handler := Handler;
   end On_Exception_Handler;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Base_Window_Type) is
   begin
      --  * AnSp: When no procedures are attached, don't close it
      --  *       Is usefull to update a control the GWindows way
      --  *       without the need to maintain a variable
      if not Object.Is_Linked then                                  --  * AnSp
         Close (Object);
      end if;                                                       --  * AnSp
   end Finalize;

   --------------------
   -- Create_Control --
   --------------------

   procedure Create_Control
     (Window     : in out Base_Window_Type;
      Parent     : in out Base_Window_Type'Class;
      Win_Class  : in     GString;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      ID         : in     Integer                := 0;
      Styles     : in     Interfaces.C.unsigned  := 0;
      Is_Dynamic : in     Boolean                := False)
   is
      use GWindows.Drawing_Objects;

      procedure Link (Window : in out Base_Window_Type'Class);
      --  Attach HWND to Window

      procedure Link (Window : in out Base_Window_Type'Class) is
      begin
         GWindows.Internal.GWindows_Object_Property_Atom := GlobalAddAtom;
         Set_GWindow_Object (Window.HWND, Window'Unchecked_Access);
      end Link;

      C_Text  : constant GString_C := GWindows.GStrings.To_GString_C (Text);
      C_Class : constant GString_C :=
        GWindows.GStrings.To_GString_C (Win_Class);
      PHWND   : constant GWindows.Types.Handle := Parent.HWND;
      Style   : Interfaces.C.unsigned;
      ExStyle : Interfaces.C.unsigned := 0;

      function CreateWindowEx
        (dwExStyle    : Interfaces.C.unsigned := ExStyle;
         lpClassName  : GString_C             := C_Class;
         lpWindowName : GString_C             := C_Text;
         dwStyle      : Interfaces.C.unsigned := Style;
         x            : Integer               := Left;
         y            : Integer               := Top;
         nWidth       : Integer               := Width;
         nHeight      : Integer               := Height;
         hwndParent   : GWindows.Types.Handle;
         hMenu        : GWindows.Types.Handle := GWindows.Types.To_Handle (ID);
         hInst        : GWindows.Types.Handle :=
            GWindows.Internal.Current_hInstance;
         lpParam      : Interfaces.C.long     := 0)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateWindowEx,
                       "CreateWindowEx" & Character_Mode_Identifier);

      Parent_Font : Font_Type;
   begin
      if (Styles and WS_POPUP) = 0 then
         Style := Styles or WS_CHILDWINDOW;
      else
         Style := Styles;
      end if;

      On_Pre_Create (Base_Window_Type'Class (Window), Style, ExStyle);

      Window.HWND := CreateWindowEx (hwndParent => PHWND);
      Window.Is_Dynamic := Is_Dynamic;

      Link (Window);
      Window.ParentWindowProc := Get_Window_Procedure (Window.HWND);
      Set_Window_Procedure (Window.HWND, New_Proc => WndProc_Control'Access);
      Window.Is_Control := True;
      Get_Font (Parent, Parent_Font);
      Set_Font (Window, Parent_Font);

      On_Create_Start (Base_Window_Type'Class (Window));
      On_Create (Base_Window_Type'Class (Window));
   end Create_Control;

   procedure Use_Mouse_Wheel (Window : in out Base_Window_Type) is
   begin
      Window.Use_Mouse_Wheel := True;
   end Use_Mouse_Wheel;

   ------------
   -- Attach --
   ------------

   --  * AnSp: When linking to special dialogs,
   --  *       like the added child dialog when extending the file open dialog,
   --  *       the original Windows procedures should not be overriden!

   procedure Attach
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True)             -- * AnSp
   is
      procedure Link (Window : in out Base_Window_Type'Class);
      --  Attach HWND to Window

      procedure Link (Window : in out Base_Window_Type'Class) is
      begin
         Set_GWindow_Object (Window.HWND, Window'Unchecked_Access);
      end Link;

   begin
      Window.HWND := HWND;
      Window.Is_Dynamic := Is_Dynamic;

      GWindows.Internal.GWindows_Object_Property_Atom := GlobalAddAtom;
      Link (Window);
      Window.Is_Linked := not Procedures;                            -- * AnSp
      if Procedures then                                             -- * AnSp
         Window.ParentWindowProc := Get_Window_Procedure (Window.HWND);
         Set_Window_Procedure (Window.HWND, New_Proc => WndProc'Access);
      end if;                                                        -- * AnSp
   end Attach;

   -------------------
   -- Attach_Dialog --
   -------------------

   procedure Attach_Dialog
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True)             -- * AnSp
   is
   begin
      Link (Window, HWND, Is_Dynamic, Window_Link, Procedures);      -- * AnSp
      Window.Is_Linked := not Procedures;                            -- * AnSp
      if Procedures then                                             -- * AnSp
         Set_Window_Procedure (Window.HWND, New_Proc => WndProc'Access);
      end if;                                                        -- * AnSp
   end Attach_Dialog;

   --------------------
   -- Attach_Control --
   --------------------

   procedure Attach_Control
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True)             -- * AnSp
   is
      procedure Link (Window : in out Base_Window_Type'Class);
      --  Attach HWND to Window

      procedure Link (Window : in out Base_Window_Type'Class) is
      begin
         GWindows.Internal.GWindows_Object_Property_Atom := GlobalAddAtom;
         Set_GWindow_Object (Window.HWND, Window'Unchecked_Access);
      end Link;

   begin
      Window.HWND := HWND;
      Window.Is_Dynamic := Is_Dynamic;

      Link (Window);
      if Procedures then                                             -- * AnSp
         Window.ParentWindowProc := Get_Window_Procedure (Window.HWND);
         Set_Window_Procedure
            (Window.HWND, New_Proc => WndProc_Control'Access);
      end if;                                                        -- * AnSp
      Window.Is_Control := True;
   end Attach_Control;

   ------------------------
   -- Attach_Dialog_Item --
   ------------------------

   procedure Attach_Dialog_Item
     (Window     : in out Base_Window_Type;
      Parent     : in     Base_Window_Type'Class;
      ID         : in     Integer;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True)             -- * AnSp
   is
      function GetDlgItem
        (HDLG : GWindows.Types.Handle := Handle (Parent);
         NID  : Integer := ID)
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetDlgItem, "GetDlgItem");
   begin
      Attach_Control (Window, GetDlgItem, Is_Dynamic, Procedures);  --  * AnSp
   end Attach_Dialog_Item;

   ----------
   -- Link --
   ----------

   procedure Link
     (Window     : in out Base_Window_Type'Class;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean;
      Link_Type  : in     Link_Type_Type        := Window_Link;
      Procedures : in     Boolean               := True)             -- * AnSp
   is
   begin
      Window.HWND := HWND;
      Window.Is_Dynamic := Is_Dynamic;

      Window.Is_Linked := not Procedures;                            -- * AnSp
      case Link_Type is
         when Window_Link =>
            Window.ParentWindowProc := DefWindowProc'Access;
         when MDI_Child_Link =>
            Window.ParentWindowProc := DefMDIChildProc'Access;
         when Control_Link =>
            Window.Is_Control := True;
            Window.ParentWindowProc := DefWindowProc'Access;
         when Desktop_Link =>
            return;
      end case;

      GWindows.Internal.GWindows_Object_Property_Atom := GlobalAddAtom;
      Set_GWindow_Object (Window.HWND, Window'Unchecked_Access);
      if Procedures then                                             -- * AnSp
         GWindows.Internal.Add_Keyboard_Control (Window'Unchecked_Access);
      end if;                                                        -- * AnSp
   end Link;

   -----------------------
   -- Accelerator_Table --
   -----------------------

   procedure Accelerator_Table
      (Window  : in out Base_Window_Type;
       Name    : in     GString)
   is
      C_Name : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      function LoadAccelerators
      (hInst    : GWindows.Types.Handle := GWindows.Internal.Current_hInstance;
      lpszName : GString_C         := C_Name)
      return GWindows.Types.Handle;
      pragma Import (StdCall, LoadAccelerators,
                       "LoadAccelerators" & Character_Mode_Identifier);
   begin
      Window.haccel := LoadAccelerators;
   end  Accelerator_Table;

   -----------
   -- Focus --
   -----------

   function Focus return Pointer_To_Base_Window_Class is
      use GWindows.Types;

      function GetFocus return GWindows.Types.Handle;
      pragma Import (StdCall, GetFocus, "GetFocus");

      Focused_Win : constant GWindows.Types.Handle := GetFocus;
   begin
      if Focused_Win /= GWindows.Types.Null_Handle then
         return Window_From_Handle (GetFocus);
      else
         return null;
      end if;
   end Focus;

   -----------
   -- Focus --
   -----------

   function Focus (Window : in Base_Window_Type)
                  return Pointer_To_Base_Window_Class
   is
   begin
      return Window_From_Handle (Window.Last_Focused);
   end Focus;

   -----------
   -- Focus --
   -----------

   procedure Focus (Window : in out Base_Window_Type) is
      procedure SetFocus  (hwnd : GWindows.Types.Handle);
      pragma  Import (StdCall, SetFocus, "SetFocus");
   begin
      SetFocus (Window.HWND);
   end Focus;

   -----------------
   -- Next_Window --
   -----------------

   function Next_Window (Window : in Base_Window_Type)
                        return Pointer_To_Base_Window_Class
   is
   begin
      return Window_From_Handle (GetWindow (Handle (Window),
                                            GW_HWNDNEXT));
   end Next_Window;

   ---------------------
   -- Previous_Window --
   ---------------------

   function Previous_Window (Window : in Base_Window_Type)
                            return Pointer_To_Base_Window_Class
   is
   begin
      return Window_From_Handle (GetWindow (Handle (Window),
                                            GW_HWNDPREV));
   end Previous_Window;

   ------------------
   -- First_Window --
   ------------------

   function First_Window (Window : in Base_Window_Type)
                         return Pointer_To_Base_Window_Class
   is
   begin
      return Window_From_Handle (GetWindow (Handle (Window),
                                            GW_HWNDFIRST));
   end First_Window;

   -----------------
   -- Last_Window --
   -----------------

   function Last_Window (Window : in Base_Window_Type)
                        return Pointer_To_Base_Window_Class
   is
   begin
      return Window_From_Handle (GetWindow (Handle (Window),
                                            GW_HWNDLAST));
   end Last_Window;

   -----------
   -- Valid --
   -----------

   function Valid (Window : in Base_Window_Type) return Boolean
   is

      function IsWindow (hwnd : GWindows.Types.Handle := Window.HWND)
                        return Interfaces.C.long;
      pragma Import (StdCall, IsWindow, "IsWindow");
   begin
      return IsWindow (Window.HWND) /= 0;
   end Valid;

   ------------
   -- Border --
   ------------

   procedure Border (Window : in out Base_Window_Type;
                     State  : in     Boolean          := True)
   is
   begin
      if State then
         SetWindowLong (Window.HWND,
                        GWL_EXSTYLE,
                        newLong =>
                          GetWindowLong (Window.HWND, GWL_EXSTYLE) or
                          WS_EX_CLIENTEDGE);
         SetWindowPos (Window.HWND);
      else
         SetWindowLong (Window.HWND,
                        GWL_EXSTYLE,
                        newLong =>
                          GetWindowLong (Window.HWND, GWL_EXSTYLE) and not
                          WS_EX_CLIENTEDGE);
         SetWindowPos (Window.HWND);
      end if;
   end Border;

   ------------
   -- Border --
   ------------

   function Border (Window : in Base_Window_Type) return Boolean is
   begin
      return (GetWindowLong (Window.HWND) and WS_BORDER) = WS_BORDER;
   end Border;

   -----------
   -- Group --
   -----------

   procedure Group (Window : in out Base_Window_Type;
                    State  : in     Boolean     := True)
   is
   begin
      if State then
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) or
                          WS_GROUP);
      else
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) and not
                          WS_GROUP);
      end if;
   end Group;

   -----------
   -- Group --
   -----------

   function Group (Window : in Base_Window_Type) return Boolean is
   begin
      return (GetWindowLong (Window.HWND) and WS_GROUP) = WS_GROUP;
   end Group;

   ----------
   -- Dock --
   ----------

   procedure Dock (Window : in out Base_Window_Type;
                   To     : in     Dock_Type)
   is
   begin
      Window.Dock := To;
   end Dock;

   ----------
   -- Dock --
   ----------

   function Dock (Window : in Base_Window_Type) return Dock_Type
   is
   begin
      return Window.Dock;
   end Dock;

   -----------
   -- Order --
   -----------

   procedure Order (Window   : in out Base_Window_Type;
                    Position : in     Order_Position_Type)
   is
      Values : constant array (Order_Position_Type) of Interfaces.C.long :=
        (Top               => HWND_TOP,
         Bottom            => HWND_BOTTOM,
         Always_On_Top     => HWND_TOPMOST,
         Not_Always_On_Top => HWND_NOTOPMOST,
         No_Change         => 0);
   begin
      if Position /= No_Change then
         SetWindowPos (Handle (Window),
                       GWindows.Types.To_Handle (Values (Position)),
                       fuFlags => SWP_NOMOVE or SWP_NOSIZE);
      end if;
   end Order;

   procedure Order (Window       : in out Base_Window_Type;
                    After_Window : in     Base_Window_Type'Class)
   is
   begin
      SetWindowPos (Handle (Window), Handle (After_Window),
                    fuFlags => SWP_NOMOVE or SWP_NOSIZE);
   end Order;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Window : in out Base_Window_Type)
   is
      WM_SETREDRAW : constant := 11;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := WM_SETREDRAW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Window : in out Base_Window_Type)
   is
      WM_SETREDRAW : constant := 11;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := WM_SETREDRAW;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Wparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Thaw;

   --------------
   -- Tab_Stop --
   --------------

   procedure Tab_Stop (Window : in out Base_Window_Type;
                       State  : in     Boolean     := True) is
   begin
      if State then
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) or
                          WS_TABSTOP);
         SetWindowPos (Window.HWND);
      else
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) and not
                          WS_TABSTOP);
         SetWindowPos (Window.HWND);
      end if;
   end Tab_Stop;

   --------------
   -- Tab_Stop --
   --------------

   function Tab_Stop (Window : in Base_Window_Type) return Boolean is
   begin
      return (GetWindowLong (Window.HWND) and WS_TABSTOP) = WS_TABSTOP;
   end Tab_Stop;

   -----------------------
   -- Client_Area_Width --
   -----------------------

   procedure Client_Area_Width (Window : in out Base_Window_Type;
                                Value  : in     Natural)
   is
   begin
      Client_Area_Size (Window, Value, Client_Area_Height (Window));
   end Client_Area_Width;

   -----------------------
   -- Client_Area_Width --
   -----------------------

   function Client_Area_Width (Window : in Base_Window_Type)
                              return Natural
   is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetClientRect (Window.HWND, Rect);
      return abs (Rect.Right - Rect.Left);
   end Client_Area_Width;

   ------------------------
   -- Client_Area_Height --
   ------------------------

   procedure Client_Area_Height (Window : in out Base_Window_Type;
                                 Value  : in     Natural)
   is
   begin
      Client_Area_Size (Window, Client_Area_Width (Window), Value);
   end Client_Area_Height;

   ------------------------
   -- Client_Area_Height --
   ------------------------

   function Client_Area_Height (Window : in Base_Window_Type)
                               return Natural
   is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetClientRect (Window.HWND, Rect);
      return abs (Rect.Bottom - Rect.Top);
   end Client_Area_Height;

   ----------------------
   -- Client_Area_Size --
   ----------------------

   procedure Client_Area_Size (Window : in out Base_Window_Type;
                               Value  : in     GWindows.Types.Size_Type)
   is
   begin
      Client_Area_Size (Window, Value.Width, Value.Height);
   end Client_Area_Size;

   procedure Client_Area_Size (Window : in out Base_Window_Type;
                               Width  : in     Natural;
                               Height : in     Natural)
   is
   begin
      Size (Window, Calculate_New_Window_Size (Window, (Width, Height)));
   end Client_Area_Size;

   function Client_Area_Size (Window : in Base_Window_Type)
                             return GWindows.Types.Size_Type
   is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetClientRect (Window.HWND, Rect);
      return (abs (Rect.Right - Rect.Left), abs (Rect.Bottom - Rect.Top));
   end Client_Area_Size;

   -------------------------------
   -- Calculate_New_Window_Size --
   -------------------------------

   function Calculate_New_Window_Size
     (Window           : in Base_Window_Type;
      Client_Area_Size : in GWindows.Types.Size_Type)
     return GWindows.Types.Size_Type
   is
      use GWindows.Types;
   begin
      return (Size (Window) - GWindows.Base.Client_Area_Size (Window)) +
        Client_Area_Size;
      --  Returns the actual Margin plus the desired Client Area,
      --  which is also equal to the actual Size plus the Delta in Client Area.
   end Calculate_New_Window_Size;

   ----------
   -- Size --
   ----------

   procedure Size (Window : in out Base_Window_Type;
                   Value  : in     GWindows.Types.Size_Type)
   is
   begin
      Size (Window, Value.Width, Value.Height);
   end Size;

   procedure Size (Window : in out Base_Window_Type;
                   Width  : in     Natural;
                   Height : in     Natural)
   is
   begin
      SetWindowPos (Window.HWND,
                    cx       => Width,
                    cy       => Height,
                    fuFlags  =>
                      SWP_NOMOVE or SWP_NOZORDER);
   end Size;

   function Size (Window : in Base_Window_Type)
                 return GWindows.Types.Size_Type
   is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      return (abs (Rect.Right - Rect.Left),
              abs (Rect.Bottom - Rect.Top));
   end Size;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Window : in Base_Window_Type)
                             return GWindows.Types.Size_Type
   is begin
      return Size (Window);
   end Recommended_Size;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Window : in out Base_Window_Type;
                       Font   : in     GWindows.Drawing_Objects.Font_Type)
   is
      use GWindows.Drawing_Objects;

      WM_SETFONT   : constant := 48;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := WM_SETFONT;
         wParam : GWindows.Types.Handle := Handle (Font);
         lParam : GWindows.Types.Lparam := 1);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Font;

   --------------
   -- Get_Font --
   --------------

   procedure Get_Font (Window : in     Base_Window_Type;
                       Font   :    out GWindows.Drawing_Objects.Font_Type)
   is
      use GWindows.Drawing_Objects;

      WM_GETFONT   : constant := 49;

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := WM_GETFONT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Handle;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Handle (Font, SendMessage);
      Protected_Object (Font);
   end Get_Font;

   ----------
   -- Text --
   ----------

   procedure Text (Window : in out Base_Window_Type;
                   Text   : in     GString)
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure SetWindowText
        (hwnd : GWindows.Types.Handle := Window.HWND;
         Text : GString_C             := C_Text);
      pragma Import (StdCall, SetWindowText,
                       "SetWindowText" & Character_Mode_Identifier);
   begin
      SetWindowText;
   end Text;

   ----------
   -- Text --
   ----------

   function Text (Window : in Base_Window_Type)
                 return GString
   is
      Buf : GString_C (1 .. Interfaces.C.size_t
                       (Text_Length (Window) + 1));

      procedure GetWindowText
        (hwnd : in     GWindows.Types.Handle   := Window.HWND;
         Text : access GChar_C                 := Buf (Buf'First)'Access;
         Max  : in     Interfaces.C.size_t     := Buf'Last);
      pragma Import (StdCall, GetWindowText,
                       "GetWindowText" & Character_Mode_Identifier);
   begin
      GetWindowText;
      return GWindows.GStrings.To_GString_From_C (Buf);
   end Text;

   -----------------
   -- Text_Length --
   -----------------

   function Text_Length (Window : in Base_Window_Type) return Integer is
      function GetWindowTextLength
        (hwnd : GWindows.Types.Handle   := Window.HWND)
        return Integer;
      pragma Import (StdCall, GetWindowTextLength,
                       "GetWindowTextLength" & Character_Mode_Identifier);
   begin
      return GetWindowTextLength;
   end Text_Length;

   --------------
   -- Location --
   --------------

   procedure Location (Window : in out Base_Window_Type;
                       Value  : in     GWindows.Types.Rectangle_Type;
                       Order  : in     Order_Position_Type := No_Change)
   is
      Flag : Interfaces.C.unsigned := SWP_NOZORDER;
      Values : constant array (Order_Position_Type) of Interfaces.C.long :=
        (Top               => HWND_TOP,
         Bottom            => HWND_BOTTOM,
         Always_On_Top     => HWND_TOPMOST,
         Not_Always_On_Top => HWND_NOTOPMOST,
         No_Change         => 0);
   begin
--      Move (Window, Value.Left, Value.Top);
--      Size (Window,
--            Width  => abs (Value.Right - Value.Left),
--            Height => abs (Value.Bottom - Value.Top));
      if Order /= No_Change then
         Flag := 0;  --  SWP_NOACTIVATE;
      end if;
      SetWindowPos (Window.HWND,
         GWindows.Types.To_Handle (Values (Order)),
         x => Value.Left,
         y => Value.Top,
         cx => abs (Value.Right - Value.Left),
         cy => abs (Value.Bottom - Value.Top),
         fuFlags => Flag);
   end Location;

   function Location (Window : in Base_Window_Type) return Types.Rectangle_Type
   is
      Rect         : GWindows.Types.Rectangle_Type;
      Left_Top     : GWindows.Types.Point_Type;
      Right_Bottom : GWindows.Types.Point_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      if Control (Window) then
         Left_Top := Point_To_Client
           (Parent (Window).all, (Rect.Left, Rect.Top));
         Right_Bottom := Point_To_Client
           (Parent (Window).all, (Rect.Right, Rect.Bottom));
         return (Left_Top.X, Left_Top.Y, Right_Bottom.X, Right_Bottom.Y);
      else
         return Rect;
      end if;
   end Location;

   procedure Location (Window : in out Base_Window_Type;
                       Value  : in     GWindows.Types.Point_Type)
   is
   begin
      Move (Window, Value.X, Value.Y);
   end Location;

   function Location (Window : in Base_Window_Type)
                     return GWindows.Types.Point_Type
   is
      Rect : constant GWindows.Types.Rectangle_Type := Location (Window);
   begin
      return (Rect.Left, Rect.Top);
   end Location;

   ----------
   -- Left --
   ----------

   procedure Left (Window : in out Base_Window_Type;
                   Value  : in     Integer)
   is
   begin
      Move (Window, Value, Top (Window));
   end Left;

   ----------
   -- Left --
   ----------

   function Left (Window : in Base_Window_Type) return Integer is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      if Control (Window) then
         return Point_To_Client
           (Parent (Window).all, (Rect.Left, Rect.Top)).X;
      end if;
      return Rect.Left;
   end Left;

   ---------
   -- Top --
   ---------

   procedure Top (Window : in out Base_Window_Type;
                  Value  : in     Integer)
   is
   begin
      Move (Window, Left (Window), Value);
   end Top;

   function Top (Window : in Base_Window_Type) return Integer is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      if Control (Window) then
         return Point_To_Client
           (Parent (Window).all, (Rect.Left, Rect.Top)).Y;
      end if;
      return Rect.Top;
   end Top;

   -----------
   -- Width --
   -----------

   procedure Width (Window : in out Base_Window_Type;
                    Value  : in     Natural)
   is
   begin
      Size (Window, Value, Height (Window));
   end Width;

   -----------
   -- Width --
   -----------

   function Width (Window : in Base_Window_Type) return Natural is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      return abs (Rect.Right - Rect.Left);
   end Width;

   ------------
   -- Height --
   ------------

   procedure Height (Window : in out Base_Window_Type;
                     Value  : in     Natural)
   is
   begin
      Size (Window, Width (Window), Value);
   end Height;

   ------------
   -- Height --
   ------------

   function Height (Window : in Base_Window_Type) return Natural is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetWindowRect (Window.HWND, Rect);
      return abs (Rect.Bottom - Rect.Top);
   end Height;

   ---------------------------
   -- Horizontal_Scroll_Bar --
   ---------------------------

   procedure Horizontal_Scroll_Bar (Window : in out Base_Window_Type;
                                    State  : in     Boolean := True) is
   begin
      if State then
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) or
                          WS_HSCROLL);
         SetWindowPos (Window.HWND);
      else
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) and not
                          WS_HSCROLL);
         SetWindowPos (Window.HWND);
      end if;
   end Horizontal_Scroll_Bar;

   ---------------------------
   -- Horizontal_Scroll_Bar --
   ---------------------------

   function Horizontal_Scroll_Bar (Window : in Base_Window_Type)
                                  return Boolean is
   begin
      return (GetWindowLong (Window.HWND) and WS_HSCROLL) = WS_HSCROLL;
   end Horizontal_Scroll_Bar;

   -------------------------
   -- Vertical_Scroll_Bar --
   -------------------------

   procedure Vertical_Scroll_Bar (Window : in out Base_Window_Type;
                                  State  : in     Boolean := True) is
   begin
      if State then
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) or
                          WS_VSCROLL);
         SetWindowPos (Window.HWND);
      else
         SetWindowLong (Window.HWND,
                        newLong =>
                          GetWindowLong (Window.HWND) and not
                          WS_VSCROLL);
         SetWindowPos (Window.HWND);
      end if;
   end Vertical_Scroll_Bar;

   -------------------------
   -- Vertical_Scroll_Bar --
   -------------------------

   function Vertical_Scroll_Bar (Window : in Base_Window_Type)
                                return Boolean is
   begin
      return (GetWindowLong (Window.HWND) and WS_VSCROLL) = WS_VSCROLL;
   end Vertical_Scroll_Bar;

   ---------------------
   -- Point_To_Client --
   ---------------------

   function Point_To_Client (Window : in Base_Window_Type;
                             Point  : in GWindows.Types.Point_Type)
                            return GWindows.Types.Point_Type
   is
      New_Point  : GWindows.Types.Point_Type := Point;

      procedure ScreenToClient
        (hwnd    : GWindows.Types.Handle := Window.HWND;
         ppPoint : in out GWindows.Types.Point_Type);
      pragma Import (StdCall, ScreenToClient, "ScreenToClient");
   begin
      ScreenToClient (ppPoint => New_Point);

      return New_Point;
   end Point_To_Client;

   ----------------------
   -- Point_To_Desktop --
   ----------------------

   function Point_To_Desktop (Window : in Base_Window_Type;
                              Point  : in GWindows.Types.Point_Type)
                             return GWindows.Types.Point_Type
   is
      New_Point  : GWindows.Types.Point_Type := Point;

      procedure ClientToScreen
        (hwnd    : GWindows.Types.Handle := Window.HWND;
         ppPoint : in out GWindows.Types.Point_Type);
      pragma Import (StdCall, ClientToScreen, "ClientToScreen");
   begin
      ClientToScreen (ppPoint => New_Point);

      return New_Point;
   end Point_To_Desktop;

   ----------------
   -- End_Dialog --
   ----------------

   procedure End_Dialog (Window : in out Base_Window_Type;
                         Result : in     Integer)
   is
   begin
      Modal_Result (Window, Result);
      Close (Window);
   end End_Dialog;

   ----------------
   -- Get_Canvas --
   ----------------

   procedure Get_Canvas
      (Window : in     Base_Window_Type;
       Canvas :    out GWindows.Drawing.Canvas_Type)
   is
      function GetDC
        (hwnd   : GWindows.Types.Handle := GWindows.Base.Handle (Window))
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetDC, "GetDC");
   begin
      GWindows.Drawing.Capture (Canvas, GWindows.Base.Handle (Window), GetDC);
   end Get_Canvas;

   ----------------------------
   -- Get_Full_Window_Canvas --
   ----------------------------

   procedure Get_Full_Window_Canvas
     (Window : in     Base_Window_Type;
      Canvas :    out GWindows.Drawing.Canvas_Type)
   is
      function GetWindowDC
        (hwnd   : GWindows.Types.Handle := Handle (Window))
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetWindowDC, "GetWindowDC");
   begin
      GWindows.Drawing.Capture (Canvas, Handle (Window), GetWindowDC);
   end Get_Full_Window_Canvas;

   ----------------------
   -- Keyboard_Support --
   ----------------------

   procedure Keyboard_Support (Window : in out Base_Window_Type;
                               State  : in     Boolean     := True)
   is
   begin
      Window.Keyboard_Support := State;
   end Keyboard_Support;

   ----------------------
   -- Keyboard_Support --
   ----------------------

   function Keyboard_Support (Window : in Base_Window_Type) return Boolean is
   begin
      return Window.Keyboard_Support;
   end Keyboard_Support;

   -------------
   -- Visible --
   -------------

   procedure Visible (Window : in out Base_Window_Type;
                      State  : in     Boolean := True)
   is
      SW_SHOW            : constant := 5;
      SW_HIDE            : constant := 0;

      procedure ShowWindow
        (hwnd     : GWindows.Types.Handle;
         nCmdShow : Interfaces.C.long);
      pragma Import (StdCall, ShowWindow, "ShowWindow");
   begin
      if State then
         ShowWindow (Window.HWND, SW_SHOW);
      else
         ShowWindow (Window.HWND, SW_HIDE);
      end if;
   end Visible;

   -------------
   -- Visible --
   -------------

   function Visible (Window : in Base_Window_Type) return Boolean is
      function IsWindowVisible
        (hwnd : GWindows.Types.Handle   := Window.HWND)
        return Integer;
      pragma Import (StdCall, IsWindowVisible, "IsWindowVisible");
   begin
      return IsWindowVisible /= 0;
   end Visible;

   -------------
   -- Enabled --
   -------------

   procedure Enabled (Window : in out Base_Window_Type;
                      State  : in     Boolean := True)
   is
   begin
      if State then
         Enable (Window);
      else
         Disable (Window);
      end if;
   end Enabled;

   function Enabled (Window : in Base_Window_Type) return Boolean
   is
      function IsWindowEnabled
        (hwnd : GWindows.Types.Handle   := Window.HWND)
        return Integer;
      pragma Import (StdCall, IsWindowEnabled, "IsWindowEnabled");
   begin
      return IsWindowEnabled /= 0;
   end Enabled;

   ----------
   -- Show --
   ----------

   procedure Show (Window : in out Base_Window_Type)
   is
   begin
      Visible (Window, True);
   end Show;

   ----------
   -- Hide --
   ----------

   procedure Hide (Window : in out Base_Window_Type)
   is
   begin
      Visible (Window, False);
   end Hide;

   ---------------
   -- Is_Dialog --
   ---------------

   function Is_Dialog (Window : in Base_Window_Type) return Boolean
   is
   begin
      return Window.In_Dialog;
   end Is_Dialog;

   ---------------
   -- Is_Dialog --
   ---------------

   procedure Is_Dialog (Window : in out Base_Window_Type;
                        Value  : in     Boolean)
   is
   begin
      Window.In_Dialog := Value;
   end Is_Dialog;

   --------------
   -- Is_Modal --
   --------------

   procedure Is_Modal (Window          : in out Base_Window_Type;
                       Value           : in     Boolean;
                       Disabled_Parent : in     Pointer_To_Base_Window_Class)
   is
   begin
      Window.Is_Modal := Value;
      Window.Disabled_Parent := Disabled_Parent;
   end Is_Modal;

   --------------
   -- Is_Modal --
   --------------

   function Is_Modal (Window : in Base_Window_Type) return Boolean
   is
   begin
      return Window.Is_Modal;
   end Is_Modal;

   ------------------
   -- Modal_Result --
   ------------------

   procedure Modal_Result (Window : in out Base_Window_Type;
                           Result : in     Integer)
   is
   begin
      Window.Modal_Result := Result;
   end Modal_Result;

   ------------------
   -- Modal_Result --
   ------------------

   function Modal_Result (Window : in Base_Window_Type) return Integer
   is
   begin
      return Window.Modal_Result;
   end Modal_Result;

   --------
   -- ID --
   --------

   function ID (Window : in Base_Window_Type) return Integer
   is
      function GetDlgCtrlID (HWND : GWindows.Types.Handle := Window.HWND)
                            return Integer;
      pragma Import (StdCall, GetDlgCtrlID, "GetDlgCtrlID");
   begin
      return GetDlgCtrlID;
   end ID;

   ------------
   -- Parent --
   ------------

   procedure Parent (Window        : in out Base_Window_Type;
                     Parent_Window : in out Base_Window_Type'Class)
   is
      procedure SetParent
        (hwnd  : GWindows.Types.Handle := Window.HWND;
         phwnd : GWindows.Types.Handle := Parent_Window.HWND);
      pragma Import (StdCall, SetParent, "SetParent");
   begin
      SetParent;
   end Parent;

   ------------
   -- Parent --
   ------------

   function Parent (Window : in Base_Window_Type)
                   return Pointer_To_Base_Window_Class
   is
      function GetParent (hwnd : GWindows.Types.Handle)
                         return GWindows.Types.Handle;
      pragma Import (StdCall, GetParent, "GetParent");

      PHWND : constant GWindows.Types.Handle := GetParent (Window.HWND);
   begin
      return Window_From_Handle (PHWND);
   end Parent;

   ------------------------
   -- Controlling_Parent --
   ------------------------

   function Controlling_Parent (Window : in Base_Window_Type)
     return Pointer_To_Base_Window_Class
   is
      Parent_Win : Pointer_To_Base_Window_Class := Parent (Window);
   begin
      while (Parent_Win /= null) and then Control (Parent_Win.all) loop
         Parent_Win := Parent (Parent_Win.all);
      end loop;

      return Parent_Win;
   end Controlling_Parent;

   -----------
   -- Child --
   -----------

   function Child (Window : in Base_Window_Type) return Boolean is
   begin
      return Parent (Window) /= null;
   end Child;

   -------------
   -- Control --
   -------------

   function Control (Window : in Base_Window_Type) return Boolean is
   begin
      return Window.Is_Control;
   end Control;

   -----------
   -- Close --
   -----------

   procedure Close (Window : in out Base_Window_Type)
   is
      WM_CLOSE                   : constant := 16;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := WM_CLOSE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
--        procedure DestroyWindow (hwnd : GWindows.Types.Handle);
--        pragma Import (StdCall, DestroyWindow, "DestroyWindow");

--        Window_HWND : GWindows.Types.Handle := Window.HWND;
--     begin
--        DestroyWindow (Window_HWND);
   end Close;

   ------------
   -- Enable --
   ------------

   procedure Enable (Window : in out Base_Window_Type)
   is
      procedure EnableWindow
        (hwnd    : GWindows.Types.Handle := Window.HWND;
         bEnable : Interfaces.C.long     := 1);
      pragma  Import (StdCall, EnableWindow, "EnableWindow");
   begin
      EnableWindow;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Window : in out Base_Window_Type)
   is
      procedure EnableWindow
        (hwnd    : GWindows.Types.Handle := Window.HWND;
         bEnable : Interfaces.C.long     := 0);
      pragma  Import (StdCall, EnableWindow, "EnableWindow");
   begin
      EnableWindow;
   end Disable;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (Window     : in out Base_Window_Type;
                     Erase      : in     Boolean          := False;
                     Redraw_Now : in     Boolean          := False)
   is
      RDW_INVALIDATE : constant := 16#0001#;
      RDW_UPDATENOW  : constant := 16#0100#;

      procedure InvalidateRect
        (Hwnd   : GWindows.Types.Handle := Window.HWND;
         lpRect : Integer               := 0;
         bErase : Integer               := Boolean'Pos (Erase));
      pragma Import (StdCall, InvalidateRect, "InvalidateRect");

      procedure RedrawWindow
        (Hwnd : GWindows.Types.Handle := Window.HWND;
         lprcUpdate : Integer := 0;
         hrgnUpdate : Integer := 0;
         Flags      : Interfaces.C.unsigned :=
                      RDW_INVALIDATE or RDW_UPDATENOW);
      pragma Import (StdCall, RedrawWindow, "RedrawWindow");

   begin
      InvalidateRect;
      if Redraw_Now then
         RedrawWindow;
      end if;
   end Redraw;

   -------------------
   -- Capture_Mouse --
   -------------------

   procedure Capture_Mouse (Window : in out Base_Window_Type)
   is
      procedure SetCapture
        (hwnd : GWindows.Types.Handle := Handle (Window));
      pragma Import (StdCall, SetCapture, "SetCapture");
   begin
      SetCapture;
   end Capture_Mouse;

   -------------------
   -- Release_Mouse --
   -------------------

   procedure Release_Mouse
   is
      procedure ReleaseCapture;
      pragma Import (StdCall, ReleaseCapture, "ReleaseCapture");
   begin
      ReleaseCapture;
   end Release_Mouse;

   ------------
   -- Center --
   ------------

   procedure Center (Window : in out Base_Window_Type) is
   begin
      if not Control (Window) then
         if not Child (Window) then
            Move (Window,
                  (GWindows.Internal.Desktop_Width - Width (Window)) / 2,
                  (GWindows.Internal.Desktop_Height - Height (Window)) / 2);
         else
            Center (Window, Parent (Window).all);
         end if;
      else
         Move
           (Window,
            (Client_Area_Width (Parent (Window).all) - Width (Window)) / 2,
            (Client_Area_Height (Parent (Window).all) - Height (Window)) / 2);
      end if;
   end Center;

   ------------
   -- Center --
   ------------

   procedure Center (Window : in out Base_Window_Type;
                     Other  : in     Base_Window_Type'Class)
   is
   begin
      Move (Window,
            (Width (Other) - Width (Window)) / 2 + Left (Other),
            (Height (Other) - Height (Window)) / 2 + Top (Other));
   end Center;

   ----------
   -- Move --
   ----------

   procedure Move (Window : in out Base_Window_Type;
                   Left   : in     Integer;
                   Top    : in     Integer)
   is
   begin
      SetWindowPos (Window.HWND,
                    x       => Left,
                    y       => Top,
                    fuFlags =>
                      SWP_NOSIZE or SWP_NOZORDER);
   end Move;

   -------------------
   -- Next_Tab_Stop --
   -------------------

   function Next_Tab_Stop (Window  : in Base_Window_Type;
                           Control : in Base_Window_Type'Class)
                          return Pointer_To_Base_Window_Class
   is
      function GetNextDlgTabItem
        (HWNND : GWindows.Types.Handle := Handle (Window);
         HCTL  : GWindows.Types.Handle := Handle (Control);
         PREV  : Boolean               := False)
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetNextDlgTabItem, "GetNextDlgTabItem");

      Result : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (GetNextDlgTabItem);
   begin
      return Result;
   end Next_Tab_Stop;

   -----------------------
   -- Previous_Tab_Stop --
   -----------------------

   function Previous_Tab_Stop (Window  : in Base_Window_Type;
                               Control : in Base_Window_Type'Class)
                              return Pointer_To_Base_Window_Class
   is
      function GetNextDlgTabItem
        (HWNND : GWindows.Types.Handle := Handle (Window);
         HCTL  : GWindows.Types.Handle := Handle (Control);
         PREV  : Boolean               := True)
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetNextDlgTabItem, "GetNextDlgTabItem");

      Result : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (GetNextDlgTabItem);
   begin
      return Result;
   end Previous_Tab_Stop;

   -------------------------
   --  Enumerate_Children --
   -------------------------

   function Enum_Proc (hwnd   : GWindows.Types.Handle;
                       lparam : Enumerate_Function)
                      return Boolean;
   pragma Convention (StdCall, Enum_Proc);

   function Enum_Proc (hwnd   : GWindows.Types.Handle;
                       lparam : Enumerate_Function)
                      return Boolean
   is
      Win_Ptr : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (hwnd);
   begin
      if Win_Ptr /= null then
         lparam (Win_Ptr);
      end if;
      return True;
   end Enum_Proc;

   procedure Enumerate_Children (Window : in Base_Window_Type;
                                 Proc   : in Enumerate_Function)
   is
      procedure EnumChildWindows (hwnd   : GWindows.Types.Handle;
                                  Proc   : System.Address;
                                  lparam : Enumerate_Function);
      pragma Import (StdCall, EnumChildWindows, "EnumChildWindows");
   begin
      EnumChildWindows (Window.HWND, Enum_Proc'Address, Proc);
   end Enumerate_Children;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Base_Window_Type) is
   begin
      Fire_On_Create (Window);
   end On_Create;

   -----------------------
   -- On_Create_Handler --
   -----------------------

   procedure On_Create_Handler (Window  : in out Base_Window_Type;
                                Handler : in Action_Event)
   is
   begin
      Window.On_Create_Event := Handler;
   end On_Create_Handler;

   --------------------
   -- Fire_On_Create --
   --------------------

   procedure Fire_On_Create (Window : in out Base_Window_Type)
   is
   begin
      if Window.On_Create_Event /= null then
         Window.On_Create_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Create;

   ----------------------
   --  On_Create_Start --
   ----------------------

   procedure On_Create_Start (Window : in out Base_Window_Type)
   is
   begin
      Fire_On_Create_Start (Window);
   end On_Create_Start;

   -----------------------------
   -- On_Create_Start_Handler --
   -----------------------------

   procedure On_Create_Start_Handler (Window  : in out Base_Window_Type;
                                      Handler : in Action_Event)
   is
   begin
      Window.On_Create_Start_Event := Handler;
   end On_Create_Start_Handler;

   --------------------------
   -- Fire_On_Create_Start --
   --------------------------

   procedure Fire_On_Create_Start (Window : in out Base_Window_Type)
   is
   begin
      if Window.On_Create_Start_Event /= null then
         Window.On_Create_Start_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Create_Start;

   -------------------
   -- On_Pre_Create --
   -------------------

   procedure On_Pre_Create (Window    : in out Base_Window_Type;
                            dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned)
   is
   begin
      Fire_On_Pre_Create (Window, dwStyle, dwExStyle);
   end On_Pre_Create;

   ---------------------------
   -- On_Pre_Create_Handler --
   ---------------------------

   procedure On_Pre_Create_Handler (Window  : in out Base_Window_Type;
                                    Handler : in Pre_Create_Event)
   is
   begin
      Window.On_Pre_Create_Event := Handler;
   end On_Pre_Create_Handler;

   ------------------------
   -- Fire_On_Pre_Create --
   ------------------------

   procedure Fire_On_Pre_Create (Window     : in out Base_Window_Type;
                                 dwStyle    : in out Interfaces.C.unsigned;
                                 dwExStyle  : in out Interfaces.C.unsigned)
   is
   begin
      if Window.On_Pre_Create_Event /= null then
         Window.On_Pre_Create_Event (Base_Window_Type'Class (Window),
                                     dwStyle,
                                     dwExStyle);
      end if;
   end Fire_On_Pre_Create;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out Base_Window_Type) is
      function GetCurrentThreadId
        return Interfaces.C.unsigned_long;
      pragma Import (StdCall, GetCurrentThreadId, "GetCurrentThreadId");

      WM_QUIT      : constant := 18;

      procedure PostThreadMessage
        (idThread : Interfaces.C.unsigned_long;
         msg      : Interfaces.C.unsigned;
         wParam   : GWindows.Types.Wparam := 0;
         lParam   : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, PostThreadMessage,
                       "PostThreadMessage" & Character_Mode_Identifier);
   begin
      Fire_On_Destroy (Window);
      Destroy_Children (Window);
      if Is_Modal (Window) then
         PostThreadMessage (GetCurrentThreadId, WM_QUIT);
      end if;
   end On_Destroy;

   ------------------------
   -- On_Destroy_Handler --
   ------------------------

   procedure On_Destroy_Handler (Window  : in out Base_Window_Type;
                                 Handler : in     Action_Event)
   is
   begin
      Window.On_Destroy_Event := Handler;
   end On_Destroy_Handler;

   ---------------------
   -- Fire_On_Destroy --
   ---------------------

   procedure Fire_On_Destroy (Window : in out Base_Window_Type)
   is
   begin
      if Window.On_Destroy_Event /= null then
         Window.On_Destroy_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Destroy;

   ---------------------
   -- On_Context_Menu --
   ---------------------

   procedure On_Context_Menu (Window : in out Base_Window_Type;
                              X      : in     Integer;
                              Y      : in     Integer)
   is
   begin
      Fire_On_Context_Menu (Window, X, Y);
   end On_Context_Menu;

   -----------------------------
   -- On_Context_Menu_Handler --
   -----------------------------

   procedure On_Context_Menu_Handler (Window  : in out Base_Window_Type;
                                      Handler : in Location_Action_Event)
   is
   begin
      Window.On_Context_Menu_Event := Handler;
   end On_Context_Menu_Handler;

   --------------------------
   -- Fire_On_Context_Menu --
   --------------------------

   procedure Fire_On_Context_Menu (Window : in out Base_Window_Type;
                                   X      : in     Integer;
                                   Y      : in     Integer)
   is
   begin
      if Window.On_Context_Menu_Event /= null then
         Window.On_Context_Menu_Event (Base_Window_Type'Class (Window),
                                       X, Y);
      end if;
   end Fire_On_Context_Menu;

   ----------------------------------
   -- On_Input_Handler --
   ----------------------------------

   procedure On_Input_Handler
     (Window  : in out Base_Window_Type;
      Handler : in     Raw_Input_Event)
   is
   begin
      Window.On_Input_Event := Handler;
   end On_Input_Handler;

   -------------------------------
   -- Fire_Input_Scroll --
   -------------------------------

   procedure Fire_On_Input
     (Window   : in out Base_Window_Type;
      WParam   : in     Integer;
      RawData  : in     GWindows.Types.Handle;
      Continue :    out Integer)
   is
   begin
      if Window.On_Input_Event /= null then
         Window.On_Input_Event (Base_Window_Type'Class (Window),
                                WParam, RawData, Continue);
      end if;
   end Fire_On_Input;

   --------------------------
   -- On_Input --
   --------------------------

   procedure On_Input
     (Window   : in out Base_Window_Type;
      WParam   : in     Integer;
      RawData  : in     GWindows.Types.Handle;
      Control  : in     Pointer_To_Base_Window_Class;
      Continue :    out Integer)
   is
   begin
      if Control /= null then
         On_Input (Control.all, WParam, RawData, null, Continue);
      else
         Fire_On_Input (Window, WParam, RawData, Continue);
      end if;
   end On_Input;

   --------------------------
   -- On_Horizontal_Scroll --
   --------------------------

   procedure On_Horizontal_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type;
      Control : in     Pointer_To_Base_Window_Class)
   is
   begin
      if Control /= null then
         On_Horizontal_Scroll (Control.all, Request, null);
      else
         Fire_On_Horizontal_Scroll (Window, Request);
      end if;
   end On_Horizontal_Scroll;

   ----------------------------------
   -- On_Horizontal_Scroll_Handler --
   ----------------------------------

   procedure On_Horizontal_Scroll_Handler
     (Window  : in out Base_Window_Type;
      Handler : in     Scroll_Event)
   is
   begin
      Window.On_Horizontal_Scroll_Event := Handler;
   end On_Horizontal_Scroll_Handler;

   -------------------------------
   -- Fire_On_Horizontal_Scroll --
   -------------------------------

   procedure Fire_On_Horizontal_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type)
   is
   begin
      if Window.On_Horizontal_Scroll_Event /= null then
         Window.On_Horizontal_Scroll_Event (Base_Window_Type'Class (Window),
                                            Request);
      end if;
   end Fire_On_Horizontal_Scroll;

   ------------------------
   -- On_Vertical_Scroll --
   ------------------------

   procedure On_Vertical_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type;
      Control : in     Pointer_To_Base_Window_Class)
   is
   begin
      if Control /= null then
         On_Vertical_Scroll (Control.all, Request, null);
      else
         Fire_On_Vertical_Scroll (Window, Request);
      end if;
   end On_Vertical_Scroll;

   --------------------------------
   -- On_Vertical_Scroll_Handler --
   --------------------------------

   procedure On_Vertical_Scroll_Handler
     (Window  : in out Base_Window_Type;
      Handler : in     Scroll_Event)
   is
   begin
      Window.On_Vertical_Scroll_Event := Handler;
   end On_Vertical_Scroll_Handler;

   -----------------------------
   -- Fire_On_Vertical_Scroll --
   -----------------------------

   procedure Fire_On_Vertical_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type)
   is
   begin
      if Window.On_Vertical_Scroll_Event /= null then
         Window.On_Vertical_Scroll_Event (Base_Window_Type'Class (Window),
                                            Request);
      end if;
   end Fire_On_Vertical_Scroll;

   ------------
   -- Handle --
   ------------

   function Handle
     (Window : in Base_Window_Type)
      return GWindows.Types.Handle
   is
   begin
      return Window.HWND;
   end Handle;

   ------------------------
   -- Window_From_Handle --
   ------------------------

   function Window_From_Handle
     (Handle : GWindows.Types.Handle)
      return Pointer_To_Base_Window_Class
   is
      function GetProp
        (hwnd     : GWindows.Types.Handle       := Handle;
         lpString : Interfaces.C.unsigned_short :=
           GWindows.Internal.GWindows_Object_Property_Atom)
        return Pointer_To_Base_Window_Class;
      pragma Import (StdCall, GetProp, "GetProp" & Character_Mode_Identifier);
   begin
      return GetProp;
   end Window_From_Handle;

   -----------------------
   -- MDI_Client_Window --
   -----------------------

   function MDI_Client_Window (Window : in Base_Window_Type)
                              return Base_Window_Access
   is
   begin
      return Window.MDI_Client;
   end MDI_Client_Window;

   -----------------------
   -- MDI_Client_Window --
   -----------------------

   procedure MDI_Client_Window (Window        : in out Base_Window_Type;
                                Client_HWND   : in     GWindows.Types.Handle)
   is
   begin
      Window.MDI_Client := new Base_Window_Type;
      Window.MDI_Client.HWND := Client_HWND;
      Window.MDI_Client.Is_Control := True;
      Window.MDI_Client.Dock := Fill;

      Set_GWindow_Object (Window.MDI_Client.HWND,
                          Pointer_To_Base_Window_Class (Window.MDI_Client));

   end MDI_Client_Window;

   ------------------------
   -- Accelerator_Handle --
   ------------------------

   function Accelerator_Handle (Window : in Base_Window_Type)
                               return GWindows.Types.Handle
   is
   begin
      return Window.haccel;
   end Accelerator_Handle;

   ------------------------
   -- Accelerator_Handle --
   ------------------------

   procedure Accelerator_Handle (Window : in out Base_Window_Type;
                                 HACCEL : in     GWindows.Types.Handle)
   is
   begin
      Window.haccel := HACCEL;
   end Accelerator_Handle;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command
     (Window  : in out Base_Window_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     Pointer_To_Base_Window_Class)
   is
   begin
      if
        Window.In_Dialog and then
        Code = 0 and then
        (ID >= 1) and then
        (ID <= 9)
      then
         End_Dialog (Window, ID);
         return;
      end if;

      if Control /= null then
         On_Command (Control.all, Code, ID, null);
      end if;
   end On_Command;

   ------------------
   -- On_Draw_Item --
   ------------------

   procedure On_Draw_Item
     (Window          : in out Base_Window_Type;
      Canvas          : in out GWindows.Drawing.Canvas_Type;
      Item_ID         : in     Integer;
      Item_Action     : in     Interfaces.C.unsigned;
      Item_State      : in     Interfaces.C.unsigned;
      Item_Rect       : in     GWindows.Types.Rectangle_Type;
      Item_Data       : in     Integer;
      Control         : in     Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, Window);
   begin
      if Control /= null then
         On_Draw_Item (Control.all,
                       Canvas,
                       Item_ID,
                       Item_Action,
                       Item_State,
                       Item_Rect,
                       Item_Data,
                       null);
      end if;
   end On_Draw_Item;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify (Window       : in out Base_Window_Type;
                        Message      : in     Pointer_To_Notification;
                        Control      : in     Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult)
   is
      pragma Warnings (Off, Window);
   begin
      if Control /= null then
         On_Notify (Control.all, Message, null, Return_Value);
      end if;
   end On_Notify;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Window       : in out Base_Window_Type;
                         message      : in     Interfaces.C.unsigned;
                         wParam       : in     GWindows.Types.Wparam;
                         lParam       : in     GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult)
   is
      function DefFrameProc
        (hwnd    : GWindows.Types.Handle;
         chwnd   : GWindows.Types.Handle;
         message : Interfaces.C.unsigned;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, DefFrameProc,
                       "DefFrameProc" & Character_Mode_Identifier);

      function CallWindowProc
        (Proc    : Windproc_Access;
         hwnd    : GWindows.Types.Handle;
         message : Interfaces.C.unsigned;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, CallWindowProc,
                       "CallWindowProc" & Character_Mode_Identifier);
   begin
      if Window.MDI_Client /= null then
         Return_Value := DefFrameProc (Window.HWND,
                                       Window.MDI_Client.HWND,
                                       message,
                                       wParam,
                                       lParam);
      else
         Return_Value := CallWindowProc (Window.ParentWindowProc,
                                         Window.HWND,
                                         message,
                                         wParam,
                                         lParam);
      end if;
   end On_Message;

   -----------------------
   -- On_Filter_Message --
   -----------------------

   procedure On_Filter_Message
     (Window       : in out Base_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult;
      Continue     :    out Boolean)
   is
      pragma Warnings (Off, Window);
      pragma Warnings (Off, message);
      pragma Warnings (Off, wParam);
      pragma Warnings (Off, lParam);
      pragma Warnings (Off, Return_Value);
   begin
      Continue := True;
   end On_Filter_Message;

   -------------------------------------------------------------------------
   --  Local Body
   -------------------------------------------------------------------------

   ------------------------
   -- Set_GWindow_Object --
   ------------------------

   procedure Set_GWindow_Object (HWND   : GWindows.Types.Handle;
                                 Object : Pointer_To_Base_Window_Class)
   is
      procedure SetProp
        (handle   : GWindows.Types.Handle        := HWND;
         lpString : Interfaces.C.unsigned_short  :=
           GWindows.Internal.GWindows_Object_Property_Atom;
         hData    : Pointer_To_Base_Window_Class := Object);
      pragma Import (StdCall, SetProp, "SetProp" & Character_Mode_Identifier);
   begin
      SetProp;
   end Set_GWindow_Object;

   MK_DISPATCHED : constant := 16#4000#;

   --  Send mouse wheel message to a window the mouse pointer is on and
   --  that supports the mouse wheel
   procedure Dispatch_Mouse_Wheel (Win_Ptr : Pointer_To_Base_Window_Class;
                                   message : Interfaces.C.unsigned;
                                   wParam  : GWindows.Types.Wparam;
                                   lParam  : GWindows.Types.Lparam)
   is
      use Interfaces.C;

      P     : Pointer_To_Base_Window_Class := Win_Ptr;
      X     : constant Integer := GWindows.Utilities.Low_Word (lParam);
      Y     : constant Integer := GWindows.Utilities.High_Word (lParam);
      Found : Boolean := False;

      --  Check if point (with desktop coordinates) is on a window
      function On_Window (Window : Pointer_To_Base_Window_Class;
                          X, Y   : Integer)
         return Boolean is
         Point : GWindows.Types.Point_Type;
         X_Min : Integer;
         X_Max : Integer;
         Y_Min : Integer;
         Y_Max : Integer;
      begin
         Point.X := X;
         Point.Y := Y;
         Point := Point_To_Client (Window.all, Point);
         X_Min := Integer'Max (0, -Left (Window.all));
         if Parent (Window.all) = null then
            X_Max := Point.X;
         else
            X_Max := Integer'Min (Width (Window.all) + Left (Window.all),
                                  Width (Parent (Window.all).all)) -
                     Left (Window.all);
         end if;
         Y_Min := Integer'Max (0, -Top (Window.all));
         if Parent (Window.all) = null then
            Y_Max := Point.Y;
         else
            Y_Max := Integer'Min (Height (Window.all) + Top (Window.all),
                                  Height (Parent (Window.all).all)) -
                     Top (Window.all);
         end if;
         return Point.X in X_Min .. X_Max and
                Point.Y in Y_Min .. Y_Max;
      end On_Window;

      procedure Dispatch_Mouse_Wheel
                   (Window : Pointer_To_Base_Window_Class) is
         procedure Handle_Child (Child : Pointer_To_Base_Window_Class) is
            use GWindows.Types;
            Return_Value : GWindows.Types.Lresult := 0;
            P            : Pointer_To_Base_Window_Class;
         begin
            Dispatch_Mouse_Wheel (Child);
            P := Child;
            while P /= null and not Found loop
               if P.all in Base_Window_Type'Class then
                  if On_Window (P, X, Y) and P.Use_Mouse_Wheel then
                     On_Message (P.all, message, wParam + MK_DISPATCHED,
                                 lParam, Return_Value);
                     if Return_Value = 0 then
                        Found := True;
                     end if;
                  end if;
               end if;
               P := Parent (P.all);
            end loop;
         end Handle_Child;
      begin
         Enumerate_Children (Window.all, Handle_Child'Unrestricted_Access);
      end Dispatch_Mouse_Wheel;
   begin
      while Parent (P.all) /= null loop
         P := Parent (P.all);
      end loop;
      Dispatch_Mouse_Wheel (P);
   end Dispatch_Mouse_Wheel;

   function Mouse_Wheel_Dispatched (wParam : GWindows.Types.Wparam)
                                    return Boolean is
      use GWindows.Types;
   begin
      return (wParam and MK_DISPATCHED) = MK_DISPATCHED;
   end Mouse_Wheel_Dispatched;

   -------------
   -- WndProc --
   -------------

   function WndProc
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam)  return GWindows.Types.Lresult is
      WM_DESTROY                 : constant := 2;
      WM_COMMAND                 : constant := 273;
      WM_CONTEXTMENU             : constant := 123;
      WM_NOTIFY                  : constant := 78;
      WM_HSCROLL                 : constant := 276;
      WM_VSCROLL                 : constant := 277;
      WM_SETFOCUS                : constant := 7;
      WM_DRAWITEM                : constant := 43;
      WM_CLOSE                   : constant := 16;
      WM_QUIT                    : constant := 18;
      WM_MOUSEWHEEL              : constant := 522;
      WM_INPUT                   : constant := 255;

      SB_LINEUP                  : constant := 0;
      SB_LINELEFT                : constant := 0;
      SB_LINEDOWN                : constant := 1;
      SB_LINERIGHT               : constant := 1;
      SB_PAGEUP                  : constant := 2;
      SB_PAGELEFT                : constant := 2;
      SB_PAGEDOWN                : constant := 3;
      SB_PAGERIGHT               : constant := 3;
      SB_THUMBPOSITION           : constant := 4;
      SB_THUMBTRACK              : constant := 5;
      SB_TOP                     : constant := 6;
      SB_LEFT                    : constant := 6;
      SB_BOTTOM                  : constant := 7;
      SB_RIGHT                   : constant := 7;

      Win_Ptr : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (hwnd);
   begin
      if Win_Ptr = null then
         return DefWindowProc (hwnd,
                               message,
                               wParam,
                               lParam);
      end if;

      declare
         Return_Value : GWindows.Types.Lresult := 0;
         Continue     : Boolean;
      begin
         On_Filter_Message (Win_Ptr.all,
                            message,
                            wParam,
                            lParam,
                            Return_Value,
                            Continue);

         if not Continue then
            return Return_Value;
         end if;
      end;

      case message is
         when WM_COMMAND =>
            declare
               Control : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (GWindows.Types.To_Handle (lParam));
            begin
               On_Command (Win_Ptr.all,
                           Integer (GWindows.Utilities.Unsigned_High_Word
                                    (wParam)),
                           Integer (GWindows.Utilities.Unsigned_Low_Word
                                    (wParam)),
                           Control);

               if Win_Ptr.MDI_Client = null then
                  return 0;
               end if;
            end;

         when WM_DRAWITEM =>
            declare
               function To_LPDRAWITEMSTRUCT is
                  new Ada.Unchecked_Conversion (GWindows.Types.Lparam,
                                                LPDRAWITEMSTRUCT);

               LPD        : constant LPDRAWITEMSTRUCT :=
                 To_LPDRAWITEMSTRUCT (lParam);
               Canvas     : GWindows.Drawing.Canvas_Type;
               Control    : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (LPD.hwndItem);
            begin
               GWindows.Drawing.Handle (Canvas, LPD.hDC);

               On_Draw_Item (Win_Ptr.all,
                             Canvas,
                             LPD.itemID,
                             LPD.itemAction,
                             LPD.itemState,
                             LPD.rcItem,
                             LPD.itemData,
                             Control);

               return 1;
            end;

         when WM_NOTIFY =>
            declare
               function To_Pointer_To_NMHDR is
                  new Ada.Unchecked_Conversion
                    (GWindows.Types.Lparam, Pointer_To_Notification);

               PNMHDR  : constant Pointer_To_Notification :=
                 To_Pointer_To_NMHDR (lParam);
               Control : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (PNMHDR.Handle);

               Return_Value : GWindows.Types.Lresult := 0;
            begin
               On_Notify (Win_Ptr.all,
                          PNMHDR,
                          Control,
                          Return_Value);

               return Return_Value;
            end;

         when WM_INPUT =>
            declare
               Control : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (GWindows.Types.To_Handle (lParam));
               Temp : GWindows.Types.Lresult;
               pragma Unreferenced (Temp);
               GoOn : Integer;
            begin
               On_Input (Win_Ptr.all, Integer (wParam),
                  GWindows.Types.To_Handle (lParam), Control, GoOn);
               --  Let the system clean-up
               Temp := DefWindowProc (hwnd, message, wParam, lParam);
               return GWindows.Types.Lresult (GoOn);
            end;

         when WM_HSCROLL =>
            declare
               Request : Scroll_Request_Type;
               Control : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (GWindows.Types.To_Handle (lParam));
            begin
               case GWindows.Utilities.Low_Word (wParam) is
                  when SB_LINELEFT =>
                     Request := Previous_Unit;
                  when SB_LINERIGHT =>
                     Request := Next_Unit;
                  when SB_PAGELEFT =>
                     Request := Previous_Page;
                  when SB_PAGERIGHT =>
                     Request := Next_Page;
                  when SB_THUMBPOSITION =>
                     Request := Thumb_Set;
                  when SB_THUMBTRACK =>
                     Request := Thumb_Drag;
                  when SB_LEFT =>
                     Request := First;
                  when SB_RIGHT =>
                     Request := Last;
                  when others =>
                     Request := End_Scroll;
               end case;

               On_Horizontal_Scroll (Win_Ptr.all,
                                     Request,
                                     Control);

               return 0;
            end;

         when WM_VSCROLL =>
            declare
               Request : Scroll_Request_Type;
               Control : constant Pointer_To_Base_Window_Class :=
                 Window_From_Handle (GWindows.Types.To_Handle (lParam));
            begin
               case GWindows.Utilities.Low_Word (wParam) is
                  when SB_LINEUP =>
                     Request := Previous_Unit;
                  when SB_LINEDOWN =>
                     Request := Next_Unit;
                  when SB_PAGEUP =>
                     Request := Previous_Page;
                  when SB_PAGEDOWN =>
                     Request := Next_Page;
                  when SB_THUMBPOSITION =>
                     Request := Thumb_Set;
                  when SB_THUMBTRACK =>
                     Request := Thumb_Drag;
                  when SB_TOP =>
                     Request := First;
                  when SB_BOTTOM =>
                     Request := Last;
                  when others =>
                     Request := End_Scroll;
               end case;

               On_Vertical_Scroll (Win_Ptr.all,
                                   Request,
                                   Control);

               return 0;
            end;

         when WM_SETFOCUS =>
            declare
               Father : constant Pointer_To_Base_Window_Class :=
                 Controlling_Parent (Win_Ptr.all);
            begin
               if Father /= null then
                  Father.Last_Focused := Win_Ptr.HWND;
               end if;
            end;

         when WM_CLOSE | WM_QUIT =>
            if Win_Ptr.Disabled_Parent /= null then
               Enable (Win_Ptr.Disabled_Parent.all);
            end if;

         when WM_DESTROY =>
            On_Destroy (Win_Ptr.all);

            if Win_Ptr.Free_Custom_Data then
               Free (Win_Ptr.Custom_Data);
            end if;

            GWindows.Internal.Remove_Keyboard_Control (Win_Ptr);
            DestroyMenu (GetMenu (Win_Ptr.HWND));
            RemoveProp (hwnd, GWindows.Internal.GWindows_Object_Property_Atom);
            GlobalDeleteAtom (GWindows.Internal.GWindows_Object_Property_Atom);

            if Win_Ptr.MDI_Client /= null then
               Free (Pointer_To_Base_Window_Class (Win_Ptr.MDI_Client));
            end if;

            if Win_Ptr.Is_Dynamic then
               Free (Win_Ptr);
            end if;

            return 0;

         when WM_CONTEXTMENU =>
            On_Context_Menu
              (Win_Ptr.all,
               GWindows.Utilities.Low_Word (lParam),
               GWindows.Utilities.High_Word (lParam));

            return 0;

         when WM_MOUSEWHEEL =>
            case Mouse_Wheel_Target is
               when Focus_Window =>
                  null;
               when Mouse_Window =>
                  if not Mouse_Wheel_Dispatched (wParam) then
                     Dispatch_Mouse_Wheel (Win_Ptr, message, wParam, lParam);
                     return 0;
                  end if;
            end case;
         when others =>
            null;
      end case;

      declare
         Return_Value : GWindows.Types.Lresult := 0;
      begin
         On_Message (Win_Ptr.all,
                     message,
                     wParam,
                     lParam,
                     Return_Value);
         return Return_Value;
      end;
   exception when E : others =>
      if Exception_Handler /= null then
         Exception_Handler (Win_Ptr.all, E);
      end if;
      raise;
   end WndProc;

   ---------------------
   -- WndProc_Control --
   ---------------------

   function WndProc_Control
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam) return GWindows.Types.Lresult is
      GWL_WNDPROC : constant := -4;

      procedure Set_Window_Procedure
        (hwnd     : GWindows.Types.Handle;
         nIndex   : Interfaces.C.int  := GWL_WNDPROC;
         New_Proc : Windproc_Access);
      pragma Import (StdCall, Set_Window_Procedure,
                     "SetWindowLongPtr" & Character_Mode_Identifier);

      WM_DESTROY                 : constant := 2;
      WM_SETFOCUS                : constant := 7;
      WM_CLOSE                   : constant := 16;
      WM_QUIT                    : constant := 18;
      WM_MOUSEWHEEL              : constant := 522;

      Win_Ptr : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (hwnd);
   begin
      if Win_Ptr = null then
         return DefWindowProc (hwnd,
                               message,
                               wParam,
                               lParam);
      end if;

      declare
         Return_Value : GWindows.Types.Lresult := 0;
         Continue     : Boolean;
      begin
         On_Filter_Message (Win_Ptr.all,
                            message,
                            wParam,
                            lParam,
                            Return_Value,
                            Continue);

         if not Continue then
            return Return_Value;
         end if;
      end;

      case message is
         when WM_CLOSE | WM_QUIT =>
            if Win_Ptr.Disabled_Parent /= null then
               Enable (Win_Ptr.Disabled_Parent.all);
            end if;

         when WM_DESTROY =>
            Set_Window_Procedure (hwnd, New_Proc => Win_Ptr.ParentWindowProc);
            On_Destroy (Win_Ptr.all);

            RemoveProp (hwnd, GWindows.Internal.GWindows_Object_Property_Atom);
            GlobalDeleteAtom (GWindows.Internal.GWindows_Object_Property_Atom);

            if Win_Ptr.Is_Dynamic then
               Free (Win_Ptr);
            end if;

            return 0;

         when WM_SETFOCUS =>
            declare
               Father : constant Pointer_To_Base_Window_Class :=
                 Controlling_Parent (Win_Ptr.all);
            begin
               if Father /= null then
                  Father.Last_Focused := Win_Ptr.HWND;
               end if;
            end;

         when WM_MOUSEWHEEL =>
            case Mouse_Wheel_Target is
               when Focus_Window =>
                  null;
               when Mouse_Window =>
                  if not Mouse_Wheel_Dispatched (wParam) then
                     Dispatch_Mouse_Wheel (Win_Ptr, message, wParam, lParam);
                     return 0;
                  end if;
            end case;

         when others =>
            null;
      end case;

      declare
         Return_Value : GWindows.Types.Lresult := 0;
      begin
         On_Message (Win_Ptr.all,
                     message,
                     wParam,
                     lParam,
                     Return_Value);
         return Return_Value;
      end;
   exception when E : others =>
      if Exception_Handler /= null then
         Exception_Handler (Win_Ptr.all, E);
      end if;
      raise;
   end WndProc_Control;

   procedure Destroy_Win
     (Window : GWindows.Base.Pointer_To_Base_Window_Class)
   is
   begin
      GWindows.Internal.Remove_Keyboard_Control (Window);
      DestroyMenu (GetMenu (Window.HWND));
      RemoveProp (Window.HWND,
                  GWindows.Internal.GWindows_Object_Property_Atom);
      GlobalDeleteAtom (GWindows.Internal.GWindows_Object_Property_Atom);

      if Window.MDI_Client /= null then
         Free (Pointer_To_Base_Window_Class (Window.MDI_Client));
      end if;

      Free (Window);
   end Destroy_Win;

   procedure Destroy_Children
     (Window : GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Base.Enumerate_Children (Window,
                                        Destroy_Win'Access);
   end Destroy_Children;

   ----------
   -- Free --
   ----------

   procedure Free (This : in Pointer_To_Base_Window_Class) is
      procedure Free_Object is
         new Ada.Unchecked_Deallocation (Base_Window_Type'Class,
                                         Pointer_To_Base_Window_Class);
   begin
      if This.Is_Dynamic then
         declare
            Object_To_Free : Pointer_To_Base_Window_Class := This;
         begin
            Free_Object (Object_To_Free);
         end;
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Pointer_To_Base_Data_Class) is
      procedure Free_Object is
         new Ada.Unchecked_Deallocation (Base_Data_Type'Class,
                                         Pointer_To_Base_Data_Class);
   begin
      if Data /= null then
         Free_Object (Data);
      end if;
   end Free;

   -----------------------
   -- Set_Standard_Font --
   -----------------------

   procedure Set_Standard_Font
     (Window     : in out Base_Window_Type;
      Stock_Font : in     GWindows.Drawing_Objects.Stock_Font_Type)
   is
      Font : GWindows.Drawing_Objects.Font_Type;
   begin
      GWindows.Drawing_Objects.Create_Stock_Font (Font, Stock_Font);
      Set_Font (Window, Font);
   end Set_Standard_Font;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class (Custom_Class : WNDCLASS)
   is
      pragma Warnings (Off, Custom_Class);
   begin
      RegisterClass (Custom_Class);
   end Register_Class;

   --------------
   -- Run_Mode --
   --------------

   procedure Run_Mode (Window : in out Base_Window_Type;
                       Value  : in     Run_Mode_Type)
   is
   begin
      Window.Run_Mode := Value;
   end Run_Mode;

   function Run_Mode (Window : in Base_Window_Type) return Run_Mode_Type
   is
   begin
      return Window.Run_Mode;
   end Run_Mode;

   -----------------
   -- Custom_Data --
   -----------------

   procedure Custom_Data
     (Window    : in out Base_Window_Type;
      Data      : in     Pointer_To_Base_Data_Class;
      Auto_Free : in     Boolean                   := True)
   is
   begin
      Window.Custom_Data := Data;
      Window.Free_Custom_Data := Auto_Free;
   end Custom_Data;

   function Custom_Data (Window : Base_Window_Type)
                        return Pointer_To_Base_Data_Class
   is
   begin
      return Window.Custom_Data;
   end Custom_Data;

begin
   InitCommonControlsEx;

   Window_Class.hInstance := GWindows.Internal.Current_hInstance;
   Window_Class.hIcon := LoadIcon;
   Window_Class.lpszClassName := GWindows.Internal.Window_Class_Name
     (GWindows.Internal.Window_Class_Name'First)'Access;
   Register_Class (Window_Class);
end GWindows.Base;
