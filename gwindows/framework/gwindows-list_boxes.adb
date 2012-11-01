------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . L I S T _ B O X E S                    --
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

with Interfaces.C;

with GWindows.GStrings;

package body GWindows.List_Boxes is
   use type Interfaces.C.unsigned;
   use GWindows.Types;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

--     LB_CTLCODE                 : constant := 0;
--     LB_OKAY                    : constant := 0;
--     LB_ERR                     : constant := -1;
--     LB_ERRSPACE                : constant := -2;
   LBN_ERRSPACE               : constant := -2;
   LBN_SELCHANGE              : constant := 1;
   LBN_DBLCLK                 : constant := 2;
   LBN_SELCANCEL              : constant := 3;
   LBN_SETFOCUS               : constant := 4;
   LBN_KILLFOCUS              : constant := 5;
   LB_ADDSTRING               : constant := 384;
   LB_INSERTSTRING            : constant := 385;
   LB_DELETESTRING            : constant := 386;
   LB_SELITEMRANGEEX          : constant := 387;
   LB_RESETCONTENT            : constant := 388;
   LB_SETSEL                  : constant := 389;
   LB_SETCURSEL               : constant := 390;
   LB_GETSEL                  : constant := 391;
   LB_GETCURSEL               : constant := 392;
   LB_GETTEXT                 : constant := 393;
   LB_GETTEXTLEN              : constant := 394;
   LB_GETCOUNT                : constant := 395;
   LB_SELECTSTRING            : constant := 396;
--     LB_DIR                     : constant := 397;
   LB_GETTOPINDEX             : constant := 398;
   LB_FINDSTRING              : constant := 399;
   LB_GETSELCOUNT             : constant := 400;
--     LB_GETSELITEMS             : constant := 401;
--     LB_SETTABSTOPS             : constant := 402;
--     LB_GETHORIZONTALEXTENT     : constant := 403;
--     LB_SETHORIZONTALEXTENT     : constant := 404;
--     LB_SETCOLUMNWIDTH          : constant := 405;
--     LB_ADDFILE                 : constant := 406;
   LB_SETTOPINDEX             : constant := 407;
--     LB_GETITEMRECT             : constant := 408;
   LB_GETITEMDATA             : constant := 409;
   LB_SETITEMDATA             : constant := 410;
--     LB_SELITEMRANGE            : constant := 411;
--     LB_SETANCHORINDEX          : constant := 412;
--     LB_GETANCHORINDEX          : constant := 413;
--     LB_SETCARETINDEX           : constant := 414;
--     LB_GETCARETINDEX           : constant := 415;
--     LB_SETITEMHEIGHT           : constant := 416;
--     LB_GETITEMHEIGHT           : constant := 417;
   LB_FINDSTRINGEXACT         : constant := 418;
--     LB_SETLOCALE               : constant := 421;
--     LB_GETLOCALE               : constant := 422;
--     LB_SETCOUNT                : constant := 423;
--     LB_INITSTORAGE             : constant := 424;
--     LB_ITEMFROMPOINT           : constant := 425;
   LBS_NOTIFY                 : constant := 1;
   LBS_SORT                   : constant := 2;
--     LBS_NOREDRAW               : constant := 4;
--     LBS_MULTIPLESEL            : constant := 8;
--     LBS_OWNERDRAWFIXED         : constant := 16;
--     LBS_OWNERDRAWVARIABLE      : constant := 32;
   LBS_HASSTRINGS             : constant := 64;
--     LBS_USETABSTOPS            : constant := 128;
--     LBS_NOINTEGRALHEIGHT       : constant := 256;
--     LBS_MULTICOLUMN            : constant := 512;
--     LBS_WANTKEYBOARDINPUT      : constant := 1024;
   LBS_EXTENDEDSEL            : constant := 2048;
--     LBS_DISABLENOSCROLL        : constant := 4096;
--     LBS_NODATA                 : constant := 8192;
--     LBS_NOSEL                  : constant := 16384;
--     LBS_STANDARD               : constant := 10485763;
   WS_TABSTOP                 : constant := 65536;
   WS_VSCROLL          : constant := 2097152;
   WS_HSCROLL          : constant := 1048576;

--     WM_USER                    : constant := 16#400#;
--     DL_BEGINDRAG               : constant := (WM_USER + 133);
--     DL_DRAGGING                : constant := (WM_USER + 134);
--     DL_DROPPED                 : constant := (WM_USER + 135);
--     DL_CANCELDRAG              : constant := (WM_USER + 136);

--     DL_CURSORSET               : constant := 0;
--     DL_STOPCURSOR              : constant := 1;
--     DL_COPYCURSOR              : constant := 2;
--     DL_MOVECURSOR              : constant := 3;

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (List       : in out List_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=
        WS_TABSTOP or LBS_HASSTRINGS or LBS_NOTIFY or WS_VSCROLL or WS_HSCROLL;
   begin
      if Sort then
         Styles := Styles or LBS_SORT;
      end if;

      Create_Control (List,
                      Parent,
                      "LISTBOX",
                      "",
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (List);

      if Show then
         GWindows.List_Boxes.Show (List);
      end if;
   end Create;

   procedure Create
     (List       : in out Multiple_Selection_List_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=
        WS_TABSTOP or LBS_HASSTRINGS or LBS_NOTIFY or LBS_EXTENDEDSEL;
   begin
      if Sort then
         Styles := Styles or LBS_SORT;
      end if;

      Create_Control (List,
                      Parent,
                      "LISTBOX",
                      "",
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (List);

      if Show then
         GWindows.List_Boxes.Show (List);
      end if;
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add (List  : in out List_Box_Type;
                  Value : in     GString) is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Add;

   function Add (List  : in List_Box_Type;
                 Value : in GString)
                return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
        return Natural;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Add;

   procedure Add (List  : in out List_Box_Type;
                  After : in     Positive;
                  Value : in     GString)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_INSERTSTRING;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (After - 1);
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (List : in out List_Box_Type;
                     Item : in     Positive)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_DELETESTRING;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out List_Box_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_RESETCONTENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear;

   ----------
   -- Find --
   ----------

   function Find (List             : in List_Box_Type;
                  Value            : in GString;
                  Start_After_Item : in Natural       := 0)
                 return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_FINDSTRING;
         wParam : GWindows.Types.Wparam :=
            GWindows.Types.Wparam (Start_After_Item) - 1;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
         return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Find;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact (List             : in List_Box_Type;
                        Value            : in GString;
                        Start_After_Item : in Natural       := 0)
                       return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_FINDSTRINGEXACT;
         wParam : GWindows.Types.Wparam :=
            GWindows.Types.Wparam (Start_After_Item) - 1;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Find_Exact;

   -------------
   -- Current --
   -------------

   procedure Current (List : in List_Box_Type;
                      Item : in Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_SETCURSEL;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Current;

   function Current (List : in List_Box_Type) return Natural is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETCURSEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Current;

   ---------------
   -- Item_Data --
   ---------------

   procedure Item_Data (List : List_Box_Type;
                        Item : Natural;
                        Data : GWindows.Types.Lparam) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_SETITEMDATA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := Data);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Item_Data;

   function Item_Data (List : List_Box_Type;
                       Item : Natural) return GWindows.Types.Lparam is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETITEMDATA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lparam;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Item_Data;

   -----------
   -- Count --
   -----------

   function Count (List : in List_Box_Type) return Natural is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Count;

   ------------------
   -- Value_Length --
   ------------------

   function Value_Length (List : in List_Box_Type;
                          Item : in Positive)
                         return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETTEXTLEN;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Value_Length;

   -----------
   -- Value --
   -----------

   function Value (List : in List_Box_Type;
                   Item : in Positive)
                  return GString
   is
      use type Interfaces.C.size_t;

      Buffer : GString_C
        (1 .. Interfaces.C.size_t (Value_Length (List, Item)) + 1);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETTEXT;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : access GChar_C        := Buffer (Buffer'First)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
      return Interfaces.C.To_Ada (Buffer);
   end Value;

   --------------
   -- Selected --
   --------------

   procedure Selected (List  : in out List_Box_Type;
                       Item  : in     Positive;
                       State : in     Boolean       := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_SETCURSEL;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0 - 1);
      end if;
   end Selected;

   procedure Selected (List  : in out Multiple_Selection_List_Box_Type;
                       Item  : in     Positive;
                       State : in     Boolean       := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_SETSEL;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Item - 1));
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Selected;

   function Selected (List  : in List_Box_Type;
                      Item  : in Positive)
                     return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int        := LB_GETSEL;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage > 0;
   end Selected;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
     (List       : in out Multiple_Selection_List_Box_Type;
      Start_Item : in     Positive;
      End_Item   : in     Positive;
      State      : in     Boolean                          := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_SELITEMRANGEEX;
         wParam : GWindows.Types.Wparam :=
            GWindows.Types.Wparam (Start_Item - 1);
         lParam : GWindows.Types.Lparam :=
            GWindows.Types.Lparam (End_Item - 1));
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => GWindows.Types.Wparam  (End_Item - 1),
                      lParam => GWindows.Types.Lparam  (Start_Item - 1));
      end if;
   end Select_Range;

   ------------------
   -- Select_Count --
   ------------------

   function Select_Count
     (List : in Multiple_Selection_List_Box_Type)
     return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETSELCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Select_Count;

   --------------------
   -- Selected_Items --
   --------------------

   function Selected_Items
     (List : in Multiple_Selection_List_Box_Type)
     return Selection_Array_Type
   is
      Selection_Array : Selection_Array_Type (1 .. Select_Count (List)) :=
        (others => 0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle  := Handle (List);
         uMsg   : Interfaces.C.int       := LB_SELITEMRANGEEX;
         wParam : GWindows.Types.Wparam  :=
            GWindows.Types.Wparam  (Select_Count (List));
         lParam : access Integer         := Selection_Array (1)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
      return Selection_Array;
   end Selected_Items;

   --------------
   -- Top_Item --
   --------------

   procedure Top_Item (List  : in out List_Box_Type;
                       Item  : in     Positive)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle  := Handle (List);
         uMsg   : Interfaces.C.int       := LB_SETTOPINDEX;
         wParam : GWindows.Types.Wparam  := GWindows.Types.Wparam  (Item - 1);
         lParam : GWindows.Types.Lparam  := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Top_Item;

   function Top_Item (List  : in List_Box_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (List);
         uMsg   : Interfaces.C.int      := LB_GETTOPINDEX;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Top_Item;

   ----------
   -- Text --
   ----------

   procedure Text (Window : in out List_Box_Type;
                   Text   : in     GString)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := LB_SELECTSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Text;

   function Text  (Window : in List_Box_Type)
                  return GString
   is
      Selection : constant Natural := Current (Window);
   begin
      if Count (Window) > 0 and Selection > 0 then
         return Value (Window, Selection);
      else
         return "";
      end if;
   end Text;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (List : in out List_Box_Type) is
   begin
      Fire_On_Double_Click (List);
   end On_Double_Click;

   -------------------------
   -- On_Selection_Cancel --
   -------------------------

   procedure On_Selection_Cancel (List : in out List_Box_Type) is
   begin
      Fire_On_Selection_Cancel (List);
   end On_Selection_Cancel;

   -------------------------
   -- On_Selection_Change --
   -------------------------

   procedure On_Selection_Change (List : in out List_Box_Type) is
   begin
      Fire_On_Selection_Change (List);
   end On_Selection_Change;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (List : in out List_Box_Type) is
   begin
      Fire_On_Focus (List);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (List : in out List_Box_Type) is
   begin
      Fire_On_Lost_Focus (List);
   end On_Lost_Focus;

   ----------------------
   -- On_Out_Of_Memory --
   ----------------------

   procedure On_Out_Of_Memory (List : in out List_Box_Type) is
   begin
      Fire_On_Out_Of_Memory (List);
   end On_Out_Of_Memory;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out List_Box_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                            GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when LBN_SETFOCUS =>
            On_Focus (List_Box_Type'Class (Window));
         when LBN_KILLFOCUS =>
            On_Lost_Focus (List_Box_Type'Class (Window));
         when LBN_ERRSPACE =>
            On_Out_Of_Memory  (List_Box_Type'Class (Window));
         when LBN_DBLCLK =>
            On_Double_Click (List_Box_Type'Class (Window));
         when LBN_SELCANCEL =>
            On_Selection_Cancel (List_Box_Type'Class (Window));
         when LBN_SELCHANGE =>
            On_Selection_Change (List_Box_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   -----------------------------
   -- On_Double_Click_Handler --
   -----------------------------

   procedure On_Double_Click_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Double_Click_Event := Handler;
   end On_Double_Click_Handler;

   --------------------------
   -- Fire_On_Double_Click --
   --------------------------

   procedure Fire_On_Double_Click (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Double_Click_Event /= null then
         List.On_Double_Click_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Double_Click;

   ---------------------------------
   -- On_Selection_Cancel_Handler --
   ---------------------------------

   procedure On_Selection_Cancel_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Selection_Cancel_Event := Handler;
   end On_Selection_Cancel_Handler;

   ------------------------------
   -- Fire_On_Selection_Cancel --
   ------------------------------

   procedure Fire_On_Selection_Cancel (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Selection_Cancel_Event /= null then
         List.On_Selection_Cancel_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Selection_Cancel;

   ---------------------------------
   -- On_Selection_Change_Handler --
   ---------------------------------

   procedure On_Selection_Change_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Selection_Change_Event := Handler;
   end On_Selection_Change_Handler;

   ------------------------------
   -- Fire_On_Selection_Change --
   ------------------------------

   procedure Fire_On_Selection_Change (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Selection_Change_Event /= null then
         List.On_Selection_Change_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Selection_Change;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Focus_Event /= null then
         List.On_Focus_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Lost_Focus_Event /= null then
         List.On_Lost_Focus_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Lost_Focus;

   ------------------------------
   -- On_Out_Of_Memory_Handler --
   ------------------------------

   procedure On_Out_Of_Memory_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      List.On_Out_Of_Memory_Event := Handler;
   end On_Out_Of_Memory_Handler;

   ---------------------------
   -- Fire_On_Out_Of_Memory --
   ---------------------------

   procedure Fire_On_Out_Of_Memory (List : in out List_Box_Type)
   is
      use GWindows.Base;
   begin
      if List.On_Out_Of_Memory_Event /= null then
         List.On_Out_Of_Memory_Event (Base_Window_Type'Class (List));
      end if;
   end Fire_On_Out_Of_Memory;

--    type Drag_List_Box_Type is new List_Box_Type with private;

--    procedure On_Create (Window : in out Drag_List_Box_Type);
--    --  Handle creation of drag list box

--    procedure Filter_Message (Window       : in out Drag_List_Box_Type;
--                              message      : in     Interfaces.C.unsigned;
--                              wParam       : in     Interfaces.C.int;
--                              lParam       : in     Interfaces.C.int;
--                              Return_Value : in out Interfaces.C.long);
--    --  Must be called from the parent windows On_Message

--    ---------------
--    -- On_Create --
--    ---------------

--    procedure On_Create (Window : in out Drag_List_Box_Type)
--    is
--       procedure MakeDragList
--         (HWND : GWindows.Types.Handle := Handle (Window));
--       pragma Import (StdCall, MakeDragList, "MakeDragList");
--    begin
--       MakeDragList;
--    end On_Create;

--    procedure Filter_Message (Window       : in out Drag_List_Box_Type;
--                              message      : in     Interfaces.C.unsigned;
--                              wParam       : in     Interfaces.C.int;
--                              lParam       : in     Interfaces.C.int;
--                              Return_Value : in out Interfaces.C.long)
--    is
--    begin
--       if message > DL_BEGINDRAG and message < DL_CANCELDRAG then
--          case message is
--             when DL_BEGINDRAG =>
--                GWindows.Base.Message_Box ("Here", "Here");
--                Return_Value := 1;
--             when DL_DRAGGING =>
--                Return_Value := DL_MOVECURSOR;
--             when others =>
--                null;
--          end case;
--       end if;
--    end Filter_Message;

end GWindows.List_Boxes;
