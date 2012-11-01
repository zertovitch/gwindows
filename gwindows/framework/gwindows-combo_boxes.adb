------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . C O M B O _ B O X E S                  --
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

with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Errors;
with GWindows.GStrings;
with Interfaces.C;
package body GWindows.Combo_Boxes is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------
   CB_ERR          : constant Interfaces.C.int := -1;
   CB_ERRSPACE     : constant Interfaces.C.int := -2;

   CBN_ERRSPACE               : constant := -1;
   CBN_SELCHANGE              : constant := 1;
   CBN_DBLCLK                 : constant := 2;
   CBN_SETFOCUS               : constant := 3;
   CBN_KILLFOCUS              : constant := 4;
   CBN_EDITCHANGE             : constant := 5;
   CBN_EDITUPDATE             : constant := 6;
   CBN_DROPDOWN               : constant := 7;
   CBN_CLOSEUP                : constant := 8;
   CBN_SELENDOK               : constant := 9;
   CBN_SELENDCANCEL           : constant := 10;
   CBS_SIMPLE                 : constant := 1;
   CBS_DROPDOWN               : constant := 2;
   CBS_DROPDOWNLIST           : constant := 3;
--     CBS_OWNERDRAWFIXED         : constant := 16;
--     CBS_OWNERDRAWVARIABLE      : constant := 32;
   CBS_AUTOHSCROLL            : constant := 64;
--     CBS_OEMCONVERT             : constant := 128;
   CBS_SORT                   : constant := 256;
   CBS_HASSTRINGS             : constant := 512;
--     CBS_NOINTEGRALHEIGHT       : constant := 1024;
--     CBS_DISABLENOSCROLL        : constant := 2048;
--     CBS_UPPERCASE              : constant := 8192;
--     CBS_LOWERCASE              : constant := 16384;
   CB_GETEDITSEL              : constant := 320;
   CB_LIMITTEXT               : constant := 321;
   CB_SETEDITSEL              : constant := 322;
   CB_ADDSTRING               : constant := 323;
   CB_DELETESTRING            : constant := 324;
--     CB_DIR                     : constant := 325;
   CB_GETCOUNT                : constant := 326;
   CB_GETCURSEL               : constant := 327;
   CB_GETLBTEXT               : constant := 328;
   CB_GETLBTEXTLEN            : constant := 329;
   CB_INSERTSTRING            : constant := 330;
   CB_RESETCONTENT            : constant := 331;
   CB_FINDSTRING              : constant := 332;
   CB_SELECTSTRING            : constant := 333;
   CB_SETCURSEL               : constant := 334;
   CB_SHOWDROPDOWN            : constant := 335;
   CB_GETITEMDATA             : constant := 336;
   CB_SETITEMDATA             : constant := 337;
--     CB_GETDROPPEDCONTROLRECT   : constant := 338;
--     CB_SETITEMHEIGHT           : constant := 339;
--     CB_GETITEMHEIGHT           : constant := 340;
--     CB_SETEXTENDEDUI           : constant := 341;
--     CB_GETEXTENDEDUI           : constant := 342;
   CB_GETDROPPEDSTATE         : constant := 343;
   CB_FINDSTRINGEXACT         : constant := 344;
--     CB_SETLOCALE               : constant := 345;
--     CB_GETLOCALE               : constant := 346;
   CB_GETTOPINDEX             : constant := 347;
   CB_SETTOPINDEX             : constant := 348;
--     CB_GETHORIZONTALEXTENT     : constant := 349;
--     CB_SETHORIZONTALEXTENT     : constant := 350;
--     CB_GETDROPPEDWIDTH         : constant := 351;
--     CB_SETDROPPEDWIDTH         : constant := 352;
--     CB_INITSTORAGE             : constant := 353;
   WS_TABSTOP                 : constant := 65536;
   WS_VSCROLL                 : constant := 2097152;

   WM_CUT                     : constant := 768;
   WM_COPY                    : constant := 769;
   WM_PASTE                   : constant := 770;
--     WM_CLEAR                   : constant := 771;
   WM_UNDO                    : constant := 772;

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Combo      : in out Combo_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=
        WS_TABSTOP or CBS_HASSTRINGS or
        WS_VSCROLL or CBS_SIMPLE or CBS_AUTOHSCROLL;
   begin
      if Sort then
         Styles := Styles or CBS_SORT;
      end if;

      Create_Control (Combo,
                      Parent,
                      "COMBOBOX",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Combo);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Combo, New_Size);
         end;
      end if;

      if Show then
         GWindows.Combo_Boxes.Show (Combo);
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Combo      : in out Drop_Down_Combo_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                            := 0;
      Top        : in     Integer                            := 0;
      Width      : in     Integer                            := 0;
      Height     : in     Integer                            := 0;
      Sort       : in     Boolean                            := True;
      ID         : in     Integer                            := 0;
      Show       : in     Boolean                            := True;
      Is_Dynamic : in     Boolean                            := False)
   is
      Styles : Interfaces.C.unsigned :=
        WS_TABSTOP or CBS_HASSTRINGS or CBS_DROPDOWN or
        WS_VSCROLL or CBS_AUTOHSCROLL;
   begin
      if Sort then
         Styles := Styles or CBS_SORT;
      end if;

      Create_Control (Combo,
                      Parent,
                      "COMBOBOX",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Combo);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Combo, New_Size);
         end;
      end if;

      if Show then
         GWindows.Combo_Boxes.Show (Combo);
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Combo      : in out Drop_Down_List_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=
        WS_TABSTOP or CBS_HASSTRINGS or CBS_DROPDOWNLIST or
        WS_VSCROLL;
   begin
      if Sort then
         Styles := Styles or CBS_SORT;
      end if;

      Create_Control (Combo,
                      Parent,
                      "COMBOBOX",
                      "",
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Combo);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Combo, Width => New_Size.Width, Height => New_Size.Height);
         end;
      end if;

      if Show then
         GWindows.Combo_Boxes.Show (Combo);
      end if;
   end Create;

   ----------------
   -- Text_Limit --
   ----------------

   procedure Text_Limit (Combo : in out Combo_Box_Type;
                         Size  : in Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int  := CB_LIMITTEXT;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Size);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Text_Limit;

   -------------
   -- Dropped --
   -------------

   procedure Dropped (Combo : in out Combo_Box_Type;
                      State : in     Boolean := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int  := CB_SHOWDROPDOWN;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Dropped;

   function Dropped (Combo : in Combo_Box_Type)
                    return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle   := Handle (Combo);
         uMsg   : Interfaces.C.int        := CB_GETDROPPEDSTATE;
         wParam : GWindows.Types.Wparam   := 0;
         lParam : GWindows.Types.Lparam   := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage /= 0;
   end Dropped;

   ------------------------
   -- Get_Edit_Selection --
   ------------------------

   procedure Get_Edit_Selection
     (Combo          : in out Combo_Box_Type;
      Start_Position :    out Natural;
      End_Position   :    out Natural)
   is
      Start_Pos : aliased Natural := 0;
      End_Pos   : aliased Natural := 0;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETEDITSEL;
         wParam : access Natural        := Start_Pos'Access;
         lParam : access Natural        := End_Pos'Access);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
      Start_Position := Start_Pos;
      End_Position := End_Pos;
   end Get_Edit_Selection;

   ------------------------
   -- Set_Edit_Selection --
   ------------------------

   procedure Set_Edit_Selection
     (Combo          : in out Combo_Box_Type;
      Start_Position : in     Integer;
      End_Position   : in     Integer)
   is
      procedure SendMessage
        (hwnd    : GWindows.Types.Handle  := Handle (Combo);
         uMsg    : Interfaces.C.int       := CB_SETEDITSEL;
         wParam  : GWindows.Types.Wparam  := 0;
         lParam  : GWindows.Types.Lparam);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);

      DWord : GWindows.Types.Lparam;
   begin
      DWord := GWindows.Types.Lparam ((End_Position * 2**16) + Start_Position);
      SendMessage (lParam => DWord);
   end Set_Edit_Selection;

   ------------------------------
   -- Alternate_User_Interface --
   ------------------------------

   procedure Alternate_User_Interface (Combo  : in out Combo_Box_Type;
                                       State  : in     Boolean := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Alternate_User_Interface;

   function Alternate_User_Interface (Combo : in Combo_Box_Type)
                                     return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_FINDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage /= 0;
   end Alternate_User_Interface;

   ---------
   -- Add --
   ---------

   procedure Add (Combo : in out Combo_Box_Type;
                  Value : in     GString)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Add;

   function Add (Combo : in Combo_Box_Type;
                 Value : in GString)
                return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
        return Natural;
      pragma Import (StdCall, SendMessage,
                        "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Add;

   procedure Add (Combo : in out Combo_Box_Type;
                  Value : in     GString;
                  Index :    out Natural)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_ADDSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
      return Interfaces.C.int;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
      Status : constant Interfaces.C.int := SendMessage;
   begin
      case Status is
      when CB_ERR | CB_ERRSPACE =>
         GWindows.Errors.Error_Check (Integer (Status));
      when others =>
         Index := Natural (Status) + 1;
      end case;
   end Add;

   procedure Add (Combo : in out Combo_Box_Type;
                  After : in     Positive;
                  Value : in     GString)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_INSERTSTRING;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (After - 1);
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (Combo : in out Combo_Box_Type;
                     Item  : in     Positive)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_DELETESTRING;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Delete;

   ---------
   -- Cut --
   ---------

   procedure Cut (Combo : in out Combo_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := WM_CUT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Cut;

   ----------
   -- Copy --
   ----------

   procedure Copy (Combo : in out Combo_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := WM_COPY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Copy;

   -----------
   -- Paste --
   -----------

   procedure Paste (Combo : in out Combo_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := WM_PASTE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Paste;

   -----------
   -- Clear --
   -----------

   procedure Clear (Combo : in out Combo_Box_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_RESETCONTENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear;

   ----------
   -- Undo --
   ----------

   procedure Undo (Combo : in out Combo_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int  := WM_UNDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Undo;

   ----------
   -- Find --
   ----------

   function Find (Combo            : Combo_Box_Type;
                  Value            : GString;
                  Start_After_Item : Natural := 0) return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);
      use GWindows.Types;

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_FINDSTRING;
         wParam : GWindows.Types.Wparam :=
           GWindows.Types.Wparam (Start_After_Item) - 1;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Find;

   ----------------
   -- Find_Exact --
   ----------------

   function Find_Exact (Combo            : Combo_Box_Type;
                        Value            : GString;
                        Start_After_Item : Natural        := 0)
                       return Natural
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Value);
      use GWindows.Types;

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_FINDSTRINGEXACT;
         wParam : GWindows.Types.Wparam :=
           GWindows.Types.Wparam (Start_After_Item) - 1;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Find_Exact;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Combo : in Combo_Box_Type)
                             return GWindows.Types.Size_Type
   is
      use GWindows.Types;

      Text_Size : Size_Type := (0, 0);
      Extra     : Size_Type := (12, 6); --  white space around text, borders.
      Canvas    : GWindows.Drawing.Canvas_Type;
      Font      : GWindows.Drawing_Objects.Font_Type;
   begin
      Get_Canvas (Combo, Canvas);
      Get_Font (Combo, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);

      for I in 1 .. Count (Combo) loop
         Text_Size := Max
           (Text_Size,
            GWindows.Drawing.Text_Output_Size (Canvas, Value (Combo, I)));
      end loop;

      Extra := Extra + (Text_Size.Height, 0); --  Drop arrow

      return Calculate_New_Window_Size (Combo, Text_Size + Extra);
   end Recommended_Size;

   -------------
   -- Current --
   -------------
   procedure Current (Combo : in Combo_Box_Type;
                      Item : in Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_SETCURSEL;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Current;

   function Current (Combo : in Combo_Box_Type) return Natural is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETCURSEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Current;

   ---------------
   -- Item_Data --
   ---------------

   procedure Item_Data (Combo : in Combo_Box_Type;
                        Item : Natural;
                        Data : GWindows.Types.Lparam) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_SETITEMDATA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := Data);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Item_Data;

   function Item_Data (Combo : in Combo_Box_Type;
                       Item : Natural) return GWindows.Types.Lparam is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETITEMDATA;
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

   function Count (Combo : in Combo_Box_Type) return Natural is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETCOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Count;

   ------------------
   -- Value_Length --
   ------------------

   function Value_Length (Combo : in Combo_Box_Type;
                          Item  : in Positive)
                         return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETLBTEXTLEN;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam := 0)
        return Natural;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Value_Length;

   -----------
   -- Value --
   -----------

   function Value (Combo : in Combo_Box_Type;
                   Item  : in Positive)
                  return GString
   is
      use type Interfaces.C.size_t;

      Buffer : GString_C
        (1 .. Interfaces.C.size_t (Value_Length (Combo, Item)) + 1) :=
        (others => GString_C_Null);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETLBTEXT;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Item - 1);
         lParam : out GString_C);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage (lParam => Buffer);
      return GWindows.GStrings.To_GString_From_C (Buffer);
   end Value;

   --------------
   -- Top_Item --
   --------------

   procedure Top_Item (Combo : in out Combo_Box_Type;
                       Item  : in     Positive)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle   := Handle (Combo);
         uMsg   : Interfaces.C.int        := CB_SETTOPINDEX;
         wParam : GWindows.Types.Wparam   := GWindows.Types.Wparam (Item - 1);
         lParam : GWindows.Types.Lparam   := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Top_Item;

   function Top_Item (Combo  : in Combo_Box_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETTOPINDEX;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage + 1;
   end Top_Item;

   ----------
   -- Text --
   ----------

   procedure Text (Window : in out Drop_Down_List_Box_Type;
                   Text   : in     GString)
   is
      C_Value : GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int      := CB_SELECTSTRING;
         wParam : GWindows.Types.Wparam := 0;
         lParam : access GChar_C        := C_Value (C_Value'First)'Access);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Text;

   function Text  (Window : in Drop_Down_List_Box_Type)
                  return GString
   is
      Selection : constant Natural := Current (Window);
   begin
      if Selection > 0 then
         return Value (Window, Selection);
      else
         return "";
      end if;
   end Text;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Double_Click (Combo);
   end On_Double_Click;

   -------------------------
   -- On_Selection_Change --
   -------------------------

   procedure On_Selection_Change (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Selection_Change (Combo);
   end On_Selection_Change;

   --------------------
   -- On_Edit_Change --
   --------------------

   procedure On_Edit_Change (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Edit_Change (Combo);
   end On_Edit_Change;

   --------------------
   -- On_Edit_Update --
   --------------------

   procedure On_Edit_Update (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Edit_Update (Combo);
   end On_Edit_Update;

   ------------------
   -- On_Drop_Down --
   ------------------

   procedure On_Drop_Down (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Drop_Down (Combo);
   end On_Drop_Down;

   ------------------
   -- On_Close_Up --
   ------------------

   procedure On_Close_Up (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Close_Up (Combo);
   end On_Close_Up;

   ----------------------
   -- On_Select_End_OK --
   ----------------------

   procedure On_Select_End_OK (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Select_End_OK (Combo);
   end On_Select_End_OK;

   --------------------------
   -- On_Select_End_Cancel --
   --------------------------

   procedure On_Select_End_Cancel (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Select_End_Cancel (Combo);
   end On_Select_End_Cancel;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Focus (Combo);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Lost_Focus (Combo);
   end On_Lost_Focus;

   ----------------------
   -- On_Out_Of_Memory --
   ----------------------

   procedure On_Out_Of_Memory (Combo : in out Combo_Box_Type) is
   begin
      Fire_On_Out_Of_Memory (Combo);
   end On_Out_Of_Memory;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out Combo_Box_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                            GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when CBN_SETFOCUS =>
            On_Focus (Combo_Box_Type'Class (Window));
         when CBN_KILLFOCUS =>
            On_Lost_Focus (Combo_Box_Type'Class (Window));
         when CBN_ERRSPACE =>
            On_Out_Of_Memory  (Combo_Box_Type'Class (Window));
         when CBN_DBLCLK =>
            On_Double_Click (Combo_Box_Type'Class (Window));
         when CBN_SELCHANGE =>
            On_Selection_Change (Combo_Box_Type'Class (Window));
         when CBN_EDITCHANGE =>
            On_Edit_Change (Combo_Box_Type'Class (Window));
         when CBN_EDITUPDATE =>
            On_Edit_Update (Combo_Box_Type'Class (Window));
         when CBN_DROPDOWN =>
            On_Drop_Down (Combo_Box_Type'Class (Window));
         when CBN_CLOSEUP =>
            On_Close_Up (Combo_Box_Type'Class (Window));
         when CBN_SELENDOK =>
            On_Select_End_OK (Combo_Box_Type'Class (Window));
         when CBN_SELENDCANCEL =>
            On_Select_End_Cancel (Combo_Box_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   -----------------------------
   -- On_Double_Click_Handler --
   -----------------------------

   procedure On_Double_Click_Handler
     (Combo   : in out Combo_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Double_Click_Event := Handler;
   end On_Double_Click_Handler;

   --------------------------
   -- Fire_On_Double_Click --
   --------------------------

   procedure Fire_On_Double_Click (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Double_Click_Event /= null then
         Combo.On_Double_Click_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Double_Click;

   ---------------------------------
   -- On_Selection_Change_Handler --
   ---------------------------------

   procedure On_Selection_Change_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Selection_Change_Event := Handler;
   end On_Selection_Change_Handler;

   ------------------------------
   -- Fire_On_Selection_Change --
   ------------------------------

   procedure Fire_On_Selection_Change (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Selection_Change_Event /= null then
         Combo.On_Selection_Change_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Selection_Change;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Focus_Event /= null then
         Combo.On_Focus_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Combo   : in out Combo_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Lost_Focus_Event /= null then
         Combo.On_Lost_Focus_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Lost_Focus;

   ------------------------------
   -- On_Out_Of_Memory_Handler --
   ------------------------------

   procedure On_Out_Of_Memory_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Out_Of_Memory_Event := Handler;
   end On_Out_Of_Memory_Handler;

   ---------------------------
   -- Fire_On_Out_Of_Memory --
   ---------------------------

   procedure Fire_On_Out_Of_Memory (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Out_Of_Memory_Event /= null then
         Combo.On_Out_Of_Memory_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Out_Of_Memory;

   ----------------------------
   -- On_Edit_Change_Handler --
   ----------------------------

   procedure On_Edit_Change_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Edit_Change_Event := Handler;
   end On_Edit_Change_Handler;

   -------------------------
   -- Fire_On_Edit_Change --
   -------------------------

   procedure Fire_On_Edit_Change (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Edit_Change_Event /= null then
         Combo.On_Edit_Change_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Edit_Change;

   ----------------------------
   -- On_Edit_Update_Handler --
   ----------------------------

   procedure On_Edit_Update_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Edit_Update_Event := Handler;
   end On_Edit_Update_Handler;

   -------------------------
   -- Fire_On_Edit_Update --
   -------------------------

   procedure Fire_On_Edit_Update (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Edit_Update_Event /= null then
         Combo.On_Edit_Update_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Edit_Update;

   --------------------------
   -- On_Drop_Down_Handler --
   --------------------------

   procedure On_Drop_Down_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Drop_Down_Event := Handler;
   end On_Drop_Down_Handler;

   -----------------------
   -- Fire_On_Drop_Down --
   -----------------------

   procedure Fire_On_Drop_Down (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Drop_Down_Event /= null then
         Combo.On_Drop_Down_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Drop_Down;

   -------------------------
   -- On_Close_Up_Handler --
   -------------------------

   procedure On_Close_Up_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Close_Up_Event := Handler;
   end On_Close_Up_Handler;

   ----------------------
   -- Fire_On_Close_Up --
   ----------------------

   procedure Fire_On_Close_Up (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Close_Up_Event /= null then
         Combo.On_Close_Up_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Close_Up;

   ------------------------------
   -- On_Select_End_OK_Handler --
   ------------------------------

   procedure On_Select_End_OK_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Select_End_OK_Event := Handler;
   end On_Select_End_OK_Handler;

   ---------------------------
   -- Fire_On_Select_End_OK --
   ---------------------------

   procedure Fire_On_Select_End_OK (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Select_End_OK_Event /= null then
         Combo.On_Select_End_OK_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Select_End_OK;

   ----------------------------------
   -- On_Select_End_Cancel_Handler --
   ----------------------------------

   procedure On_Select_End_Cancel_Handler (Combo   : in out Combo_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Combo.On_Select_End_Cancel_Event := Handler;
   end On_Select_End_Cancel_Handler;

   -------------------------------
   -- Fire_On_Select_End_Cancel --
   -------------------------------

   procedure Fire_On_Select_End_Cancel (Combo : in out Combo_Box_Type)
   is
      use GWindows.Base;
   begin
      if Combo.On_Select_End_Cancel_Event /= null then
         Combo.On_Select_End_Cancel_Event (Base_Window_Type'Class (Combo));
      end if;
   end Fire_On_Select_End_Cancel;

end GWindows.Combo_Boxes;
