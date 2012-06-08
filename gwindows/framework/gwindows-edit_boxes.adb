------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . E D I T _ B O X E S                    --
--                                                                          --
--                                 S p e c                                  --
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
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;
with Interfaces.C;
with GWindows.Types; use GWindows.Types;

package body GWindows.Edit_Boxes is
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   GWL_STYLE : constant := -16;

   procedure SetWindowLong
     (hwnd    : GWindows.Types.Handle;
      nIndex  : Interfaces.C.int := GWL_STYLE;
      newLong : Interfaces.C.unsigned);
   pragma Import (StdCall, SetWindowLong, "SetWindowLong"
                    & Character_Mode_Identifier);

   function GetWindowLong
     (hwnd   : GWindows.Types.Handle;
      nIndex : Interfaces.C.int := GWL_STYLE)
     return Interfaces.C.unsigned;
   pragma Import (StdCall, GetWindowLong, "GetWindowLong"
                    & Character_Mode_Identifier);

   SWP_NOSIZE                 : constant := 1;
   SWP_NOMOVE                 : constant := 2;
   SWP_NOZORDER               : constant := 4;
   SWP_FRAMECHANGED           : constant := 32;

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

   ------------
   -- Create --
   ------------

   procedure Create
     (Edit              : in out Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                            := 0;
      Top               : in     Integer                            := 0;
      Width             : in     Integer                            := 0;
      Height            : in     Integer                            := 0;
      Horizontal_Scroll : in     Boolean                            := True;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
      Styles : Interfaces.C.unsigned := WS_TABSTOP;
   begin
      if Horizontal_Scroll then
         Styles := Styles or ES_AUTOHSCROLL;
      end if;

      if Read_Only then
         Styles := Styles or ES_READONLY;
      end if;

      Create_Control (Edit,
                      Parent,
                      "Edit",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (Edit);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Edit);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Edit, New_Size);
         end;
      end if;

      if Show then
         GWindows.Edit_Boxes.Show (Edit);
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                              := 0;
      Top               : in     Integer                              := 0;
      Width             : in     Integer                              := 0;
      Height            : in     Integer                              := 0;
      Horizontal_Scroll : in     Boolean                            := False;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
   begin
      Create_Multi_Line
        (Edit, Parent, Text, Left, Top, Width, Height, Horizontal_Scroll,
         True, True, Read_Only, ID, Show, Is_Dynamic);
   end Create;

   -----------------------
   -- Create_Multi_Line --
   -----------------------

   procedure Create_Multi_Line
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                              := 0;
      Top               : in     Integer                              := 0;
      Width             : in     Integer                              := 0;
      Height            : in     Integer                              := 0;
      Horizontal_Scroll : in     Boolean                            := False;
      Vertical_Scroll   : in     Boolean                            := True;
      Capture_Return    : in     Boolean                            := True;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
      Styles : Interfaces.C.unsigned := ES_MULTILINE or WS_TABSTOP;
   begin
      if Capture_Return then
         Styles := Styles or ES_WANTRETURN;
      end if;

      if Vertical_Scroll then
         Styles := Styles or ES_AUTOVSCROLL or WS_VSCROLL;
      end if;

      if Horizontal_Scroll then
         Styles := Styles or ES_AUTOHSCROLL or WS_HSCROLL;
      end if;

      if Read_Only then
         Styles := Styles or ES_READONLY;
      end if;

      Create_Control (Edit,
                      Parent,
                      "Edit",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (Edit);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Edit);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Edit, New_Size);
         end;
      end if;

      if Show then
         GWindows.Edit_Boxes.Show (Edit);
      end if;
   end Create_Multi_Line;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Edit : in Edit_Box_Type)
                             return GWindows.Types.Size_Type
   is

      Text_Size : Size_Type;
      Extra     : constant Size_Type := (9, 4);
      --  white space around text, borders.

      Canvas       : GWindows.Drawing.Canvas_Type;
      Font         : GWindows.Drawing_Objects.Font_Type;
      Current_Text : constant GString := Text (Edit);
   begin
      Get_Canvas (Edit, Canvas);
      Get_Font (Edit, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);

      if Current_Text'Length = 0 then
         --  Recommend a minimum size
         Text_Size := GWindows.Drawing.Text_Output_Size (Canvas, "Hello");
      else
         Text_Size := GWindows.Drawing.Text_Output_Size (Canvas, Current_Text);
      end if;

      return Calculate_New_Window_Size (Edit, Text_Size + Extra);
   end Recommended_Size;

   ---------------------
   -- Lower_Case_Only --
   ---------------------

   procedure Lower_Case_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True)
   is
   begin
      if State then
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) or
                          ES_LOWERCASE);
         SetWindowPos (Handle (Window));
      else
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) and not
                          ES_LOWERCASE);
         SetWindowPos (Handle (Window));
      end if;
   end Lower_Case_Only;

   function Lower_Case_Only (Window : in Edit_Box_Type) return Boolean is
   begin
      return (GetWindowLong (Handle (Window)) and ES_LOWERCASE) = ES_LOWERCASE;
   end Lower_Case_Only;

   ---------------------
   -- Upper_Case_Only --
   ---------------------

   procedure Upper_Case_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True)
   is
   begin
      if State then
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) or
                          ES_UPPERCASE);
         SetWindowPos (Handle (Window));
      else
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) and not
                          ES_UPPERCASE);
         SetWindowPos (Handle (Window));
      end if;
   end Upper_Case_Only;

   function Upper_Case_Only (Window : in Edit_Box_Type) return Boolean is
   begin
      return (GetWindowLong (Handle (Window)) and ES_UPPERCASE) = ES_UPPERCASE;
   end Upper_Case_Only;

   -----------------
   -- Digits_Only --
   -----------------

   procedure Digits_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True)
   is
   begin
      if State then
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) or
                           ES_NUMBER);
         SetWindowPos (Handle (Window));
      else
         SetWindowLong (Handle (Window),
                        newLong =>
                          GetWindowLong (Handle (Window)) and not
                          ES_NUMBER);
         SetWindowPos (Handle (Window));
      end if;
   end Digits_Only;

   function Digits_Only (Window : in Edit_Box_Type) return Boolean is
   begin
      return (GetWindowLong (Handle (Window)) and ES_NUMBER) = ES_NUMBER;
   end Digits_Only;

   -----------------
   -- Read_Only --
   -----------------

   procedure Read_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := EM_SETREADONLY;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Read_Only;

   function Read_Only (Window : in Edit_Box_Type) return Boolean is
   begin
      return (GetWindowLong (Handle (Window)) and ES_READONLY) = ES_READONLY;
   end Read_Only;

   --------------
   -- Password --
   --------------

   procedure Password (Window        : in out Edit_Box_Type;
                       Password_Char : in     Character     := '*')
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := EM_SETPASSWORDCHAR;
         wParam : GWindows.Types.Wparam := Character'Pos (Password_Char);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Password;

   function Password (Window : in Edit_Box_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Window);
         uMsg   : Interfaces.C.int  := EM_GETPASSWORDCHAR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage) /= 0;
   end Password;

   ---------
   -- Cut --
   ---------

   procedure Cut (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := WM_CUT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Cut;

   ----------
   -- Copy --
   ----------

   procedure Copy (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := WM_COPY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Copy;

   -----------
   -- Paste --
   -----------

   procedure Paste (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := WM_PASTE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Paste;

   -----------
   -- Clear --
   -----------

   procedure Clear (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := WM_CLEAR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear;

   ----------
   -- Undo --
   ----------

   procedure Undo (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_UNDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Undo;

   ----------------
   -- Clear_Undo --
   ----------------

   procedure Clear_Undo (Edit : in out Edit_Box_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_EMPTYUNDOBUFFER;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear_Undo;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo (Edit : in Edit_Box_Type) return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_CANUNDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage /= 0;
   end Can_Undo;

   -------------------
   -- First_Visible --
   -------------------

   function First_Visible (Edit : in Edit_Box_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETFIRSTVISIBLELINE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end First_Visible;

   ----------------
   -- Text_Limit --
   ----------------

   procedure Text_Limit (Edit : in out Edit_Box_Type;
                         Size : in     Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_LIMITTEXT;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Size);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Text_Limit;

   function Text_Limit (Edit : in Edit_Box_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETLIMITTEXT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end Text_Limit;

   ----------------
   -- Line_Count --
   ----------------

   function Line_Count (Edit : Multi_Line_Edit_Box_Type) return Natural
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETLINECOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end Line_Count;

   --------------
   -- Modified --
   --------------

   procedure Modified (Edit  : in out Edit_Box_Type;
                       State : in     Boolean       := True)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_SETMODIFY;
         wParam : GWindows.Types.Wparam := 1;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if State then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Modified;

   function Modified (Edit : in Edit_Box_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETMODIFY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return SendMessage /= 0;
   end Modified;

   -------------------
   -- Get_Selection --
   -------------------

   procedure Get_Selection
     (Edit           : in out Edit_Box_Type;
      Start_Position :    out Natural;
      End_Position   :    out Natural)
   is
      Start_Pos : aliased Natural := 0;
      End_Pos   : aliased Natural := 0;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETSEL;
         wParam : access Natural    := Start_Pos'Access;
         lParam : access Natural    := End_Pos'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
      Start_Position := Start_Pos;
      End_Position := End_Pos;
   end Get_Selection;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Edit           : in out Edit_Box_Type;
      Start_Position : in     Integer;
      End_Position   : in     Integer)
   is
      procedure SendMessage
        (hwnd    : GWindows.Types.Handle        := Handle (Edit);
         uMsg    : Interfaces.C.int             := EM_SETSEL;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);

   begin
      SendMessage (wParam => To_Wparam (Start_Position),
                   lParam => To_Lparam (End_Position));
   end Set_Selection;

   -----------------------
   -- Replace_Selection --
   -----------------------

   procedure Replace_Selection
     (Edit     : in out Edit_Box_Type;
      Text     : in     GString;
      Can_Undo : in     Boolean       := True)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure SendMessage
        (hwnd   : in     GWindows.Types.Handle := Handle (Edit);
         uMsg   : in     Interfaces.C.int  := EM_REPLACESEL;
         wParam : in     GWindows.Types.Wparam := 1;
         lParam : access GChar_C           := C_Text (C_Text'First)'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if Can_Undo then
         SendMessage;
      else
         SendMessage (wParam => 0);
      end if;
   end Replace_Selection;

   ------------------------
   -- Line_From_Position --
   ------------------------

   function Line_From_Position (Edit     : Multi_Line_Edit_Box_Type;
                                Position : Natural)
                               return Positive
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_LINEFROMCHAR;
         wParam : GWindows.Types.Wparam := To_Wparam ((Position) - 1);
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage) + 1;
   end Line_From_Position;

   ------------------------
   -- Position_From_Line --
   ------------------------

   function Position_From_Line (Edit : Multi_Line_Edit_Box_Type;
                                Line : Natural)
                               return Positive
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_LINEINDEX;
         wParam : GWindows.Types.Wparam := To_Wparam (Line - 1);
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage) + 1;
   end Position_From_Line;

   ---------------
   -- Line_Text --
   ---------------

   function Line_Text (Edit : Multi_Line_Edit_Box_Type;
                       Line : Positive)
                      return GString
   is
      use Interfaces.C;
      type Pointer_To_Integer is access all Integer;

      function To_PINT is new Ada.Unchecked_Conversion (Pointer_To_GChar_C,
                                                        Pointer_To_Integer);

      Length : constant Integer := Line_Length (Edit, Line);
   begin
      if Length = 0 then
         return "";
      else
         declare
            C_Text : GString_C
              (0 .. Interfaces.C.size_t
               (Length)) := (others => GString_C_Null);

            function SendMessage
              (hwnd   : GWindows.Types.Handle := Handle (Edit);
               uMsg   : Interfaces.C.int  := EM_GETLINE;
               wParam : GWindows.Types.Wparam := To_Wparam (Line - 1);
               lParam : GString_C         := C_Text)
              return GWindows.Types.Lresult;
            pragma Import (StdCall, SendMessage, "SendMessage"
                             & Character_Mode_Identifier);
         begin
            To_PINT (C_Text (0)'Unchecked_Access).all := Length;
            if SendMessage < 1 then
               return "";
            else
               return Interfaces.C.To_Ada (C_Text);
            end if;
         end;
      end if;
   end Line_Text;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (Edit : Multi_Line_Edit_Box_Type;
                         Line : Positive)
                        return Natural
   is
      Position : constant Integer := Position_From_Line (Edit, Line);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_LINELENGTH;
         wParam : GWindows.Types.Wparam := To_Wparam (Position);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      if Position = 0 then
         return 0;
      end if;

      return To_Integer (SendMessage) + 1;
   end Line_Length;

   ---------------------
   -- Scroll_Position --
   ---------------------

   function Scroll_Position (Edit : in Multi_Line_Edit_Box_Type)
                            return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int  := EM_GETTHUMB;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
      return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end Scroll_Position;

   ------------
   -- Scroll --
   ------------

   procedure Scroll (Edit    : in out Multi_Line_Edit_Box_Type;
                     Command : in     Scroll_Command_Type)
   is
      procedure SendMessage
        (hwnd    : GWindows.Types.Handle   := Handle (Edit);
         uMsg    : Interfaces.C.int        := EM_SETSEL;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam   := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      case Command is
         when Line_Up =>
            SendMessage (wParam => SB_LINEUP);
         when Line_Down =>
            SendMessage (wParam => SB_LINEDOWN);
         when Page_Up =>
            SendMessage (wParam => SB_PAGEUP);
         when Page_Down =>
            SendMessage (wParam => SB_PAGEDOWN);
      end case;
   end Scroll;

   ---------------------
   -- Scroll_To_Caret --
   ---------------------

   procedure Scroll_To_Caret (Edit : in out Multi_Line_Edit_Box_Type) is
      procedure SendMessage
        (hwnd    : GWindows.Types.Handle := Handle (Edit);
         uMsg    : Interfaces.C.int      := EM_SCROLLCARET;
         wParam  : GWindows.Types.Wparam := 0;
         lParam  : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
   end Scroll_To_Caret;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Focus (Edit);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Lost_Focus (Edit);
   end On_Lost_Focus;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Change (Edit);
   end On_Change;

   -----------------
   -- On_Max_Text --
   -----------------

   procedure On_Max_Text (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Max_Text (Edit);
   end On_Max_Text;

   --------------------------
   -- On_Horizontal_Scroll --
   --------------------------

   procedure On_Horizontal_Scroll (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Horizontal_Scroll (Edit);
   end On_Horizontal_Scroll;

   ------------------------
   -- On_Vertical_Scroll --
   ------------------------

   procedure On_Vertical_Scroll (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Vertical_Scroll (Edit);
   end On_Vertical_Scroll;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Update (Edit);
   end On_Update;

   ----------------------
   -- On_Out_Of_Memory --
   ----------------------

   procedure On_Out_Of_Memory (Edit : in out Edit_Box_Type) is
   begin
      Fire_On_Out_Of_Memory (Edit);
   end On_Out_Of_Memory;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out Edit_Box_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when EN_SETFOCUS =>
            On_Focus (Edit_Box_Type'Class (Window));
         when EN_KILLFOCUS =>
            On_Lost_Focus (Edit_Box_Type'Class (Window));
         when EN_CHANGE =>
            On_Change (Edit_Box_Type'Class (Window));
         when EN_UPDATE =>
            On_Update (Edit_Box_Type'Class (Window));
         when EN_MAXTEXT =>
            On_Max_Text (Edit_Box_Type'Class (Window));
         when EN_ERRSPACE =>
            On_Out_Of_Memory  (Edit_Box_Type'Class (Window));
         when EN_HSCROLL =>
            On_Horizontal_Scroll (Edit_Box_Type'Class (Window));
         when EN_VSCROLL =>
            On_Vertical_Scroll (Edit_Box_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Focus_Event /= null then
         Edit.On_Focus_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Edit   : in out Edit_Box_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Lost_Focus_Event /= null then
         Edit.On_Lost_Focus_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Lost_Focus;

   -----------------------
   -- On_Change_Handler --
   -----------------------

   procedure On_Change_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Change_Event := Handler;
   end On_Change_Handler;

   --------------------
   -- Fire_On_Change --
   --------------------

   procedure Fire_On_Change (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Change_Event /= null then
         Edit.On_Change_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Change;

   --------------------------
   -- On_Max_Text_Handler --
   --------------------------

   procedure On_Max_Text_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Max_Text_Event := Handler;
   end On_Max_Text_Handler;

   ----------------------
   -- Fire_On_Max_Text --
   ----------------------

   procedure Fire_On_Max_Text (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Max_Text_Event /= null then
         Edit.On_Max_Text_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Max_Text;

   ----------------------------------
   -- On_Horizontal_Scroll_Handler --
   ----------------------------------

   procedure On_Horizontal_Scroll_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Horizontal_Scroll_Event := Handler;
   end On_Horizontal_Scroll_Handler;

   -------------------------------
   -- Fire_On_Horizontal_Scroll --
   -------------------------------

   procedure Fire_On_Horizontal_Scroll (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Horizontal_Scroll_Event /= null then
         Edit.On_Horizontal_Scroll_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Horizontal_Scroll;

   --------------------------------
   -- On_Vertical_Scroll_Handler --
   --------------------------------

   procedure On_Vertical_Scroll_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Vertical_Scroll_Event := Handler;
   end On_Vertical_Scroll_Handler;

   ----------------------------
   -- Fire_On_Vertical_Scroll --
   -----------------------------

   procedure Fire_On_Vertical_Scroll (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Vertical_Scroll_Event /= null then
         Edit.On_Vertical_Scroll_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Vertical_Scroll;

   -----------------------
   -- On_Update_Handler --
   -----------------------

   procedure On_Update_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Update_Event := Handler;
   end On_Update_Handler;

   -------------------
   -- Fire_On_Update --
   -------------------

   procedure Fire_On_Update (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Update_Event /= null then
         Edit.On_Update_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Update;

   ------------------------------
   -- On_Out_Of_Memory_Handler --
   ------------------------------

   procedure On_Out_Of_Memory_Handler (Edit   : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Edit.On_Out_Of_Memory_Event := Handler;
   end On_Out_Of_Memory_Handler;

   ---------------------------
   -- Fire_On_Out_Of_Memory --
   ---------------------------

   procedure Fire_On_Out_Of_Memory (Edit : in out Edit_Box_Type)
   is
      use GWindows.Base;
   begin
      if Edit.On_Out_Of_Memory_Event /= null then
         Edit.On_Out_Of_Memory_Event (Base_Window_Type'Class (Edit));
      end if;
   end Fire_On_Out_Of_Memory;

   ------------
   -- Create --
   ------------

   procedure Create
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                              := 0;
      Top               : in     Integer                              := 0;
      Width             : in     Integer                              := 0;
      Height            : in     Integer                              := 0;
      Horizontal_Scroll : in     Boolean;
      Vertical_Scroll   : in     Boolean;
      Capture_Return    : in     Boolean;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
   begin
      Create_Multi_Line (Edit, Parent, Text,
                         Left, Top, Width, Height,
                         Horizontal_Scroll,
                         Vertical_Scroll,
                         Capture_Return,
                         Read_Only,
                         ID, Show,
                         Is_Dynamic);
   end Create;

   function Recommended_Size (Edit : in Multi_Line_Edit_Box_Type)
                             return GWindows.Types.Size_Type
   is
      Text_Size : Size_Type;
      Extra     : constant Size_Type := (9, 4);
      --  white space around text, borders.

      Canvas          : GWindows.Drawing.Canvas_Type;
      Font            : GWindows.Drawing_Objects.Font_Type;
      Base_Size       : Size_Type;
      Max_Line_Length : Natural := 0;

   begin
      Get_Canvas (Edit, Canvas);
      Get_Font (Edit, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);

      for I in 1 .. Line_Count (Edit) loop
         Max_Line_Length := Integer'Max
           (Line_Length (Edit, I), Max_Line_Length);
      end loop;

      Base_Size := GWindows.Drawing.Text_Output_Size (Canvas, "M");

      Text_Size :=
        (Width  => Max_Line_Length * Base_Size.Width,
         Height => Line_Count (Edit) * Base_Size.Height);

      return Calculate_New_Window_Size (Edit, Text_Size + Extra);
   end Recommended_Size;

end GWindows.Edit_Boxes;
