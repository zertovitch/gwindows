------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--              G W I N D O W S . S T A T I C _ C O N T R O L S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2021 David Botton                   --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Drawing;
with Interfaces.C;

package body GWindows.Static_Controls is
   use type Interfaces.C.unsigned;

   SS_LEFT           : constant := 0;
   SS_CENTER         : constant := 1;
   SS_RIGHT          : constant := 2;
   SS_ICON           : constant := 3;
--     SS_BLACKRECT      : constant := 4;
--     SS_GRAYRECT       : constant := 5;
--     SS_WHITERECT      : constant := 6;
--     SS_BLACKFRAME     : constant := 7;
--     SS_GRAYFRAME      : constant := 8;
--     SS_WHITEFRAME     : constant := 9;
--     SS_USERITEM       : constant := 10;
--     SS_SIMPLE         : constant := 11;
   SS_LEFTNOWORDWRAP : constant := 12;
--     SS_NOPREFIX       : constant := 128;
--     SS_OWNERDRAW      : constant := 16#D#;
   SS_BITMAP         : constant := 16#E#;
   SS_ENHMETAFILE    : constant := 16#F#;
--     SS_ETCHEDHORZ     : constant := 16#10#;
--     SS_ETCHEDVERT     : constant := 16#11#;
--     SS_ETCHEDFRAME    : constant := 16#12#;
--     SS_TYPEMASK       : constant := 16#1F#;
   SS_NOTIFY         : constant := 16#100#;
   SS_CENTERIMAGE    : constant := 16#200#;
   SS_RIGHTJUST      : constant := 16#400#;
   SS_REALSIZEIMAGE  : constant := 16#800#;
   SS_SUNKEN         : constant := 16#1000#;
--     SS_ENDELLIPSIS    : constant := 16#4000#;
--     SS_PATHELLIPSIS   : constant := 16#8000#;
--     SS_WORDELLIPSIS   : constant := 16#C000#;
--     SS_ELLIPSISMASK   : constant := 16#C000#;

--     STM_SETICON  : constant := 16#0170#;
--     STM_GETICON  : constant := 16#0171#;
   STM_SETIMAGE : constant := 16#0172#;
--     STM_GETIMAGE : constant := 16#0173#;

   STN_CLICKED  : constant := 0;
   STN_DBLCLK   : constant := 1;
   STN_ENABLE   : constant := 2;
   STN_DISABLE  : constant := 3;

   procedure Adapt_To_Border
      (Styles : in out Interfaces.C.unsigned;
       Border :        Border_Type)
   is
      WS_BORDER        : constant := 8388608;
      --  WS_EX_CLIENTEDGE : constant := 16#200#;
   begin
      case Border is
         when None =>
            null;
         when Simple =>
            Styles := Styles or WS_BORDER;
         when Half_Sunken =>
            Styles := Styles or SS_SUNKEN;
         --  when Fully_Sunken =>
            --  ExStyles := ExStyles or WS_EX_CLIENTEDGE;
      end case;
   end Adapt_To_Border;

   ------------
   -- Create --
   ------------

   procedure Create
     (Static     : in out Label_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=  SS_NOTIFY;
   begin
      case Alignment is
         when Right =>
            Styles := Styles or SS_RIGHT;
         when Center =>
            Styles := Styles or SS_CENTER;
         when Left_No_Word_Wrap =>
            Styles := Styles or SS_LEFTNOWORDWRAP;
         when others =>
            Styles := Styles or SS_LEFT;
      end case;

      Adapt_To_Border (Styles, Border);

      Create_Control (Static,
                      Parent,
                      "Static",
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
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Static);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Static, New_Size);
         end;
      end if;

      if Show then
         GWindows.Static_Controls.Show (Static);
      end if;
   end Create;

   ------------------
   -- Create_Label --
   ------------------

   procedure Create_Label
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True)
   is
      Temp_Label : constant Label_Access := new Label_Type;
   begin
      Create (Temp_Label.all,
              Parent, Text, Left, Top, Width, Height,
              Alignment, Border,
              ID, Show,
              Is_Dynamic => True);
   end Create_Label;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Static : in Label_Type)
                             return GWindows.Types.Size_Type
   is
      use GWindows.Types;

      Text_Size : Size_Type;
      Extra     : constant Size_Type := (9, 4);
      --  white space around text, borders.

      Canvas       : GWindows.Drawing.Canvas_Type;
      Font         : GWindows.Drawing_Objects.Font_Type;
      Current_Text : constant GString := Text (Static);
   begin
      Get_Canvas (Static, Canvas);
      Get_Font (Static, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);

      Text_Size := GWindows.Drawing.Text_Output_Size (Canvas, Current_Text);

      return Calculate_New_Window_Size (Static, Text_Size + Extra);
   end Recommended_Size;

   ------------
   -- Create --
   ------------

   procedure Create
     (Static     : in out Icon_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=  SS_NOTIFY or SS_ICON;
   begin
      case Alignment is
         when Right =>
            Styles := Styles or SS_RIGHTJUST;
         when Center =>
            Styles := Styles or SS_CENTERIMAGE;
         when Static_Size =>
            Styles := Styles or SS_REALSIZEIMAGE or SS_CENTERIMAGE;
         when others =>
            null;
      end case;

      Adapt_To_Border (Styles, Border);

      Create_Control (Static,
                      Parent,
                      "Static",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Static_Controls.Show (Static);
      end if;
   end Create;

   ----------------
   -- Create_Icon --
   ----------------

   procedure Create_Icon
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True)
   is
      Temp_Icon : constant Icon_Access := new Icon_Type;
   begin
      Create (Temp_Icon.all,
              Parent, Text, Left, Top, Width, Height,
              Alignment, Border,
              ID, Show,
              Is_Dynamic => True);
   end Create_Icon;

   ------------
   -- Create --
   ------------

   procedure Create
     (Static     : in out Bitmap_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned := SS_NOTIFY or SS_BITMAP;
   begin
      case Alignment is
         when Right =>
            Styles := Styles or SS_RIGHTJUST;
         when Center =>
            Styles := Styles or SS_CENTERIMAGE;
         when Static_Size =>
            Styles := Styles or SS_REALSIZEIMAGE or SS_CENTERIMAGE;
         when others =>
            null;
      end case;

      Adapt_To_Border (Styles, Border);

      Create_Control (Static,
                      Parent,
                      "Static",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Static_Controls.Show (Static);
      end if;
   end Create;

   -------------------
   -- Create_Bitmap --
   -------------------

   procedure Create_Bitmap
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True)
   is
      Temp_Bitmap : constant Bitmap_Access := new Bitmap_Type;
   begin
      Create (Temp_Bitmap.all,
              Parent, Text, Left, Top, Width, Height,
              Alignment, Border,
              ID, Show,
              Is_Dynamic => True);
   end Create_Bitmap;

   ----------------
   -- Set_Bitmap --
   ----------------

   procedure Set_Bitmap (Static : in out Bitmap_Type;
                         Bitmap : in GWindows.Drawing_Objects.Bitmap_Type) is
      IMAGE_BITMAP : constant := 0;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Static);
         uMsg   : Interfaces.C.int      := STM_SETIMAGE;
         wParam : GWindows.Types.Wparam := IMAGE_BITMAP;
         lParam : GWindows.Types.Handle :=
            GWindows.Drawing_Objects.Handle (Bitmap));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Bitmap;

   ------------
   -- Create --
   ------------

   procedure Create
     (Static     : in out Meta_File_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned :=
        SS_NOTIFY or SS_ENHMETAFILE;
   begin
      case Alignment is
         when Right =>
            Styles := Styles or SS_RIGHTJUST;
         when Center =>
            Styles := Styles or SS_CENTERIMAGE;
         when Static_Size =>
            Styles := Styles or SS_REALSIZEIMAGE or SS_CENTERIMAGE;
         when others =>
            null;
      end case;

      Adapt_To_Border (Styles, Border);

      Create_Control (Static,
                      Parent,
                      "Static",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Static_Controls.Show (Static);
      end if;
   end Create;

   procedure Create_Meta_File
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True)
   is
      Temp_Meta_File : constant Meta_File_Access := new Meta_File_Type;
   begin
      Create (Temp_Meta_File.all,
              Parent, Text, Left, Top, Width, Height,
              Alignment, Border,
              ID, Show,
              Is_Dynamic => True);
   end Create_Meta_File;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Static : in out Label_Type) is
   begin
      Fire_On_Click (Static);
   end On_Click;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (Static : in out Label_Type) is
   begin
      Fire_On_Double_Click (Static);
   end On_Double_Click;

   ---------------
   -- On_Enable --
   ---------------

   procedure On_Enable (Static : in out Label_Type) is
   begin
      Fire_On_Enable (Static);
   end On_Enable;

   ----------------
   -- On_Disable --
   ----------------

   procedure On_Disable (Static : in out Label_Type) is
   begin
      Fire_On_Disable (Static);
   end On_Disable;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out Label_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Unreferenced (ID, Control);
   begin
      case Code is
         when STN_CLICKED =>
            On_Click (Label_Type'Class (Window));
         when STN_DBLCLK =>
            On_Double_Click (Label_Type'Class (Window));
         when STN_ENABLE =>
            On_Enable (Label_Type'Class (Window));
         when STN_DISABLE =>
            On_Disable (Label_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   ----------------------
   -- On_Click_Handler --
   ----------------------

   procedure On_Click_Handler (Static  : in out Label_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Static.On_Click_Event := Handler;
   end On_Click_Handler;

   -------------------
   -- Fire_On_Click --
   -------------------

   procedure Fire_On_Click (Static : in out Label_Type)
   is
      use GWindows.Base;
   begin
      if Static.On_Click_Event /= null then
         Static.On_Click_Event (Base_Window_Type'Class (Static));
      end if;
   end Fire_On_Click;

   -----------------------------
   -- On_Double_Click_Handler --
   -----------------------------

   procedure On_Double_Click_Handler
     (Static  : in out Label_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Static.On_Double_Click_Event := Handler;
   end On_Double_Click_Handler;

   --------------------------
   -- Fire_On_Double_Click --
   --------------------------

   procedure Fire_On_Double_Click (Static : in out Label_Type)
   is
      use GWindows.Base;
   begin
      if Static.On_Double_Click_Event /= null then
         Static.On_Double_Click_Event (Base_Window_Type'Class (Static));
      end if;
   end Fire_On_Double_Click;

   -----------------------
   -- On_Enable_Handler --
   -----------------------

   procedure On_Enable_Handler (Static  : in out Label_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Static.On_Enable_Event := Handler;
   end On_Enable_Handler;

   --------------------
   -- Fire_On_Enable --
   --------------------

   procedure Fire_On_Enable (Static : in out Label_Type)
   is
      use GWindows.Base;
   begin
      if Static.On_Enable_Event /= null then
         Static.On_Enable_Event (Base_Window_Type'Class (Static));
      end if;
   end Fire_On_Enable;

   ------------------------
   -- On_Disable_Handler --
   ------------------------

   procedure On_Disable_Handler (Static  : in out Label_Type;
                                 Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Static.On_Disable_Event := Handler;
   end On_Disable_Handler;

   ---------------------
   -- Fire_On_Disable --
   ----------------------

   procedure Fire_On_Disable (Static : in out Label_Type)
   is
      use GWindows.Base;
   begin
      if Static.On_Disable_Event /= null then
         Static.On_Disable_Event (Base_Window_Type'Class (Static));
      end if;
   end Fire_On_Disable;

end GWindows.Static_Controls;
