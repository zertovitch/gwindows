------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . S C R O L L _ B A R S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

with Interfaces.C;
with GWindows.Types;

package body GWindows.Scroll_Bars is
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   SB_CTL : constant := 2;

   SBS_HORZ   : constant := 0;
   SBS_VERT   : constant := 1;
   WS_TABSTOP : constant := 65536;

   SIF_RANGE           : constant := 1;
   SIF_PAGE            : constant := 2;
   SIF_POS             : constant := 4;
--     SIF_DISABLENOSCROLL : constant := 8;
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

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Bar        : in out Scroll_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Direction  : in     Scroll_Direction_Type;
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : Interfaces.C.unsigned := WS_TABSTOP;
   begin
      if Direction = Horizontal then
         Styles := Styles or SBS_HORZ;
      else
         Styles := Styles or SBS_VERT;
      end if;

      Create_Control (Bar,
                      Parent,
                      "SCROLLBAR",
                      "",
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Scroll_Bars.Show (Bar);
      end if;
   end Create;

   ------------------
   -- Scroll_Range --
   ------------------

   procedure Scroll_Range
     (Bar  : in out Scroll_Bar_Type;
      Minimum : in     Integer;
      Maximum : in     Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Minimum;
      Info.nMax := Maximum;

      SetScrollInfo (Handle (Bar), SB_CTL, Info);
   end Scroll_Range;

   --------------------
   -- Scroll_Maximum --
   --------------------

   procedure Scroll_Maximum
     (Bar  : in out Scroll_Bar_Type;
      Maximum : in     Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Scroll_Minimum (Bar);
      Info.nMax := Maximum;

      SetScrollInfo (Handle (Bar), SB_CTL, Info);
   end Scroll_Maximum;

   function Scroll_Maximum
     (Bar : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;

      GetScrollInfo (Handle (Bar), SB_CTL, Info);

      return Info.nMax;
   end Scroll_Maximum;

   --------------------
   -- Scroll_Minimum --
   --------------------

   procedure Scroll_Minimum
     (Bar  : in out Scroll_Bar_Type;
      Minimum : in     Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;
      Info.nMin := Minimum;
      Info.nMax := Scroll_Maximum (Bar);

      SetScrollInfo (Handle (Bar), SB_CTL, Info);
   end Scroll_Minimum;

   function Scroll_Minimum
     (Bar : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_RANGE;

      GetScrollInfo (Handle (Bar), SB_CTL, Info);

      return Info.nMin;
   end Scroll_Minimum;

   ---------------------
   -- Scroll_Position --
   ---------------------

   procedure Scroll_Position
     (Bar   : in out Scroll_Bar_Type;
      Position : in     Integer)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_POS;
      Info.nPos := Position;

      SetScrollInfo (Handle (Bar), SB_CTL, Info);
   end Scroll_Position;

   function Scroll_Position
     (Bar : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_POS;

      GetScrollInfo (Handle (Bar), SB_CTL, Info);

      return Info.nPos;
   end Scroll_Position;

   ----------------------
   -- Scroll_Page_Size --
   ----------------------

   procedure Scroll_Page_Size
     (Bar   : in out Scroll_Bar_Type;
      Size     : in     Natural)
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_PAGE;
      Info.nPage := Size;

      SetScrollInfo (Handle (Bar), SB_CTL, Info);
   end Scroll_Page_Size;

   function Scroll_Page_Size
     (Bar : in Scroll_Bar_Type) return Natural
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_PAGE;

      GetScrollInfo (Handle (Bar), SB_CTL, Info);

      return Info.nPage;
   end Scroll_Page_Size;

   --------------------------
   -- Scroll_Drag_Position --
   --------------------------

   function Scroll_Drag_Position
     (Bar : in Scroll_Bar_Type) return Integer
   is
      Info : SCROLLINFO;
   begin
      Info.fMask := SIF_TRACKPOS;

      GetScrollInfo (Handle (Bar), SB_CTL, Info);

      return Info.nTrackPos;
   end Scroll_Drag_Position;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll
     (Bar     : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type)
   is
      use GWindows.Base;
   begin
      case Request is
         when First =>
            Scroll_Position (Bar,
                             Scroll_Minimum (Bar));
         when Last =>
            Scroll_Position (Bar,
                             Scroll_Maximum (Bar));
         when Previous_Unit =>
            Scroll_Position (Bar,
                             Scroll_Position (Bar) - 1);
         when Next_Unit =>
            Scroll_Position (Bar,
                             Scroll_Position (Bar) + 1);
         when Previous_Page =>
            Scroll_Position (Bar,
                             Scroll_Position (Bar) -
                             Scroll_Page_Size (Bar));
         when Next_Page =>
            Scroll_Position (Bar,
                             Scroll_Position (Bar) +
                             Scroll_Page_Size (Bar));
         when Thumb_Set =>
            Scroll_Position (Bar,
                             Scroll_Drag_Position (Bar));
         when others =>
            null;
      end case;

      Fire_On_Scroll (Bar, Request);
   end On_Scroll;

   --------------------------
   -- On_Horizontal_Scroll --
   --------------------------

   procedure On_Horizontal_Scroll
     (Window  : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, Control);
   begin
      On_Scroll (Scroll_Bar_Type'Class (Window), Request);
   end On_Horizontal_Scroll;

   ------------------------
   -- On_Vertical_Scroll --
   ------------------------

   procedure On_Vertical_Scroll
     (Window  : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, Control);
   begin
      On_Scroll (Scroll_Bar_Type'Class (Window), Request);
   end On_Vertical_Scroll;

   -----------------------
   -- On_Scroll_Handler --
   -----------------------

   procedure On_Scroll_Handler
     (Bar  : in out Scroll_Bar_Type;
      Handler : in     GWindows.Base.Scroll_Event)
   is
   begin
      Bar.On_Scroll_Event := Handler;
   end On_Scroll_Handler;

   --------------------
   -- Fire_On_Scroll --
   --------------------

   procedure Fire_On_Scroll
     (Bar  : in out Scroll_Bar_Type;
      Request : in     GWindows.Base.Scroll_Request_Type)
   is
      use GWindows.Base;
   begin
      if Bar.On_Scroll_Event /= null then
         Bar.On_Scroll_Event (Base_Window_Type'Class (Bar),
                                 Request);
      end if;
   end Fire_On_Scroll;

end GWindows.Scroll_Bars;
