------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     GWindows.Common_controls.Ex_Tb                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                   Copyright (C) 1999 - 2021 KonAd GmbH                   --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Strings;
with GWindows.GStrings;
with GWindows.Types;

package body GWindows.Common_Controls.Ex_Tb is

   CCS_NORESIZE        : constant := 4;
   --
   WM_USER             : constant := 16#400#;
   TB_ADDBUTTONS       : constant := WM_USER + 20;
   --  TB_AUTOSIZE         : constant := WM_USER + 33;
   TB_BUTTONSTRUCTSIZE : constant := WM_USER + 30;
   TBSTATE_ENABLED     : constant := 16#4#;
   TBSTYLE_FLAT        : constant := 16#800#;
   TBSTYLE_LIST        : constant := 16#1000#;
   TBSTYLE_TOOLTIPS    : constant := 16#100#;
   --
   BTNS_CHECK          : constant := 2;
   BTNS_AUTOSIZE       : constant := 16#10#;
   I_IMAGENONE         : constant := (-2);

   type Tbbutton is
      record
         Image   : Integer                    := 0;
         Command : Integer                    := 0;
         State   : Interfaces.C.unsigned_char := 0;
         Style   : Interfaces.C.unsigned_char := 0;
         Pad1    : Interfaces.C.unsigned_char := 0;
         pad2    : Interfaces.C.unsigned_char := 0;
         Data    : GWindows.Types.Wparam      := 0;
         Istring : Interfaces.C.Strings.chars_ptr;
      end record;

   --------------
   -- CreateEx --
   --------------

   procedure CreateEx
     (Control       : in out Ex_Toolbar_Control_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left          : in     Integer;
      Top           : in     Integer;
      Width         : in     Integer;
      Height        : in     Integer;
      Max_Buttons   : in     Positive                             := 100;
      Text_Position : in     Text_Position_Type                   := Right;
      Is_Flat       : in     Boolean                              := True;
      Show          : in     Boolean                              := True;
      Is_Dynamic    : in     Boolean                              := False) is

      use Interfaces.C;
      use type GWindows.Types.Wparam;

      Styles : Interfaces.C.unsigned := CCS_NORESIZE + TBSTYLE_TOOLTIPS;

      procedure Sendmessage_Buttonstructsize
        (Hwnd   : GWindows.Types.Handle := Handle (Control);
         Umsg   : Interfaces.C.int      := TB_BUTTONSTRUCTSIZE;
         Wparam : GWindows.Types.Wparam := Tbbutton'Size / 8;
         Lparam : GWindows.Types.Lparam := 0);
      pragma Import (Stdcall, Sendmessage_Buttonstructsize,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      if Text_Position = Right then
         Styles := Styles + TBSTYLE_LIST;
      end if;
      if Is_Flat then
         Styles := Styles + TBSTYLE_FLAT;
      end if;

      GWindows.Base.Create_Control (GWindows.Base.Base_Window_Type (Control),
                                    Parent,
                                    "ToolbarWindow32",
                                    "",
                                    Left, Top, Width, Height,
                                    0, Styles,
                                    Is_Dynamic => Is_Dynamic);

      Sendmessage_Buttonstructsize;

      Control.ToolTip_Array := new Tooltipdata_Array_Type (1 .. Max_Buttons);

      if Show then
         GWindows.Common_Controls.Show (Toolbar_Control_Type (Control));
      end if;

   end CreateEx;

   --------------------
   -- Set_Image_list --
   --------------------

   procedure Set_Image_List
     (Control : in out Ex_Toolbar_Control_Type;
      List    : in     GWindows.Image_Lists.Ex_Image_Lists.Ex_Image_List_Type)
   is
      use GWindows.Image_Lists;
   begin
      Set_Image_List (Toolbar_Control_Type (Control), Image_List_Type (List));
   end Set_Image_List;

   ----------------
   -- Add_Button --
   ----------------

   procedure Add_Button (Control      : in out Ex_Toolbar_Control_Type;
                         Image_Index  : in     Natural;
                         Command      : in     Integer;
                         Button_Style : in     Button_Style_Type;
                         Text         : in     GString               := " ";
                         Tooltip      : in     GString               := " ";
                         None_Image   : in     Boolean               := False)
   is
      use Interfaces.C;

      type Button_Array is array (1 .. 1) of Tbbutton;
      Tb : Button_Array;

      procedure Sendmessage_Addbutton
        (Hwnd   : GWindows.Types.Handle := Handle (Control);
         Umsg   : Interfaces.C.int      := TB_ADDBUTTONS;
         Wparam : GWindows.Types.Lparam := 1;
         Lparam : Button_Array          := Tb);
      pragma Import (Stdcall, Sendmessage_Addbutton,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case Button_Style is
         when Check_Style =>
            Tb (1).Style := BTNS_CHECK;
         when others =>
            Tb (1).Style := 0;
      end case;

      Tb (1).Style   := Tb (1).Style + BTNS_AUTOSIZE;
      Tb (1).Istring := Interfaces.C.Strings.New_String (GWindows.GStrings.To_String (Text));
      Tb (1).Image   := Image_Index;
      Tb (1).Command := Command;
      Tb (1).State   := TBSTATE_ENABLED;

      if None_Image then
         Tb (1).Image := I_IMAGENONE;
      end if;

      if Control.Next_Tooltip > 100 then
         return;
      end if;

      Control.ToolTip_Array (Control.Next_Tooltip).Command := Command;
      Control.ToolTip_Array (Control.Next_Tooltip).Tooltip :=
        new GString'(Tooltip);
      Control.Next_Tooltip := Control.Next_Tooltip + 1;

      Sendmessage_Addbutton;
   end Add_Button;

   -----------------
   -- Get_Tooltip --
   -----------------

   function Get_Tooltip (Control : in Ex_Toolbar_Control_Type;
                         command : in Integer)
                        return GString
   is
   begin
      for I in 1 .. Control.Next_Tooltip - 1 loop
         if Control.ToolTip_Array (I).Command = command then
            return Control.ToolTip_Array (I).Tooltip.all;
         end if;
      end loop;
      return "";

   exception
      when others =>
         return "";
   end Get_Tooltip;

end GWindows.Common_Controls.Ex_Tb;
