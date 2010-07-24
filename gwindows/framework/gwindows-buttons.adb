------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . B U T T O N S                       --
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

with GWindows.Drawing;
with Interfaces.C;

package body GWindows.Buttons is
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   --  Win32 Button Styles

--     BS_PUSHBUTTON              : constant := 0;
   BS_DEFPUSHBUTTON           : constant := 1;
   BS_CHECKBOX                : constant := 2;
--     BS_AUTOCHECKBOX            : constant := 3;
--     BS_RADIOBUTTON             : constant := 4;
   BS_3STATE                  : constant := 5;
--     BS_AUTO3STATE              : constant := 6;
   BS_GROUPBOX                : constant := 7;
--     BS_USERBUTTON              : constant := 8;
   BS_AUTORADIOBUTTON         : constant := 9;
--     BS_OWNERDRAW               : constant := 11;
--     BS_LEFTTEXT                : constant := 32;
--     BS_TEXT                    : constant := 0;
--     BS_ICON                    : constant := 64;
--     BS_BITMAP                  : constant := 128;
--     BS_LEFT                    : constant := 256;
--     BS_RIGHT                   : constant := 512;
--     BS_CENTER                  : constant := 768;
--     BS_TOP                     : constant := 1024;
--     BS_BOTTOM                  : constant := 2048;
--     BS_VCENTER                 : constant := 3072;
   BS_PUSHLIKE                : constant := 4096;
--     BS_MULTILINE               : constant := 8192;
--     BS_NOTIFY                  : constant := 16384;
--     BS_FLAT                    : constant := 32768;
--     BS_RIGHTBUTTON             : constant := 32;

   --  Win32 Button Messages

   BM_GETCHECK                : constant := 240;
   BM_SETCHECK                : constant := 241;
--     BM_GETSTATE                : constant := 242;
--     BM_SETSTATE                : constant := 243;
--     BM_SETSTYLE                : constant := 244;
--     BM_CLICK                   : constant := 245;
--     BM_GETIMAGE                : constant := 246;
--     BM_SETIMAGE                : constant := 247;

   --  Win32 Button Notifications

   BN_CLICKED                 : constant := 0;
--     BN_PAINT                   : constant := 1;
--     BN_HILITE                  : constant := 2;
--     BN_UNHILITE                : constant := 3;
--     BN_DISABLE                 : constant := 4;
--     BN_DOUBLECLICKED           : constant := 5;
   BN_PUSHED                  : constant := 2;
   BN_UNPUSHED                : constant := 3;
--     BN_DBLCLK                  : constant := 5;
   BN_SETFOCUS                : constant := 6;
   BN_KILLFOCUS               : constant := 7;

   --  Win32 Button States

   BST_UNCHECKED              : constant := 0;
   BST_CHECKED                : constant := 1;
   BST_INDETERMINATE          : constant := 2;
--     BST_PUSHED                 : constant := 4;
--     BST_FOCUS                  : constant := 8;

   GWL_STYLE : constant := -16;

   procedure SetWindowLong
     (hwnd    : GWindows.Types.Handle;
      nIndex  : Interfaces.C.int := GWL_STYLE;
      newLong : Interfaces.C.unsigned);
   pragma Import (StdCall, SetWindowLong,
                    "SetWindowLong" & Character_Mode_Identifier);

   function GetWindowLong
     (hwnd   : GWindows.Types.Handle;
      nIndex : Interfaces.C.int := GWL_STYLE)
     return Interfaces.C.unsigned;
   pragma Import (StdCall, GetWindowLong,
                    "GetWindowLong" & Character_Mode_Identifier);

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Button     : in out Button_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_Control (Button,
                      Parent,
                      "Button",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Is_Dynamic => Is_Dynamic);

      Tab_Stop (Button);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Button);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;

            if Height /= 0 then
               New_Size.Height := Height;
            end if;

            Size (Button, New_Size);
         end;
      end if;

      if Show then
         GWindows.Buttons.Show (Button);
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Box        : in out Group_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_Control (Box,
                      Parent,
                      "Button",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      0,
                      Is_Dynamic => Is_Dynamic);

      SetWindowLong (Handle (Box),
                     newLong =>
                       GetWindowLong (Handle (Box)) or
                       BS_GROUPBOX);

      Group (Box);

      if Show then
         GWindows.Buttons.Show (Box);
      end if;
   end Create;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Window : in Button_Type)
                             return GWindows.Types.Size_Type
   is
      use type GWindows.Types.Size_Type;
      Canvas : GWindows.Drawing.Canvas_Type;
   begin
      Get_Canvas (Window, Canvas);
      return Calculate_New_Window_Size
        (Window, (6, 6) +
         GWindows.Drawing.Text_Output_Size (Canvas, Text (Window)));
   end Recommended_Size;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Button_Type) is
   begin
      Fire_On_Click (Button);
   end On_Click;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Cancel_Button_Type) is
   begin
      GWindows.Base.Close (Controlling_Parent (Button).all);
   end On_Click;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Default_Cancel_Button_Type)
   is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_DEFPUSHBUTTON);
   end On_Create;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Dialog_Button_Type)
   is
   begin
      GWindows.Base.End_Dialog (Controlling_Parent (Button).all,
                                ID (Button));
   end On_Click;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Default_Dialog_Button_Type)
   is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_DEFPUSHBUTTON);
   end On_Create;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Three_State_Box_Type) is
   begin
      case State (Button) is
         when Checked =>
            State (Button, Unchecked);
         when Unchecked =>
            State (Button, Indeterminate);
         when Indeterminate =>
            State (Button, Checked);
      end case;

      On_Click (Button_Type (Button));
   end On_Click;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Check_Box_Type) is
   begin
      case State (Button) is
         when Checked =>
            State (Button, Unchecked);
         when Unchecked =>
            State (Button, Checked);
      end case;

      On_Click (Button_Type (Button));
   end On_Click;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Button : in out Radio_Button_Type) is
   begin
      On_Click (Button_Type (Button));
   end On_Click;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Button : in out Button_Type) is
   begin
      Fire_On_Focus (Button);
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Button : in out Button_Type) is
   begin
      Fire_On_Lost_Focus (Button);
   end On_Lost_Focus;

   ---------------
   -- On_Pushed --
   ---------------

   procedure On_Pushed (Button : in out Button_Type) is
   begin
      Fire_On_Pushed (Button);
   end On_Pushed;

   -----------------
   -- On_Released --
   -----------------

   procedure On_Released (Button : in out Button_Type) is
   begin
      Fire_On_Released (Button);
   end On_Released;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command (Window  : in out Button_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class)
   is
      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when BN_CLICKED =>
            On_Click (Button_Type'Class (Window));
         when BN_SETFOCUS =>
            On_Focus (Button_Type'Class (Window));
         when BN_KILLFOCUS =>
            On_Lost_Focus (Button_Type'Class (Window));
         when BN_PUSHED =>
            On_Pushed (Button_Type'Class (Window));
         when BN_UNPUSHED =>
            On_Released (Button_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Default_Button_Type) is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_DEFPUSHBUTTON);
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Three_State_Box_Type) is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       (GetWindowLong (Handle (Window)) or BS_3STATE));
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Three_State_Button_Type) is
   begin
      On_Create (Three_State_Box_Type (Window));

      SetWindowLong (Handle (Window),
                     newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_PUSHLIKE);
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Check_Box_Type) is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       (GetWindowLong (Handle (Window)) or BS_CHECKBOX));
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Two_State_Button_Type) is
   begin
      On_Create (Check_Box_Type (Window));

      SetWindowLong (Handle (Window),
                     newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_PUSHLIKE);
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Radio_Button_Type) is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       (GetWindowLong (Handle (Window))
                        or
                        BS_AUTORADIOBUTTON));
   end On_Create;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Push_Radio_Button_Type) is
   begin
      SetWindowLong (Handle (Window),
                     newLong =>
                       (GetWindowLong (Handle (Window))
                        or
                        BS_AUTORADIOBUTTON or BS_PUSHLIKE));
   end On_Create;

   -----------
   -- State --
   -----------

   function State (Button : in Three_State_Box_Type)
                  return Three_State_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Button);
         uMsg   : Interfaces.C.int  := BM_GETCHECK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Interfaces.C.long;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case SendMessage is
         when BST_CHECKED =>
            return Checked;
         when BST_UNCHECKED =>
            return Unchecked;
         when others =>
            return Indeterminate;
      end case;
   end State;

   -----------
   -- State --
   -----------

   function State (Button : in Check_Box_Type)
                  return Check_State_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Button);
         uMsg   : Interfaces.C.int  := BM_GETCHECK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Interfaces.C.long;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case SendMessage is
         when BST_CHECKED =>
            return Checked;
         when others =>
            return Unchecked;
      end case;
   end State;

   -----------
   -- State --
   -----------

   procedure State (Button : in out Three_State_Box_Type;
                    State  : in     Three_State_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Button);
         uMsg   : Interfaces.C.int  := BM_SETCHECK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case State is
         when Checked =>
            SendMessage (wParam => BST_CHECKED);
         when Unchecked =>
            SendMessage (wParam => BST_UNCHECKED);
         when Indeterminate =>
            SendMessage (wParam => BST_INDETERMINATE);
      end case;
   end State;

   -----------
   -- State --
   -----------

   procedure State (Button : in out Check_Box_Type;
                    State  : in     Check_State_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Button);
         uMsg   : Interfaces.C.int      := BM_SETCHECK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case State is
         when Checked =>
            SendMessage (wParam => BST_CHECKED);
         when Unchecked =>
            SendMessage (wParam => BST_UNCHECKED);
      end case;
   end State;

   ----------------------
   -- On_Click_Handler --
   ----------------------

   procedure On_Click_Handler (Button  : in out Button_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Button.On_Click_Event := Handler;
   end On_Click_Handler;

   -------------------
   -- Fire_On_Click --
   -------------------

   procedure Fire_On_Click (Button : in out Button_Type)
   is
      use GWindows.Base;
   begin
      if Button.On_Click_Event /= null then
         Button.On_Click_Event (Base_Window_Type'Class (Button));
      end if;
   end Fire_On_Click;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler (Button  : in out Button_Type;
                               Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Button.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Button : in out Button_Type)
   is
      use GWindows.Base;
   begin
      if Button.On_Focus_Event /= null then
         Button.On_Focus_Event (Base_Window_Type'Class (Button));
      end if;
   end Fire_On_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Button  : in out Button_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Button.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Button : in out Button_Type)
   is
      use GWindows.Base;
   begin
      if Button.On_Lost_Focus_Event /= null then
         Button.On_Lost_Focus_Event (Base_Window_Type'Class (Button));
      end if;
   end Fire_On_Lost_Focus;

   -----------------------
   -- On_Pushed_Handler --
   -----------------------

   procedure On_Pushed_Handler (Button  : in out Button_Type;
                                Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Button.On_Pushed_Event := Handler;
   end On_Pushed_Handler;

   --------------------
   -- Fire_On_Pushed --
   --------------------

   procedure Fire_On_Pushed (Button : in out Button_Type)
   is
      use GWindows.Base;
   begin
      if Button.On_Pushed_Event /= null then
         Button.On_Pushed_Event (Base_Window_Type'Class (Button));
      end if;
   end Fire_On_Pushed;

   -------------------------
   -- On_Released_Handler --
   -------------------------

   procedure On_Released_Handler (Button  : in out Button_Type;
                                  Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Button.On_Released_Event := Handler;
   end On_Released_Handler;

   ----------------------
   -- Fire_On_Released --
   ----------------------

   procedure Fire_On_Released (Button : in out Button_Type)
   is
      use GWindows.Base;
   begin
      if Button.On_Released_Event /= null then
         Button.On_Released_Event (Base_Window_Type'Class (Button));
      end if;
   end Fire_On_Released;

   ---------
   -- not --
   ---------

   function "not" (State : in Check_State_Type) return Check_State_Type is
   begin
     if State = Checked then
       return Unchecked;
     else
       return Checked;
     end if;
   end "not";

end GWindows.Buttons;
