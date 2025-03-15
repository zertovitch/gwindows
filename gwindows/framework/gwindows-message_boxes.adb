------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . M E S S A G E _ B O X E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.GStrings;

with GWindows.Windows;
with GWindows.Drawing_Objects;
with GWindows.Packing_Boxes;
with GWindows.Edit_Boxes;
with GWindows.Buttons;
with GWindows.Static_Controls;
with GWindows.Application;
with GWindows.Types;

package body GWindows.Message_Boxes is

   MB_OK                      : constant := 0;
   MB_OKCANCEL                : constant := 1;
   MB_ABORTRETRYIGNORE        : constant := 2;
   MB_YESNOCANCEL             : constant := 3;
   MB_YESNO                   : constant := 4;
   MB_RETRYCANCEL             : constant := 5;
   MB_ICONHAND                : constant := 16;
   MB_ICONQUESTION            : constant := 32;
   MB_ICONEXCLAMATION         : constant := 48;
   MB_ICONASTERISK            : constant := 64;
   MB_TOPMOST                 : constant := 2#10_0000_0000_0000_0000#;
   MB_DEFBUTTON2              : constant := 16#100#;
   MB_DEFBUTTON3              : constant := 16#200#;

--     MB_USERICON                : constant := 128;
--     MB_ICONWARNING             : constant := 48;
--     MB_ICONERROR               : constant := 16;
--     MB_ICONINFORMATION         : constant := 64;
--     MB_ICONSTOP                : constant := 16;

   -----------------
   -- Message_Box --
   -----------------

   function Message_Box
     (Window      : in GWindows.Base.Base_Window_Type'Class;
      Title, Text : in GString;
      Style       : in Message_Box_Type                     := OK_Box;
      Icon        : in Message_Icon_Type                    := No_Icon;
      Top_Most    : in Boolean                              := False)
     return Message_Box_Result
   is
      use GWindows.Constants;
      use type Interfaces.C.unsigned;

      function MessageBox
        (hwnd     : GWindows.Types.Handle := GWindows.Base.Handle (Window);
         Message  : access GChar_C;
         mb_Title : access GChar_C;
         uType    : in     Interfaces.C.unsigned   := 0)
        return Integer;
      pragma Import (StdCall, MessageBox,
                       "MessageBox" & Character_Mode_Identifier);

      BoxTitle   : GString_C := GWindows.GStrings.To_GString_C (Title);
      BoxMessage : GString_C := GWindows.GStrings.To_GString_C (Text);

      MIcon      : Interfaces.C.unsigned := 0;
      MStyle     : Interfaces.C.unsigned := 0;
      Result     : Integer;
      EResult    : Message_Box_Result;
   begin
      case Style is
         when Abort_Retry_Ignore_Box =>
            MStyle :=  MB_ABORTRETRYIGNORE;
         when OK_Box =>
            MStyle := MB_OK;
         when OK_Cancel_Box =>
            MStyle := MB_OKCANCEL;
         when OK_Cancel_Def_Box =>
            MStyle := MB_OKCANCEL + MB_DEFBUTTON2;
         when Retry_Cancel_Box =>
            MStyle := MB_RETRYCANCEL;
         when Yes_No_Box =>
            MStyle := MB_YESNO;
         when Yes_No_Def_Box =>
            MStyle := MB_YESNO + MB_DEFBUTTON2;
         when Yes_No_Cancel_Box =>
            MStyle := MB_YESNOCANCEL;
         when Yes_No_Def_Cancel_Box =>
            MStyle := MB_YESNOCANCEL + MB_DEFBUTTON2;
         when Yes_No_Cancel_Def_Box =>
            MStyle := MB_YESNOCANCEL + MB_DEFBUTTON3;
      end case;

      case Icon is
         when Asterisk_Icon | Information_Icon =>
            MIcon := MB_ICONASTERISK;
         when Exclamation_Icon | Warning_Icon =>
            MIcon := MB_ICONEXCLAMATION;
         when Hand_Icon | Stop_Icon | Error_Icon =>
            MIcon := MB_ICONHAND;
         when Question_Icon =>
            MIcon := MB_ICONQUESTION;
         when No_Icon =>
            MIcon := 0;
      end case;

      if Top_Most then
         MStyle := MStyle or MB_TOPMOST;
      end if;

      Result := MessageBox (Message  => BoxMessage (BoxMessage'First)'Access,
                            mb_Title => BoxTitle (BoxTitle'First)'Access,
                            uType    => MIcon or MStyle);

      case Result is
         when IDOK =>
            EResult := OK;
         when IDABORT =>
            EResult := Abort_Message;
         when IDRETRY =>
            EResult := Retry;
         when IDIGNORE =>
            EResult := Ignore;
         when IDYES =>
            EResult := Yes;
         when IDNO =>
            EResult := No;
         when others =>
            EResult := Cancel;
      end case;

      return EResult;
   end Message_Box;

   procedure Message_Box
     (Window      : in GWindows.Base.Base_Window_Type'Class;
      Title, Text : in GString;
      Style       : in Message_Box_Type                     := OK_Box;
      Icon        : in Message_Icon_Type                    := No_Icon;
      Top_Most    : in Boolean                              := False)
   is
      Result : Message_Box_Result :=
        Message_Box (Window, Title, Text, Style, Icon, Top_Most);
      pragma Warnings (Off, Result);
   begin
      null;
   end Message_Box;

   function Message_Box (Title, Text : in GString;
                         Style       : in Message_Box_Type  := OK_Box;
                         Icon        : in Message_Icon_Type := No_Icon;
                         Top_Most    : in Boolean           := False)
                        return Message_Box_Result
   is
      Tmp_Win : GWindows.Base.Base_Window_Type;
   begin
      return Message_Box (Tmp_Win, Title, Text, Style, Icon, Top_Most);
   end Message_Box;

   procedure Message_Box (Title, Text : in GString;
                          Style       : in Message_Box_Type  := OK_Box;
                          Icon        : in Message_Icon_Type := No_Icon;
                          Top_Most    : in Boolean           := False)
   is
      Result : Message_Box_Result := Message_Box
        (Title, Text, Style, Icon, Top_Most);
      pragma Warnings (Off, Result);
   begin
      null;
   end Message_Box;

   ------------------
   -- Message_Beep --
   ------------------

   procedure Message_Beep (Beep : Message_Icon_Type := No_Icon)
   is
      procedure MessageBeep (Tone : Integer);
      pragma Import (StdCall, MessageBeep, "MessageBeep");

      Tone : Integer;
   begin

      case Beep is
         when Asterisk_Icon | Information_Icon =>
            Tone := MB_ICONASTERISK;
         when Exclamation_Icon | Warning_Icon =>
            Tone := MB_ICONEXCLAMATION;
         when Hand_Icon | Stop_Icon | Error_Icon =>
            Tone := MB_ICONHAND;
         when Question_Icon =>
            Tone := MB_ICONQUESTION;
         when No_Icon =>
            Tone := MB_OK;
      end case;

      MessageBeep (Tone);
   end Message_Beep;

   ---------------
   -- Input_Box --
   ---------------

   procedure Input_Box (Window      : in out
                           GWindows.Base.Base_Window_Type'Class;
                        Title, Text : in     GString;
                        Out_Text    : in out GString_Unbounded;
                        Result      :    out Boolean;
                        Left        : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Top         : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Width       : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Height      : in     Integer :=
                          GWindows.Constants.Use_Default)

   is
      use GWindows.Windows;
      use GWindows.Packing_Boxes;
      use GWindows.Edit_Boxes;
      use GWindows.Buttons;

      Window_Font   : GWindows.Drawing_Objects.Font_Type;
      Win_Box       : Window_Type;
      Pack_Box      : Packing_Box_Type;
      In_Box        : Edit_Box_Type;
      Button_Box    : Packing_Box_Type;
      Ok_Button     : Default_Dialog_Button_Type;
      Cancel_Button : Cancel_Button_Type;
      Box_Result    : Integer;

      procedure Do_On_Destroy
        (Window_Dummy : in out GWindows.Base.Base_Window_Type'Class);
      --  Grab data

      procedure Do_On_Destroy
        (Window_Dummy : in out GWindows.Base.Base_Window_Type'Class)
      is
         pragma Unmodified (Window_Dummy);
      begin
         Out_Text := GWindows.GStrings.To_GString_Unbounded
           (GWindows.Edit_Boxes.Text (In_Box));
      end Do_On_Destroy;

   begin
      Result := False;

      Create_As_Dialog (Win_Box,
                        Window,
                        Title,
                        Left   => Left,
                        Top    => Top,
                        Width  => Width,
                        Height => Height);
      Center (Win_Box, Window);
      On_Destroy_Handler (Win_Box, Do_On_Destroy'Unrestricted_Access);

      --  Use Standard Windows GUI font instead of system font

      GWindows.Drawing_Objects.Create_Stock_Font
        (Window_Font, GWindows.Drawing_Objects.Default_GUI);

      Set_Font (Win_Box, Window_Font);

      Create (Pack_Box,
              Win_Box,
              1, 1, 1, 30,
              Vertical_From_Center);
      Dock (Pack_Box, GWindows.Base.Fill);
      Padding (Pack_Box, 5);
      Dock_Children (Win_Box);

      Insets (Pack_Box, (10, 10, 10, 10));
      Fill_Width (Pack_Box, True);

      GWindows.Static_Controls.Create_Label (Pack_Box, Text, 1, 1, 1, 30);

      GWindows.Edit_Boxes.Create (In_Box, Pack_Box,
                                  GWindows.GStrings.To_GString_From_Unbounded
                                    (Out_Text),
                                  1, 1, 1, 30);

      Focus (In_Box);

      Create (Button_Box,
              Pack_Box,
              1, 1, 50, 30,
              Horizontal_From_Center);
      Fill_Height (Button_Box);
      Padding (Button_Box, 5);
      Pack (Pack_Box);

      Create (Ok_Button, Button_Box, "O&k", 1, 1, 75, 30,
              ID => GWindows.Constants.IDOK);
      Create (Cancel_Button, Button_Box, "&Cancel", 1, 1, 75, 30,
              ID => GWindows.Constants.IDCANCEL);

      Pack (Button_Box);

      Box_Result := GWindows.Application.Show_Dialog (Win_Box, Window);

      if Box_Result = GWindows.Constants.IDOK then
         Result := True;
      else
         Out_Text := GWindows.GStrings.To_GString_Unbounded ("");
         Result := False;
      end if;
   end Input_Box;

   procedure Input_Box (Title, Text : in     GString;
                        Out_Text    : in out GString_Unbounded;
                        Result      :    out Boolean;
                        Left        : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Top         : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Width       : in     Integer :=
                          GWindows.Constants.Use_Default;
                        Height      : in     Integer :=
                          GWindows.Constants.Use_Default)
   is
      Tmp_Win : GWindows.Base.Base_Window_Type;
   begin
      Input_Box (Tmp_Win, Title, Text, Out_Text, Result,
                 Left, Top, Width, Height);
   end Input_Box;

end GWindows.Message_Boxes;
