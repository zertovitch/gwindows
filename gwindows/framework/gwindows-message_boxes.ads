------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . M E S S A G E _ B O X E S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

with GWindows.Base;
with GWindows.Constants;

package GWindows.Message_Boxes is

   type Message_Box_Result is
     (OK, Cancel, Yes, No, Abort_Message, Retry, Ignore);

   type Message_Icon_Type is (Asterisk_Icon,
                              Information_Icon,
                              Exclamation_Icon,
                              Warning_Icon,
                              Hand_Icon,
                              Stop_Icon,
                              Error_Icon,
                              Question_Icon,
                              No_Icon);

   type Message_Box_Type is (Abort_Retry_Ignore_Box,
                             OK_Box,
                             OK_Cancel_Box,
                             OK_Cancel_Def_Box,
                             Retry_Cancel_Box,
                             Yes_No_Box,
                             Yes_No_Def_Box,
                             Yes_No_Cancel_Box,
                             Yes_No_Def_Cancel_Box,
                             Yes_No_Cancel_Def_Box);

   function Message_Box
     (Window      : in GWindows.Base.Base_Window_Type'Class;
      Title, Text : in GString;
      Style       : in Message_Box_Type  := OK_Box;
      Icon        : in Message_Icon_Type := No_Icon;
      Top_Most    : in Boolean           := False)
     return Message_Box_Result;

   procedure Message_Box
     (Window      : in GWindows.Base.Base_Window_Type'Class;
      Title, Text : in GString;
      Style       : in Message_Box_Type                     := OK_Box;
      Icon        : in Message_Icon_Type                    := No_Icon;
      Top_Most    : in Boolean                              := False);
   --  Pop a message box connected to this window

   function Message_Box
     (Title, Text : in GString;
      Style       : in Message_Box_Type  := OK_Box;
      Icon        : in Message_Icon_Type := No_Icon;
      Top_Most    : in Boolean           := False)
     return Message_Box_Result;

   procedure Message_Box
     (Title, Text : in GString;
      Style       : in Message_Box_Type  := OK_Box;
      Icon        : in Message_Icon_Type := No_Icon;
      Top_Most    : in Boolean           := False);
   --  Message Box

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
                          GWindows.Constants.Use_Default);

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
                          GWindows.Constants.Use_Default);

   procedure Message_Beep (Beep : Message_Icon_Type := No_Icon);
   --  Message Beep based that relates to Message_Icon_Type

end GWindows.Message_Boxes;
