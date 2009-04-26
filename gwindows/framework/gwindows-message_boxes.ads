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
