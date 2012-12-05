------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--      G W I N D O W S . W I N D O W S . C O M M O N _ C O N T R O L S     --
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

with Ada.Calendar;

with Interfaces.C;

with GWindows.Base;
with GWindows.Colors;
with GWindows.Types;
with GWindows.Windows;
with GWindows.Image_Lists;

package GWindows.Common_Controls is

   -------------------------------------------------------------------------
   --  Common_Control_Type
   -------------------------------------------------------------------------
   --  Base class for common controls

   type Common_Control_Type is
     new GWindows.Base.Base_Window_Type with private;
   type Pointer_To_Common_Control_Class is
     access all Common_Control_Type'Class;

   --  AnSp: moved from body, needed by TVITEM
   type LPTSTR is access all GChar_C;

   -------------------------------------------------------------------------
   --  Common_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Click_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Click (Control : in out Common_Control_Type);

   procedure On_Double_Click_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Double_Click
     (Control : in out Common_Control_Type);

   procedure On_Right_Click_Handler
     (Control  : in out Common_Control_Type;
      Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Right_Click
     (Control : in out Common_Control_Type);

   procedure On_Right_Double_Click_Handler
     (Control  : in out Common_Control_Type;
      Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Right_Double_Click
     (Control : in out Common_Control_Type);

   procedure On_Return_Handler (Control : in out Common_Control_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Return (Control : in out Common_Control_Type);

   procedure On_Out_Of_Memory_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Out_Of_Memory (Control : in out Common_Control_Type);

   procedure On_Focus_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Focus (Control : in out Common_Control_Type);

   procedure On_Lost_Focus_Handler
     (Control : in out Common_Control_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (Control : in out Common_Control_Type);

   procedure On_Hover_Handler (Control : in out Common_Control_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Hover (Control : in out Common_Control_Type);

   -------------------------------------------------------------------------
   --  Common_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Click (Control : in out Common_Control_Type);
   --  Clicked

   procedure On_Double_Click (Control : in out Common_Control_Type);
   --  Double Clicked

   procedure On_Right_Click (Control : in out Common_Control_Type);
   --  Right Clicked

   procedure On_Right_Double_Click (Control : in out Common_Control_Type);
   --  Right Double Clicked

   procedure On_Return (Control : in out Common_Control_Type);
   --  Enter pressed

   procedure On_Out_Of_Memory (Control : in out Common_Control_Type);
   --  Can not allocate more memory

   procedure On_Focus (Control : in out Common_Control_Type);
   --  Received focus

   procedure On_Lost_Focus (Control : in out Common_Control_Type);
   --  Lost focus

   procedure On_Hover (Control : in out Common_Control_Type);
   --  Mouse hover over control

   -------------------------------------------------------------------------
   --  Common_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Notify
     (Window       : in out Common_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);
   --  Handles Notify Messages

   -------------------------------------------------------------------------
   --  Status_Bar_Type
   -------------------------------------------------------------------------

   type Status_Bar_Type is new Common_Control_Type with private;
   type Status_Bar_Access is access all Status_Bar_Type;
   type Pointer_To_Status_Bar_Class is access all Status_Bar_Type'Class;

   -------------------------------------------------------------------------
   --  Status_Bar_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Bar        : in out Status_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Size_Grip  : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create a status bar

   type Status_Bar_Position_Type is array (Natural range <>) of Integer;
   procedure Parts (Bar       : in out Status_Bar_Type;
                    Positions : in     Status_Bar_Position_Type);
   --  Setup number of parts and their positions in the statusbar
   --  Each element specifies the position, in client coordinates,
   --  of the right edge of the corresponding part. If an element is -1,
   --  the right edge of the corresponding part extends to the border of
   --  the window

   type Status_Kind_Type is (Flat, Sunken, Raised);
   procedure Text (Bar  : in out Status_Bar_Type;
                   Text : in     GString;
                   Part : in     Natural;
                   How  : in     Status_Kind_Type := Sunken);
   --  Set text in a specific part of the status bar
   --  0 is the first part

   procedure Background_Color (Bar   : in out Status_Bar_Type;
                               Color :        GWindows.Colors.Color_Type);
   --  Set background color of status bar

   -------------------------------------------------------------------------
   --  Animation_Control_Type
   -------------------------------------------------------------------------

   type Animation_Control_Type is new Common_Control_Type with private;
   type Animation_Control_Access is access all Animation_Control_Type;
   type Pointer_To_Animation_Control is
     access all Animation_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Animation_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control    : in out Animation_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer                              := 1;
      Height     : in     Integer                              := 1;
      Auto_Size  : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  If Auto_Size is true, the control will size to the width and height
   --  of the first frame of the AVI file.

   -------------------------------------------------------------------------
   --  Animation_Control_Type - Methods
   -------------------------------------------------------------------------

   procedure Open (Control : in out Animation_Control_Type;
                   Name    : in     GString);
   --  Name is the name of a resource (use #XXX for numeric resources) or a
   --  path to an AVI file. The AVI file must be uncompressed
   --  or run length encoded and NOT contain and audio track

   procedure Close (Control : in out Animation_Control_Type);

   procedure Play (Control     : in out Animation_Control_Type;
                   Repeat      : in     Integer                := 1;
                   Start_Frame : in     Natural                := 0;
                   End_Frame   : in     Integer                := -1);
   --  End_Frame of -1 to play until end
   --  Use repeat value of -1 for never ending loop

   procedure Seek (Control : in out Animation_Control_Type;
                   Frame   : in     Natural);
   --  Display Frame of AVI

   procedure Stop (Control : in out Animation_Control_Type);

   -------------------------------------------------------------------------
   --  Animation_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Start_Handler (Control : in out Animation_Control_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Start (Control : in out Animation_Control_Type);

   procedure On_Stop_Handler (Control : in out Animation_Control_Type;
                              Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Stop (Control : in out Animation_Control_Type);

   -------------------------------------------------------------------------
   --  Animation_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Start (Control : in out Animation_Control_Type);

   procedure On_Stop (Control : in out Animation_Control_Type);

   -------------------------------------------------------------------------
   --  Animation_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Command (Window  : in out Animation_Control_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type
   -------------------------------------------------------------------------

   type Date_Time_Picker_Type is new Common_Control_Type with private;
   type Date_Time_Picker_Access is access all Date_Time_Picker_Type;
   type Pointer_To_Date_Time_Picker_Class is
     access all Date_Time_Picker_Type'Class;

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type - Creation Methods
   -------------------------------------------------------------------------

   type Date_Format is (Long_Format, Short_Format, Time_Format);
   type Date_Selection_Method is (Up_Down, Calendar);

   procedure Create
     (Control    : in out Date_Time_Picker_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Format     : in     Date_Format                          := Long_Format;
      Method     : in     Date_Selection_Method                := Calendar;
      None_OK    : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type - Properties
   -------------------------------------------------------------------------

   procedure Date_Time_Format (Control : in out Date_Time_Picker_Type;
                               Format  : in     GString);
   --  Allows for a custom date/time format
   --
   --  Element Description
   --   "d"     The one- or two-digit day.
   --   "dd"    The two-digit day. Single-digit day values are preceded by a
   --           zero.
   --  "ddd"    The three-character weekday abbreviation.
   --  "dddd"   The full weekday name.
   --  "h"      The one- or two-digit hour in 12-hour format.
   --  "hh"     The two-digit hour in 12-hour format.
   --           Single-digit values are preceded by a zero.
   --  "H"      The one- or two-digit hour in 24-hour format.
   --  "HH"     The two-digit hour in 24-hour format.
   --           Single-digit values are preceded by a zero.
   --  "m"      The one- or two-digit minute.
   --  "mm"     The two-digit minute.
   --           Single-digit values are preceded by a zero.
   --  "M"      The one- or two-digit month number.
   --  "MM"     The two-digit month number.
   --           Single-digit values are preceded by a zero.
   --  "MMM"    The three-character month abbreviation.
   --  "MMMM"   The full month name.
   --  "t"      The one-letter AM/PM abbreviation
   --           (that is, AM is displayed as "A").
   --  "tt"     The two-letter AM/PM abbreviation
   --          (that is, AM is displayed as "AM").
   --  "yy"     The last two digits of the year
   --          (that is, 1996 would be displayed as "96").
   --  "yyyy"   The full year (that is, 1996 would be displayed as "1996").
   --
   --   To make the information more readable, you can add body text to
   --   the format string by enclosing it in single quotes. Spaces and
   --   punctuation marks do not need to be quoted.
   --
   --   Note Nonformat characters that are not delimited by single
   --   quotes will result in unpredictable display by the DTP control.
   --
   --   For example, to display the current date with the format "'Today
   --   is: 04:22:31 Tuesday Mar 23, 1996", the format string is "'Today
   --   is: 'hh':'m':'s dddd MMM dd', 'yyyy". To include a single quote
   --   in your body text, use two consecutive single quotes. For
   --   example, "'Don''t forget' MMM dd',' yyyy" produces output that
   --   looks like: Don't forget Mar 23, 1996. It is not necessary to
   --   use quotes with the comma, so "'Don''t forget' MMM dd, yyyy" is
   --   also valid, and produces the same output.

   procedure Set_Range (Control     : in out Date_Time_Picker_Type;
                        Range_Start : in     Ada.Calendar.Time;
                        Range_End   : in     Ada.Calendar.Time);
   --  Set range of date/times for control

   procedure Date_Time (Control  : in out Date_Time_Picker_Type;
                        Date_Time : in     Ada.Calendar.Time);
   function Date_Time (Control : in Date_Time_Picker_Type)
                      return Ada.Calendar.Time;
   --  Set Date/Time

   procedure None_Date_Time (Control : in out Date_Time_Picker_Type);
   function None_Date_Time (Control : in Date_Time_Picker_Type)
                           return Boolean;
   --  If the None_OK option was selected during create, sets None status
   --  or returns None Status

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Date_Time_Change_Handler
     (Control : in out Date_Time_Picker_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Date_Time_Change
     (Control : in out Date_Time_Picker_Type);

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Date_Time_Change (Control : in out Date_Time_Picker_Type);

   -------------------------------------------------------------------------
   --  Date_Time_Picker_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Notify
     (Window       : in out Date_Time_Picker_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);
   --  Handles Notify Messages

   procedure On_Create (Control : in out Date_Time_Picker_Type);
   --  Sets control as a tab stop

   function Recommended_Size (Control : in Date_Time_Picker_Type)
                             return GWindows.Types.Size_Type;

   -------------------------------------------------------------------------
   --  IP_Address_Control_Type
   -------------------------------------------------------------------------

   type IP_Address_Control_Type is new Common_Control_Type with private;
   type IP_Address_Control_Access is access all IP_Address_Control_Type;
   type Pointer_To_IP_Address_Control_Class is
     access all IP_Address_Control_Type'Class;

   -------------------------------------------------------------------------
   --  IP_Address_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control    : in out IP_Address_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  IP_Address_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event
   --  Only supports On_Focus, On_Lost_Focus, and On_Change

   procedure On_Change_Handler (Control    : in out IP_Address_Control_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Change (Control : in out IP_Address_Control_Type);

   -------------------------------------------------------------------------
   --  IP_Address_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Change (Control : in out IP_Address_Control_Type);
   --  Change made

   -------------------------------------------------------------------------
   --  IP_Address_Control_Type - Event Framework Handlers
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Command (Window  : in out IP_Address_Control_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles event dispatch

   procedure On_Create (Control : in out IP_Address_Control_Type);
   --  Sets control as a tab stop

   -------------------------------------------------------------------------
   --  Progress_Control_Type
   -------------------------------------------------------------------------

   type Progress_Control_Type is new Common_Control_Type with private;
   type Progress_Control_Access is access all Progress_Control_Type;
   type Pointer_To_Progress_Control_Class is
     access all Progress_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Progress_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   type Progress_Control_Direction_Type is (Horizontal, Vertical);

   procedure Create
     (Control    : in out Progress_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Direction  : in     Progress_Control_Direction_Type      := Vertical;
      Smooth     : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Progress_Control_Type - Properties
   -------------------------------------------------------------------------

   procedure Position (Control : in out Progress_Control_Type;
                       Where   : in     Natural);
   function Position (Control : in Progress_Control_Type) return Natural;
   --  Progress position

   procedure Progress_Range (Control : in out Progress_Control_Type;
                             Low     : in     Natural;
                             High    : in     Natural);
   --  Progress range

   procedure Increment (Control : in out Progress_Control_Type;
                        Amount  : in     Natural               := 1);
   --  Increment position

   procedure Step_Size (Control : in out Progress_Control_Type;
                        Size    : in     Natural               := 10);
   --  Set step size for stepping

   -------------------------------------------------------------------------
   --  Progress_Control_Type - Methods
   -------------------------------------------------------------------------

   procedure Step (Control : in out Progress_Control_Type);
   --  Step increment progress
   --  If step goes over high bound, progress starts over from beginning

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type
   -------------------------------------------------------------------------

   type Up_Down_Control_Type is new Common_Control_Type with private;
   type Up_Down_Control_Access is access all Up_Down_Control_Type;
   type Pointer_To_Up_Down_Control_Class is
     access all Up_Down_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   type Up_Down_Control_Direction_Type is (Horizontal, Vertical);
   type Up_Down_Control_Align_Type is (Align_Left, Align_Right);

   procedure Create
     (Control    : in out Up_Down_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Keyboard   : in     Boolean                              := True;
      Direction  : in     Up_Down_Control_Direction_Type       := Vertical;
      Wrap       : in     Boolean                              := False;
      Auto_Buddy : in     Boolean                              := True;
      Send_Int   : in     Boolean                              := True;
      Thousands  : in     Boolean                              := True;
      Align      : in     Up_Down_Control_Align_Type           := Align_Right;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  If Auto_Buddy is true, it attaches to the last created window (ie. the
   --  last window in the Z-Order)

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Properties
   -------------------------------------------------------------------------

   procedure Position
     (Control : in out Up_Down_Control_Type;
      Where   : in     Integer);
   function Position (Control : in Up_Down_Control_Type) return Integer;
   --  Position of control

   procedure Set_Range
     (Control  : in out Up_Down_Control_Type;
      Min, Max : in     Integer);
   --  Set range of control

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Event Types
   -------------------------------------------------------------------------

   type Up_Down_Changing_Event is access
     procedure (Window         : in out GWindows.Base.Base_Window_Type'Class;
                Position       : in     Integer;
                Delta_Position : in     Integer);

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Position_Changing_Handler
     (Control : in out Up_Down_Control_Type;
      Handler : in     Up_Down_Changing_Event);
   procedure Fire_On_Position_Changing
     (Control        : in out Up_Down_Control_Type;
      Position       : in     Integer;
      Delta_Position : in     Integer);

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Position_Changing
     (Window         : in out Up_Down_Control_Type;
      Position       : in     Integer;
      Delta_Position : in     Integer);
   --  Position is about to change

   -------------------------------------------------------------------------
   --  Up_Down_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Notify
     (Window       : in out Up_Down_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);
   --  Handles Notify Messages

   procedure On_Create (Control : in out Up_Down_Control_Type);
   --  Sets control as a tab stop

   -------------------------------------------------------------------------
   --  Trackbar_Control_Type
   -------------------------------------------------------------------------

   type Trackbar_Control_Type is new Common_Control_Type with private;
   type Trackbar_Control_Access is access all Trackbar_Control_Type;
   type Pointer_To_Trackbar_Control_Class is
     access all Trackbar_Control_Type'Class;
   --  Note: Trackbar movements are sent as scroll bar messages

   -------------------------------------------------------------------------
   --  Trackbar_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   type Trackbar_Control_Direction_Type is (Horizontal, Vertical);
   type Trackbar_Control_Ticks_Type is (Top_Ticks,
                                        Bottom_Ticks,
                                        Both_Ticks,
                                        No_Ticks);

   procedure Create
     (Control    : in out Trackbar_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Where      : in     Trackbar_Control_Ticks_Type          := No_Ticks;
      Direction  : in     Trackbar_Control_Direction_Type      := Horizontal;
      Thumb      : in     Boolean                              := True;
      Tips       : in     Boolean                              := True;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Trackbar_Control_Type - Properties
   -------------------------------------------------------------------------

   procedure Position
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer);
   function Position (Control : in Trackbar_Control_Type) return Integer;
   --  Position of control

   procedure Minimum
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer);
   function Minimum (Control : in Trackbar_Control_Type) return Integer;
   --  Minimum range of control

   procedure Maximum
     (Control : in out Trackbar_Control_Type;
      Where   : in     Integer);
   function Maximum (Control : in Trackbar_Control_Type) return Integer;
   --  Maximum range of control

   -------------------------------------------------------------------------
   --  Trackbar_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Control : in out Trackbar_Control_Type);
   --  Sets control as a tab stop

   -------------------------------------------------------------------------
   --  List_View_Control_Type
   -------------------------------------------------------------------------

   type List_View_Control_Type is new Common_Control_Type with private;
   type List_View_Control_Access is access all List_View_Control_Type;
   type Pointer_To_List_View_Control_Class is
     access all List_View_Control_Type'Class;

   -------------------------------------------------------------------------
   --  List_View_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   type List_View_Control_View_Type is (Icon_View,
                                        Small_Icon_View,
                                        List_View,
                                        Report_View);

   type List_View_Control_Alignment_Type is (Align_Top,
                                             Align_Left,
                                             Align_None);

   type List_View_Control_Sort_Type is (Sort_Ascending,
                                        Sort_Descending,
                                        Sort_Custom,
                                        No_Sorting);

   type List_View_Control_Select_Type is (Single,
                                          Multiple);

   procedure Create
     (Control    : in out List_View_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Selection  : in     List_View_Control_Select_Type        := Single;
      View       : in     List_View_Control_View_Type          := List_View;
      Sort       : in     List_View_Control_Sort_Type          := No_Sorting;
      Arrange    : in     Boolean                              := True;
      Align      : in     List_View_Control_Alignment_Type     := Align_Left;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  List_View_Control_Type - Properties
   -------------------------------------------------------------------------

   function Item_Count (Control : in List_View_Control_Type) return Integer;

   function Selected_Item_Count (Control : in List_View_Control_Type)
                                return Integer;

   function Is_Selected (Control : in List_View_Control_Type;
                         Index   : in Integer)
                        return Boolean;

   procedure Selected
     (Control : in out List_View_Control_Type;
      Item    : in     Integer;
      State   : in     Boolean);
   --  Sets the selected state of a row

   procedure Item_At_Position
     (Control  : in     List_View_Control_Type;
      Position : in     GWindows.Types.Point_Type;
      Item     : in out Integer;
      SubItem  : in out Integer);
   --  Item under position (position is in client coordinates)

   function Text
     (Control : in List_View_Control_Type;
      Item    : in Integer;
      SubItem : in Integer)
     return GString;
   --  Text at Item and SubItem

   -------------------------------------------------------------------------
   --  List_View_Control_Type - Methods
   -------------------------------------------------------------------------

   procedure Set_Item (Control : in out List_View_Control_Type;
                       Text    : in GString;
                       Index   : in Integer;
                       Icon    : in Integer := 0);

   procedure Set_Sub_Item (Control   : in out List_View_Control_Type;
                           Text      : in     GString;
                           Index     : in     Integer;
                           Sub_Index : in     Integer);

   procedure Insert_Item (Control      : in out List_View_Control_Type;
                          Text         : in  GString;
                          Index        : in  Integer;
                          Sorted_Index : out Integer;
                          Icon         : in  Integer := 0);

   procedure Insert_Item (Control : in out List_View_Control_Type;
                          Text    : in GString;
                          Index   : in Integer;
                          Icon    : in Integer := 0);

   procedure Delete_Item (Control : in out List_View_Control_Type;
                          Index   : in     Integer);

   procedure Set_Column (Control : in out List_View_Control_Type;
                         Text    : in     GString;
                         Index   : in     Integer;
                         Width   : in     Integer);

   procedure Insert_Column (Control : in out List_View_Control_Type;
                            Text    : in     GString;
                            Index   : in     Integer;
                            Width   : in     Integer);

   procedure Set_Column_Width
     (Control : in out List_View_Control_Type;
      Index   : in     Integer;
      Width   : in     Integer);

   function Column_Width
     (Control : in List_View_Control_Type;
      Index   : in Integer)
     return Integer;

   procedure Clear (Control : in out List_View_Control_Type);

   -------------------------------------------------------------------------
   --  List_View_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Control : in out List_View_Control_Type);
   --  Sets control as a tab stop and with a border

   procedure On_Item_Changed (Control : in out List_View_Control_Type);

   procedure On_Item_Changed_Handler
      (Control : in out List_View_Control_Type;
       Handler : in     GWindows.Base.Action_Event);

   procedure Fire_On_Item_Changed (Control : in out List_View_Control_Type);

   procedure On_Notify
     (Window       : in out List_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);

   --------------
   -- AnSp: Next List_View_Control_Type functions are added --
   --------------

   type List_View_Image_Type is (Normal, Small, State);
   procedure Set_Image_List
     (Control   : in out List_View_Control_Type;
      ImageType : in     List_View_Image_Type;
      List      : in     GWindows.Image_Lists.Image_List_Type);

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type
   -------------------------------------------------------------------------

   type Tree_View_Control_Type is new Common_Control_Type with private;
   type Tree_View_Control_Access is access all Tree_View_Control_Type;
   type Pointer_To_Tree_View_Control_Class is
     access all Tree_View_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control       : in out Tree_View_Control_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left          : in     Integer;
      Top           : in     Integer;
      Width         : in     Integer;
      Height        : in     Integer;
      Buttons       : in     Boolean                              := True;
      Lines         : in     Boolean                              := True;
      Lines_At_Root : in     Boolean                              := True;
      Single_Expand : in     Boolean                              := False;
      Show          : in     Boolean                              := True;
      Is_Dynamic    : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Properties
   -------------------------------------------------------------------------

   type Tree_Item_Node is new GWindows.Types.Lparam;

   type Tree_View_List_Location_Type is (First, Last, Sort, As_A_Root);

   type TVITEM is
      record
         Mask           : Interfaces.C.unsigned := 0;
         HItem          : Tree_Item_Node        := 0;
         State          : Interfaces.C.unsigned := 0;
         State_Mask     : Interfaces.C.unsigned := 0;
         Text           : LPTSTR := null;
         TextMax        : Integer := 0;
         Image          : Integer := 0;
         Selected_Image : Integer := 0;
         Children       : Integer := 0;
         LPARAM         : GWindows.Types.Lparam := 0;
      end record;

   function Selected_Item (Control : in Tree_View_Control_Type)
                          return GString;

   function Selected_Item (Control : in Tree_View_Control_Type)
                          return Tree_Item_Node;

   function Text (Control : in Tree_View_Control_Type;
                  Where   : in Tree_Item_Node)
                 return GString;

   --  GdM: procedure Text (with node) added 2-Jun-2009, uses AnSp's Set_Item
   procedure Text (Control : in out Tree_View_Control_Type;
                   Where   : in     Tree_Item_Node;
                   Text    : in     GString);

   function Get_Root_Item (Control : in Tree_View_Control_Type)
                          return Tree_Item_Node;

   function Get_Parent_Item (Control : in Tree_View_Control_Type;
                             From    : in Tree_Item_Node)
                            return Tree_Item_Node;

   function Get_First_Child_Item (Control : in Tree_View_Control_Type;
                                  From    : in Tree_Item_Node)
                                 return Tree_Item_Node;

   function Get_Next_Item (Control : in Tree_View_Control_Type;
                           From    : in Tree_Item_Node)
                          return Tree_Item_Node;

   function Get_Previous_Item (Control : in Tree_View_Control_Type;
                               From    : in Tree_Item_Node)
                              return Tree_Item_Node;

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Methods
   -------------------------------------------------------------------------

   procedure Insert_Item
     (Control     : in out Tree_View_Control_Type;
      Text        : in     GString;
      Parent_Node : in     Tree_Item_Node;
      New_Node    :    out Tree_Item_Node;
      Where       : in     Tree_Item_Node);

   procedure Insert_Item
     (Control     : in out Tree_View_Control_Type;
      Text        : in     GString;
      Parent_Node : in     Tree_Item_Node;
      New_Node    :    out Tree_Item_Node;
      Where       : in     Tree_View_List_Location_Type := Sort);

   procedure Delete_Item
     (Control : in out Tree_View_Control_Type;
      Where   : in     Tree_Item_Node);

   procedure Expand
     (Control     : in out Tree_View_Control_Type;
      At_Node     : in     Tree_Item_Node);

   procedure Collapse
     (Control     : in out Tree_View_Control_Type;
      At_Node     : in     Tree_Item_Node);

   --  GdM: Select_Item added 21-May-2009

   procedure Select_Item
     (Control     : in out Tree_View_Control_Type;
      Node        : in     Tree_Item_Node);

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Control : in out Tree_View_Control_Type);
   --  Sets control as a tab stop and with a border

   --------------
   --  AnSp: Next Tree_View_Control_Type functions are added --
   --------------

   function Get_Count
      (Control : in Tree_View_Control_Type)
      return Integer;

   procedure Set_Item
     (Control       : in out Tree_View_Control_Type;
      Where         : in     Tree_Item_Node;
      Mask          : in     Integer;
      Text          : in     GString;
      Image         : in     Integer;
      SelectedImage : in     Integer;
      State         : in     Integer;
      StateMask     : in     Integer;
      Param         : in     Integer);

   procedure Set_Image_List
     (Control : in out Tree_View_Control_Type;
      List    : in     GWindows.Image_Lists.Image_List_Type);

   procedure Set_Image
     (Control     : in out Tree_View_Control_Type;
      Where       : in     Tree_Item_Node;
      Image       : in     Integer;
      ImageSelect : in     Integer);

   --  GdM: Tree_View_Control_Type handlers added 2-Jun-2009
   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Selection_Change_Handler
     (Control : in out Tree_View_Control_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Selection_Change
     (Control : in out Tree_View_Control_Type);

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Selection_Change (Control : in out Tree_View_Control_Type);

   -------------------------------------------------------------------------
   --  Tree_View_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Notify
     (Window       : in out Tree_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);

   -------------------------------------------------------------------------
   --  Tab_Control_Type
   -------------------------------------------------------------------------

   type Tab_Control_Type is new Common_Control_Type with private;
   type Tab_Control_Access is access all Tab_Control_Type;
   type Pointer_To_Tab_Control_Class is
     access all Tab_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control    : in out Tab_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Fixed_Tabs : in     Boolean                              := False;
      Multi_Line : in     Boolean                              := False;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Properties
   -------------------------------------------------------------------------

   procedure Text (Control : in out Tab_Control_Type;
                   Where   : in     Integer;
                   Value   : in     GString);
   function Text (Control : in Tab_Control_Type;
                  Where   : in Integer)
                 return GString;

   function Tab_Count (Control : in Tab_Control_Type) return Integer;

   function Tab_Row_Count (Control : in Tab_Control_Type) return Integer;

   procedure Selected_Tab (Control : in out Tab_Control_Type;
                           Where   : in     Integer);
   function Selected_Tab (Control : in Tab_Control_Type) return Integer;

   function Display_Area (Control : in Tab_Control_Type)
                         return GWindows.Types.Rectangle_Type;

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Methods
   -------------------------------------------------------------------------

   procedure Insert_Tab (Control : in out Tab_Control_Type;
                         Where   : in     Integer;
                         Value   : in     GString);

   procedure Delete_Tab (Control : in out Tab_Control_Type;
                         Where   : in     Integer);

   procedure Delete_All_Tabs (Control : in out Tab_Control_Type);

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Change_Handler (Control    : in out Tab_Control_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Change (Control : in out Tab_Control_Type);

   procedure On_Changing_Handler (Control    : in out Tab_Control_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Changing (Control : in out Tab_Control_Type);

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Change (Control : in out Tab_Control_Type);
   --  Tab page changed

   procedure On_Changing (Control : in out Tab_Control_Type);
   --  Tab page changingd

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Control : in out Tab_Control_Type);
   --  Sets control as a tab stop

   procedure On_Notify
     (Window       : in out Tab_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);
   --  Handles Notify Messages

   -------------------------------------------------------------------------
   --  Tab_Window_Control_Type
   -------------------------------------------------------------------------

   type Tab_Window_Control_Type is new Tab_Control_Type with private;
   type Tab_Window_Control_Access is access all Tab_Window_Control_Type;
   type Pointer_To_Tab_Window_Control_Class is
     access all Tab_Window_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Tab_Window_Control_Type - Properties
   -------------------------------------------------------------------------

   procedure Tab_Window
     (Control : in out Tab_Window_Control_Type;
      Where   : in     Integer;
      Window  : in     GWindows.Base.Pointer_To_Base_Window_Class);

   function Tab_Window (Control : in Tab_Window_Control_Type;
                        Where   : in Integer)
                       return GWindows.Base.Pointer_To_Base_Window_Class;
   --  Get or Set the window associated with a tab

   -------------------------------------------------------------------------
   --  Tab_Window_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Changing (Control : in out Tab_Window_Control_Type);
   --  Handle changes to tab window (hide old window)

   procedure On_Change (Control : in out Tab_Window_Control_Type);
   --  Handle changes to tab window (show new window)

   -------------------------------------------------------------------------
   --  Toolbar_Control_Type
   -------------------------------------------------------------------------

   type Toolbar_Control_Type is new Common_Control_Type with private;
   type Toolbar_Control_Access is access all Toolbar_Control_Type;
   type Pointer_To_Toolbar_Control_Class is
     access all Toolbar_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Toolbar_Control_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control    : in out Toolbar_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Toolbar_Control_Type - Methods
   -------------------------------------------------------------------------

   --  IDs for standard image lists
   IDB_STD_SMALL_COLOR  : constant := 0;
   IDB_STD_LARGE_COLOR  : constant := 1;
   IDB_VIEW_SMALL_COLOR : constant := 4;
   IDB_VIEW_LARGE_COLOR : constant := 5;

   --  Toolbar Standard Button Image Index Values
   --  for IDB_STD_LARGE_COLOR and IDB_STD_SMALL_COLOR
   STD_CUT        : constant := 0;
   STD_COPY       : constant := 1;
   STD_PASTE      : constant := 2;
   STD_UNDO       : constant := 3;
   STD_REDOW      : constant := 4;
   STD_DELETE     : constant := 5;
   STD_FILENEW    : constant := 6;
   STD_FILEOPEN   : constant := 7;
   STD_FILESAVE   : constant := 8;
   STD_PRINTPRE   : constant := 9;
   STD_PROPERTIES : constant := 10;
   STD_HELP       : constant := 11;
   STD_FIND       : constant := 12;
   STD_REPLACE    : constant := 13;
   STD_PRINT      : constant := 14;

   --  Toolbar Standard Button Image Index Values
   --  for IDB_VIEW_LARGE_COLOR and IDB_VIEW_SMALL_COLOR
   VIEW_LARGEICONS    : constant := 0;
   VIEW_SMALLICONS    : constant := 1;
   VIEW_LIST          : constant := 2;
   VIEW_DETAILS       : constant := 3;
   VIEW_SORTNAME      : constant := 4;
   VIEW_SORTSIZE      : constant := 5;
   VIEW_SORTDATE      : constant := 6;
   VIEW_SORTTYPE      : constant := 7;
   VIEW_PARENTFOLDER  : constant := 8;
   VIEW_NETCONNECT    : constant := 9;
   VIEW_NETDISCONNECT : constant := 10;
   VIEW_NEWFOLDER     : constant := 11;

   procedure Load_Image_List
     (Control   : in out Toolbar_Control_Type;
      Bitmap_ID : in     Integer);

   procedure Set_Image_List
     (Control : in out Toolbar_Control_Type;
      List    : in     GWindows.Image_Lists.Image_List_Type);

   procedure Add_Image_List
     (Control   : in out Toolbar_Control_Type;
      Bitmap_ID : in     Integer;
      Index     :    out Integer);

   --  AlKe: added
   --  Text can be a series of strings seperated by GCharacter'Val (0),
   --  e.g. to be used for button labels or tooltips.
   --  See Add_Button below with IString.
   procedure Add_String
     (Control     : in out Toolbar_Control_Type;
      Text        : in     GString);

   procedure Add_Button
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer);

   --  AlKe: added
   procedure Add_Button
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer;
      IString     : in     Integer);

   --  AlKe: added
   procedure Set_Button_Image
     (Control     : in out Toolbar_Control_Type;
      Image_Index : in     Natural;
      Command     : in     Integer);

   --  AnSp: added
   type Toolbar_Button_State is (Normal, Hidden, Disabled);

   procedure Set_Button_State
     (Control     : in out Toolbar_Control_Type;
      Command     : in     Integer;
      State       : in     Toolbar_Button_State);

   procedure Visible (Control : in out Toolbar_Control_Type;
                      Command : in     Integer;
                      State   : in     Boolean := True);
   function Visible (Control : in Toolbar_Control_Type;
                     Command : in Integer) return Boolean;
   --  Visible state of button

   procedure Enabled (Control : in out Toolbar_Control_Type;
                      Command : in     Integer;
                      State   : in     Boolean := True);
   function Enabled (Control : in Toolbar_Control_Type;
                     Command : in Integer) return Boolean;
   --  Enabled state of button

   --  AlKe: added
   procedure Button_Size (Control : in     Toolbar_Control_Type;
                          Width   :    out Integer;
                          Height  :    out Integer);

   procedure Add_Separator
     (Control : in out Toolbar_Control_Type;
      Width   : in     Integer);

   function Get_Style
     (Control    : in Toolbar_Control_Type)
     return Interfaces.C.unsigned;

   procedure Set_Style
     (Control    : in out Toolbar_Control_Type;
      Style      : in     Interfaces.C.unsigned);

   procedure Set_Extended_Style
     (Control    : in out Toolbar_Control_Type;
      Style      : in     Interfaces.C.unsigned);

   --  * AnSp: Added constants and functions to get/set styles and states
   --  *       of buttons on the toolbar. Think of check style and enable.
   --  Toolbar button styles
   TBSTYLE_BUTTON        : constant := 16#0000#;
   TBSTYLE_SEP           : constant := 16#0001#;
   TBSTYLE_CHECK         : constant := 16#0002#;
   TBSTYLE_GROUP         : constant := 16#0004#;
   TBSTYLE_CHECKGROUP    : constant := TBSTYLE_GROUP + TBSTYLE_CHECK;
   TBSTYLE_DROPDOWN      : constant := 16#0008#;
   TBSTYLE_AUTOSIZE      : constant := 16#0010#;
   TBSTYLE_NOPREFIX      : constant := 16#0020#;

   function Get_Button_Style
     (Control    : in Toolbar_Control_Type;
      Button     : in Integer)
     return Integer;

   procedure Set_Button_Style
     (Control    : in out Toolbar_Control_Type;
      Button     : in     Integer;
      Style      : in     Integer);

   --  Toolbar button states
   TBSTATE_CHECKED       : constant := 16#01#;
   TBSTATE_PRESSED       : constant := 16#02#;
   TBSTATE_ENABLED       : constant := 16#04#;
   TBSTATE_HIDDEN        : constant := 16#08#;
   TBSTATE_INDETERMINATE : constant := 16#10#;
   TBSTATE_WRAP          : constant := 16#20#;
   TBSTATE_ELLIPSES      : constant := 16#40#;
   TBSTATE_MARKED        : constant := 16#80#;

   function Get_Button_State
     (Control    : in Toolbar_Control_Type;
      Button     : in Integer)
     return Integer;

   procedure Set_Button_State
     (Control    : in out Toolbar_Control_Type;
      Button     : in     Integer;
      State      : in     Integer);
   --  * AnSp: up to here

   -------------------------------------------------------------------------
   --  Toolbar_Control_Type - Event Handlres
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Button_Select_Handler
     (Control : in out Toolbar_Control_Type;
      Handler : in GWindows.Windows.Select_Event);

   procedure Fire_On_Button_Select (Control : in out Toolbar_Control_Type;
                                    Item    : in     Integer);

   -------------------------------------------------------------------------
   --  Tab_Control_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Button_Select (Control : in out Toolbar_Control_Type;
                               Item    : in     Integer);
   --  Called when an item is selected in the toolbar

   -------------------------------------------------------------------------
   --  Toolbar_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Control : in out Toolbar_Control_Type);
   --  Setups control

   procedure On_Command
     (Window  : in out Toolbar_Control_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Dispatches button clicks

   -------------------------------------------------------------------------
   --  Tool_Tip_Type
   -------------------------------------------------------------------------

   type Tool_Tip_Type is new Common_Control_Type with private;
   type Tool_Tip_Access is access all Tool_Tip_Type;
   type Pointer_To_Tool_Tip_Class is
     access all Tool_Tip_Type'Class;

   -------------------------------------------------------------------------
   --  Tool_Tip_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Control    : in out Tool_Tip_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Is_Dynamic : in     Boolean                              := False);

   -------------------------------------------------------------------------
   --  Tool_Tip_Type - Properties
   -------------------------------------------------------------------------

   procedure Maximum_Width (Control : in out Tool_Tip_Type;
                                Width   : in     Integer);
   --  Maximum width of tool tip pop up

   -------------------------------------------------------------------------
   --  Tool_Tip_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class;
      Tip     : in     GString);
   --  Add this tool tip to show when hover over Window

   procedure Update_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class;
      Tip     : in     GString);
   --  Update tool tip to show when hover over Window

   procedure Delete_Tool_Tip
     (Control : in out Tool_Tip_Type;
      Window  : in     GWindows.Base.Base_Window_Type'Class);
   --  Delete tool tip to show when hover over Window

   procedure Get_Durations
     (Control  : in out Tool_Tip_Type;
      Initial  :    out Duration;
      Reshow   :    out Duration;
      Til_Hide :    out Duration);
   --  Get tooltip timing

   procedure Set_Durations
     (Control  : in out Tool_Tip_Type;
      Initial  : in     Duration;
      Reshow   : in     Duration;
      Til_Hide : in     Duration);
   --  Set tooltip timing

private
   type Common_Control_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Click_Event              : GWindows.Base.Action_Event := null;
         On_Double_Click_Event       : GWindows.Base.Action_Event := null;
         On_Right_Click_Event        : GWindows.Base.Action_Event := null;
         On_Right_Double_Click_Event : GWindows.Base.Action_Event := null;
         On_Return_Event             : GWindows.Base.Action_Event := null;
         On_Out_Of_Memory_Event      : GWindows.Base.Action_Event := null;
         On_Focus_Event              : GWindows.Base.Action_Event := null;
         On_Lost_Focus_Event         : GWindows.Base.Action_Event := null;
         On_Hover_Event              : GWindows.Base.Action_Event := null;
      end record;

   type Animation_Control_Type is new Common_Control_Type with
      record
         On_Start_Event : GWindows.Base.Action_Event := null;
         On_Stop_Event  : GWindows.Base.Action_Event := null;
      end record;

   type Status_Bar_Type is new Common_Control_Type with null record;

   type Date_Time_Picker_Type is new Common_Control_Type with
      record
         On_Date_Time_Change_Event : GWindows.Base.Action_Event := null;

         Format : GWindows.GString_Unbounded;
      end record;

   function Largest_Formatted_String (Item : in GWindows.GString)
                                     return GWindows.GString;

   type IP_Address_Control_Type is new Common_Control_Type with
      record
         On_Change_Event            : GWindows.Base.Action_Event := null;
      end record;

   type Progress_Control_Type is new Common_Control_Type with null record;

   type List_View_Control_Type is new Common_Control_Type with
      record
         On_Item_Changed_Event  : GWindows.Base.Action_Event := null;
      end record;

   type Tree_View_Control_Type is new Common_Control_Type with
      record
         On_Selection_Change_Event  : GWindows.Base.Action_Event := null;
      end record;

   type Trackbar_Control_Type is new Common_Control_Type with null record;

   type Up_Down_Control_Type is new Common_Control_Type with
      record
         On_Position_Changing_Event : Up_Down_Changing_Event;
      end record;

   type Tab_Control_Type is new Common_Control_Type with
      record
         On_Change_Event   : GWindows.Base.Action_Event := null;
         On_Changing_Event : GWindows.Base.Action_Event := null;
      end record;

   type Tab_Window_Control_Type is new Tab_Control_Type with null record;

   type Toolbar_Control_Type is new Common_Control_Type with
      record
         On_Button_Select_Event : GWindows.Windows.Select_Event := null;
      end record;

   type Tool_Tip_Type is new Common_Control_Type with null record;

end GWindows.Common_Controls;
