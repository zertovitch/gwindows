------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
-- G W I N D O W S . C O M M O N _ C O N T R O L S . E X _ L I S T _ V I E W--
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2007 - 2024 Falk Maier                     --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Colors;
with GWindows.Drawing_Objects;

with Interfaces.C;

generic
   type Data is private;

package GWindows.Common_Controls.Ex_List_View is

   type Data_Access is access all Data;

   type Ex_List_View_Control_Type is new List_View_Control_Type with private;
   type Ex_List_View_Control_Access is access all Ex_List_View_Control_Type;
   type Pointer_To_Ex_List_View_Control_Class is access all Ex_List_View_Control_Type'Class;

   Elv_Exception : exception;

   --  Return the colum count
   function Column_Count (Control : in Ex_List_View_Control_Type) return Natural;

   type Autosize_Type is (Columnsize, Headersize);
   --  Automatically adjust column width according to content
   procedure Autosize (Control : in out Ex_List_View_Control_Type;
                       Column  : in     Natural;
                       Sizing  : in     Autosize_Type := Headersize);

   -- Coloring ---------------------
   --
   --  All_Items = all items/subitems have the same text/backcolor
   --  Item_Alternately = the backcolor of items (complete rows) changes
   --                      line by line between two backcolors
   --  Subitem = the text/backcolor can set for each subitem separately
   --
   --  color_mode = All_Items is the best performing
   --  color_mode = Subitem is the least performant
   type Color_Mode_Type is (All_Items, Item_Alternately, Subitem);

   --  Get the active color-mode
   function Color_Mode (Control : in Ex_List_View_Control_Type) return Color_Mode_Type;

   use GWindows.Colors;

   --  Set the text/backcolor for color_mode = All_Items
   --  is another color_mode active, it automatically switches to color_mode = All_Items
   --  after create of the control the default text color is black and background color is white
   procedure Text_Color (Control : in out Ex_List_View_Control_Type;
                         Color   : in     Color_Type);
   procedure Back_Color (Control : in out Ex_List_View_Control_Type;
                         Color   : in     Color_Type);

   --  Set the background color of the control in the area where
   --  there are no items/subitems.
   --  The color_mode is not changed!
   procedure Control_Back_Color
      (Control : in  out  Ex_List_View_Control_Type;
       Color   : in       Color_Type);

   --  The default margin (in pixels) of text in headers may have evolved
   --  from a version of Windows to another.
   --  For Windows 10 it seems to be 4 (checked experimentally).
   Default_Header_Text_Top_Margin : constant := 2;

   --  Set custom header appearance.
   --  Call this method *after* having defined the columns to be used.
   procedure Customize_Header
      (Control                : in out Ex_List_View_Control_Type;
       Text_Color             : in     Color_Type;
       Back_Color             : in     Color_Type;
       Separator_Color        : in     Color_Type;
       Text_Top_Margin        : in     Natural := Default_Header_Text_Top_Margin;
       Force_Default_GUI_Font : in     Boolean := False);

   --  Set the backcolors for color_mode = item_alternately
   --  is another color_mode active, it automatically switches to color_mode = item_alternately
   procedure Set_Alternately_Colors
      (Control : in out Ex_List_View_Control_Type;
       Color1  : in     Color_Type;
       Color2  : in     Color_Type);

   --  Set the text/backcolors on subitem level.
   --  Is another color_mode active, it automatically switches
   --  to color_mode = subitem.
   --  When Sub_Index = -1, the colors are set for complete item (row).
   --  When Index = -1, the colors are set for complete column.
   --  When Index = -1 and Sub_Index = -1, an exception will raised ->
   --  for controlwide colors use the procedures Text_color() and Back_color()
   procedure Subitem_Color (Control    : in out Ex_List_View_Control_Type;
                            Text_Color : in     Color_Type := Black;
                            Back_Color : in     Color_Type := White;
                            Index      : in     Integer := -1;
                            Sub_Index  : in     Integer := -1);

   --  Change the color_mode manually
   --  when the mode is changed (either explicitly or utomatically by library) retained the old color settings,
   --  but applied again only when the associated color_mode is reactivated
   procedure Color_Mode (Control : in out Ex_List_View_Control_Type;
                         Mode    : in     Color_Mode_Type;
                         Redraw  : in     Boolean := True);

   -- Payload ----------------------------
   --
   --  set payload for index
   --  attention: passing a pointer to the type that was passed at instantiation
   --  the user of library is responsible for the lifetime of the payload data,
   --  the control manages the pointer only!
   procedure Item_Data (Control : in Ex_List_View_Control_Type;
                        Index   : in Natural;
                        Payload : in Data_Access);
   --  returns a pointer to payload data
   function Item_Data (Control : in Ex_List_View_Control_Type;
                       Index   : in Natural) return Data_Access;

   --  Method for freeing a row's payload.
   procedure On_Free_Payload (Control : in out Ex_List_View_Control_Type;
                              Payload :    out Data_Access);

   --  Alternative: use an event handler for freeing a row's payload.
   --  Define an event to free the payload by the user of the library.
   type Free_Payload_Event is access
     procedure (Control : in out Ex_List_View_Control_Type;
                Payload : out Data_Access);

   --  Here we can set the handler for the event.
   procedure On_Free_Payload_Handler (Control : in out Ex_List_View_Control_Type;
                                      Event   : in     Free_Payload_Event);

   -- Sorting ----------------------------
   --
   --  the gui sorting is started by clicking on the column head
   --  gui sorting is automatically active when the control is created with parameter sort /= no_sort

   --  Colors for painting the sort icon
   Sort_Icon_Pen_Color   : Color_Type := To_Color (100, 100, 100);
   Sort_Icon_Brush_Color : Color_Type := To_Color (100, 100, 100);

   --  The control use an alphabetical sorting by default.
   --  if you want another sorting then your own sorting routine can be set here
   --  if value1 > value2 the return 1
   --  if value1 = value2 then return 0
   --  if value1 < value2 then return -1

   --  There are different comparison techniques for sorting.
   --  Note that you can choose the technique depending on the column
   --  *before* calling Sort, since you know which column will be sorted.
   --  For instance, you may want to use `Using_Payloads` for a certain
   --  set of columns with dates or numbers, and As_Strings for other
   --  columns containing names. Note that the `Using_Payloads` approach
   --  can be around 100 times faster than the other techniques. For large
   --  lists (10,000 items or more) it is valuable to consider storing
   --  the strings into the payload (although it is duplication of data)
   --  and do the string comparison from the On_Compare method, technique
   --  #3 variant, instead of using `As_Strings` or `As_Strings_Default`
   --  techniques that do API calls for retrieving the strings from the
   --  widget for each comparison.
   --
   --  See AZip, the Zip archive manager (https://azip.sourceforge.io/),
   --  for experimenting (set the constant timing := True).
   --  You can use rt.jar (the Java runtime) as test data.

   type Comparison_Technique_Type is
      (As_Strings,           --  Use the On_Compare method with
                             --     *string* parameters (technique #1).
       General,              --  Use the On_Compare method with
                             --     *index* parameters (technique #2).
       Using_Payloads,       --  Use the On_Compare method with
                             --     *payload* parameters (technique #3).
       As_Strings_Default);  --  Use default string comparison, a fast
                             --     alphabetical / lexicographical comparison.

   --  Comparison technique 1:  custom comparison using ONLY the list view cells'
   --  ----------------------   contents (the Text function).
   --  `As_Strings`

   --  You can override On_Compare for a derived type, and define there your
   --  custom sorting.
   function On_Compare
      (Control : in Ex_List_View_Control_Type;
       Column  : in Natural;
       Value1  : in GString;
       Value2  : in GString) return Integer;

   --  Alternatively, you can dynamically set a handler.
   type Compare_Event is access
     function (Control : in Ex_List_View_Control_Type;
               Column : in Natural;
               Value1 : in GString;
               Value2 : in GString) return Integer;

   procedure On_Compare_Handler (Control : in out Ex_List_View_Control_Type;
                                 Event   : in     Compare_Event);

   --  Use the handler, if available.
   --  If not available, it defaults to alphabetical sorting.
   function Fire_On_Compare
      (Control : in Ex_List_View_Control_Type;
       Column  : in Natural;
       Value1  : in GString;
       Value2  : in GString) return Integer;

   --  Comparison technique 2:  custom comparison using the list view cells'
   --  ----------------------   coordinates. Depending on the column, you can
   --  `General`                opt to compare the Text, or use the Payload if
   --                           it is more practical or faster. For instance,
   --                           you may have numerical informations in some
   --                           columns. Then you can store the numbers
   --                           directly in the payload record.

   --  You can override On_Compare for a derived type, and define there your
   --  custom sorting.
   function On_Compare
      (Control : in Ex_List_View_Control_Type;
       Column  : in Natural;
       Index_1 : in Natural;
       Index_2 : in Natural) return Integer;

   --  Alternatively, you can dynamically set a handler.
   type General_Compare_Event is access
     function (Control : in Ex_List_View_Control_Type;
               Column  : in Natural;
               Index_1 : in Natural;
               Index_2 : in Natural) return Integer;

   procedure On_Compare_Handler (Control       : in out Ex_List_View_Control_Type;
                                 General_Event : in     General_Compare_Event);

   --  Use the handler, if available.
   --  If not available, it defaults to alphabetical sorting.
   function Fire_On_Compare
      (Control : in Ex_List_View_Control_Type;
       Column  : in Natural;
       Index_1 : in Natural;
       Index_2 : in Natural) return Integer;

   --  Comparison technique 3: custom comparison using ONLY the payloads.
   --  ----------------------
   --  `Using_Payloads`

   --  You can override On_Compare for a derived type, and define there your
   --  custom sorting.
   function On_Compare
     (Control   : in Ex_List_View_Control_Type;
      Column    : in Natural;
      Payload_1 : in Data;
      Payload_2 : in Data) return Integer;

   --  Alternatively, you can dynamically set a handler.
   type Payload_Compare_Event is access
     function (Control   : in Ex_List_View_Control_Type;
               Column    : in Natural;
               Payload_1 : in Data;
               Payload_2 : in Data) return Integer;

   procedure On_Compare_Handler (Control       : in out Ex_List_View_Control_Type;
                                 Payload_Event : in     Payload_Compare_Event);

   --  Use the handler, if available.
   --  If not available, it raises a Program_Error exception.
   function Fire_On_Compare
     (Control   : in Ex_List_View_Control_Type;
      Column    : in Natural;
      Payload_1 : in Data;
      Payload_2 : in Data) return Integer;

   type Sort_Direction_Type is (Up, Down, Auto);

   --  Start the sort by procedure call
   procedure Sort
     (Control    : in out Ex_List_View_Control_Type;
      Column     : in     Natural;
      Direction  : in     Sort_Direction_Type;
      Show_Icon  : in     Boolean := True;
      Technique  : in     Comparison_Technique_Type := As_Strings);

   --  Get the information of actual sorting.
   --  If sorting is inactive -> OUT-Parameter column = -1
   procedure Sort_Info (Control   : in     Ex_List_View_Control_Type;
                        Column    :    out Integer;
                        Direction :    out Sort_Direction_Type);

private

   NullColor : Color_Type := 17000000; -- out of RGB-range

   --  internal
   type Internal_Color_Type is record
      Textcolor : Color_Type := NullColor;
      Backcolor : Color_Type := NullColor;
   end record;
   type Internal_Color_Array_Type is array (Natural range <>) of Internal_Color_Type;
   type Internal_Color_Array_Access is access all Internal_Color_Array_Type;
   type Internal_Type is
      record
         Colors : Internal_Color_Array_Access := null;
         User_Data : Data_Access := null;
      end record;
   type Internal_Access is access all Internal_Type;
   pragma No_Strict_Aliasing (Internal_Access);

   type Sorting_Object is
      record
         Sort_Column : Integer := -1;
         Sort_Direction : Integer := 0;
         Icon_Visible : Boolean := True;
         Sort_Pen : Drawing_Objects.Pen_Type;
         Sort_Brush : Drawing_Objects.Brush_Type;
      end record;

   type Header_Object is record
      Separator_Pen          : Drawing_Objects.Pen_Type;
      Text_Top_Margin        : Natural := Default_Header_Text_Top_Margin;
      Text_Color             : Color_Type := Black;
      Back_Color             : Color_Type := White;
      Force_Default_GUI_Font : Boolean := False;
   end record;

   type Ex_List_View_Control_Type is new List_View_Control_Type with record
      Color_Mode : Color_Mode_Type := All_Items;
      List_Text_Color : Color_Type := Black;
      List_Back_Color : Color_Type := White;
      Alt_Color1 : Color_Type;
      Alt_Color2 : Color_Type;
      Comctl_Version : Natural := 0;
      Need_Custom_Sort_Icons : Boolean;  --  Initialized by On_Create.
      Want_Custom_Header : Boolean := False;
      Header : Header_Object;
      --  Events
      On_Free_Payload : Free_Payload_Event := null;
      On_Compare_Event         : Compare_Event := null;
      On_General_Compare_Event : General_Compare_Event := null;
      On_Payload_Compare_Event : Payload_Compare_Event := null;
      --  For callback
      Sort_Object : Sorting_Object;
   end record;

   procedure On_Create (Control : in out Ex_List_View_Control_Type);
   procedure On_Message (Control      : in out Ex_List_View_Control_Type;
                         message      : in     Interfaces.C.unsigned;
                         wParam       : in     GWindows.Types.Wparam;
                         lParam       : in     GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult);
   procedure On_Notify
     (Window       : in out Ex_List_View_Control_Type;
      Message      : in     Base.Pointer_To_Notification;
      Control      : in     Base.Pointer_To_Base_Window_Class;
      Return_Value : in out Types.Lresult);

   procedure On_Destroy (Control : in out Ex_List_View_Control_Type);
   procedure Delete_Item (Control : in out Ex_List_View_Control_Type;
                          Index   : in     Integer);
   procedure Clear (Control : in out Ex_List_View_Control_Type);

end GWindows.Common_Controls.Ex_List_View;
