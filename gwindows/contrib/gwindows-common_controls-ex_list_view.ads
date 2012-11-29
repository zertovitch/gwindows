------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
-- G W I N D O W S . C O M M O N _ C O N T R O L S . E X _ L I S T _ V I E W--
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2007 - 2012 Falk Maier                     --
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

with Gwindows.Colors; use gwindows.Colors;
with Gwindows.Drawing_Objects;

with Interfaces.C;

generic
   type Data is private;

package Gwindows.Common_Controls.Ex_List_View is
   pragma Linker_Options("-luxtheme");

   type Data_Access is access all Data;

   type Ex_List_View_Control_Type is new List_View_Control_Type with private;
   type Ex_List_View_Control_Access is access all Ex_List_View_Control_Type;
   type Pointer_To_Ex_List_View_Control_Class is access all Ex_List_View_Control_Type'Class;

   Elv_Exception: Exception;

   -- return the colum count
   function Column_Count(Control    : in Ex_List_View_Control_Type) return Natural;

   type Autosize_Type is (Columnsize, Headersize);
   -- automatically adjust column width according to content
   procedure Autosize(Control: in out Ex_List_View_Control_Type;
                      Column : in Natural;
                      Sizing: in Autosize_Type := headersize);

   -- styles
   -- all styles may be combined
   -- grid = gridlines between the subitems
   -- Headerdragdrop = move the columns with drag and drop
   -- fullrowselect = selection mark on complete row
   type Extended_Style_Type is (Grid, Headerdragdrop, Fullrowselect);
   procedure Set_Extended_Style (Control : in     Ex_List_View_Control_Type;
                                 Style   : in     Extended_Style_Type );
   procedure Remove_Extended_Style (Control : in     Ex_List_View_Control_Type;
                                      Style   : in     Extended_Style_Type );


   -- coloring ---------------------

   -- allitems = all items/subitems have the same text/backcolor
   -- item_alternately = the backcolor of items (complete rows) changes line by line between two backcolors
   -- subitem = the text/backcolor can set for each subitem separately
   -- color_mode = allitems is the best performing
   -- color_mode = subitem is the least performant
   type Color_Mode_Type is (allitems, Item_Alternately, Subitem);

   -- get the active color-mode
   function Color_Mode(Control : in Ex_List_View_Control_Type) return Color_Mode_Type;

   -- set the text/backcolor for color_mode = allitems
   -- is another color_mode active, it automatically switches to color_mode = AllItmes
   -- after create of the control the default text color is black and background color is white
   procedure Text_Color (Control : in  out Ex_List_View_Control_Type;
                         Color   : in     Color_Type                 );
   procedure back_Color (Control : in  out  Ex_List_View_Control_Type;
                         Color   : in     Color_Type                 );

   -- sets the background color of the control in the area, where there are no items/subitems
   -- the color_mode is not changed!
   procedure control_Back_Color (Control : in  out  Ex_List_View_Control_Type;
                                 Color   : in     Color_Type                 );


   -- set the backcolors for color_mode = item_alternately
   -- is another color_mode active, it automatically switches to color_mode = item_alternately
   procedure Set_Alternately_Colors(Control : in  out  Ex_List_View_Control_Type;
                                    Color1: in Color_Type;
                                    Color2:in Color_Type);

   -- set the text/backcolors on subitem level
   -- is another color_mode active, it automatically switches to color_mode = subitem
   -- when Sub_index = -1 the colors are set for complete item (row)
   -- when Index = -1 the colors are set for complete column
   -- when index = -1 and Sub_index = -1 an exception will raised ->
   -- for controlwide colors use the procedures Text_color() and Back_color()
   procedure subItem_Color (Control    : in out Ex_List_View_Control_Type;
                            Text_Color : in     Color_Type := black;
                            back_Color : in     Color_Type := white;
                            Index      : in     Integer := -1;
                            Sub_Index  : in     Integer := -1            );

   -- change the color-mode manually
   -- when the mode is changed (either explicitly or utomatically by library) retained the old color settings,
   -- but applied again only when the associated color_mode is reactivated
   procedure Color_Mode(Control : in out Ex_List_View_Control_Type;
                        Mode: in Color_Mode_Type;
                        Redraw: in Boolean := true);


   -- payload ----------------------------

   -- set payload for index
   -- attention: passing a pointer to the type that was passed at instantiation
   -- the user of library is responsible for the lifetime of the payload data,
   -- the control manages the pointer only!
   procedure Item_Data(Control : in Ex_List_View_Control_Type;
                       Index: in Natural;
                       Payload: in Data_access);
   -- returns a pointer to payload data
   function Item_Data(Control : in Ex_List_View_Control_Type;
                      Index: in Natural) return Data_access;

   -- event to free the payload by the user of the library
   type Free_Payload_event is access
     procedure (Control: in out Ex_List_View_Control_Type;
                Payload: out Data_access);

   -- event-handler for free payload
   procedure On_Free_Payload_Handler(Control: in out Ex_List_View_Control_Type;
                                     Event: in Free_Payload_Event);

   -- Sorting
   -- the gui sorting is started by clicking on the column head
   -- gui sorting is automatically active when the control is created with parameter sort /= no_sort

   -- colors for paint the sort icon
   Sort_Icon_Pen_Color: Color_Type := To_Color(100,100,100);
   Sort_Icon_brush_Color: Color_Type := To_Color(100,100,100);

   -- the control use an alphabetical sorting
   -- if you wants an another sorting then your own sorting routine can be set here
   -- if value1 > value2 the return 1
   -- if value1 = value2 then return 0
   -- if value1 < value2 then return -1
   type Compare_event is access
     function (Control: in ex_list_view_control_type;
               Column: in Natural;
               Value1: in Gstring;
               Value2: in Gstring) return Integer;

   procedure On_Compare_Handler(Control: in out Ex_List_View_Control_Type;
                                Event: in Compare_Event);


   type Sort_Direction_Type is (Up, Down, auto);
   -- start the sort by procedure call
   procedure Sort(Control: in out Ex_List_View_Control_Type;
                  Column: in Natural;
                  Direction: in Sort_Direction_Type;
                  Show_Icon: in Boolean := true);

private

   NullColor: Color_Type := 17000000; -- out of RGB-range

   -- internal
   type Internal_Color_Type is record
      Textcolor: Color_Type := nullcolor;
      Backcolor: Color_Type := nullcolor;
   end record;
   type Internal_Color_Array_Type is array (natural range <>) of Internal_color_Type;
   type Internal_Color_Array_Access is access all Internal_Color_Array_Type;
   type Internal_Type is
      record
         Colors: Internal_Color_Array_Access := null;
         User_Data: Data_Access := null;
      end record;
   type Internal_Access is access all Internal_Type;

   -- sorting_object
   type Sorting_Object is
      record
         Sort_Column: Integer := -1;
         Sort_Direction: Integer := 0;
         Icon_visible: Boolean := True;
         Sort_Pen: Gwindows.Drawing_Objects.Pen_Type;
         Sort_brush: Gwindows.Drawing_Objects.brush_Type;
      end record;

   -- control_type
   type Ex_List_View_Control_Type is new List_View_Control_Type with record
      Color_Mode: Color_Mode_Type := allitems;
      Control_Textcolor: Color_Type := Black;
      Control_backcolor: Color_Type := white;
      Alt_Color1: Color_Type;
      Alt_Color2: Color_Type;
      Comctl_Version: Natural := 0;
      -- events
      On_Free_Payload: Free_Payload_Event := null;
      On_Compare: Compare_Event := null;
      -- for callback
      Sort_Object: Sorting_Object;
   end record;

   procedure On_Create(Control: in out Ex_List_View_Control_Type);
   procedure On_Message(control       : in out Ex_List_View_Control_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     Gwindows.Types.wparam;
                        lParam       : in     Gwindows.Types.lparam;
                        Return_Value : in out Gwindows.Types.lresult);
   procedure On_Notify (Window       : in out Ex_List_View_Control_Type;
                        Message      : in     Gwindows.Base.Pointer_To_Notification;
                        Control      : in     Gwindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out gwindows.Types.lresult    );
   procedure On_Destroy (control : in out Ex_List_View_Control_Type);

end Gwindows.Common_Controls.Ex_List_View;
