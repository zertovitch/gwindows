------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     GWindows.Common_controls.Ex_lv                       --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------
--  This Package provides an Extension of the List_View_Control_TYpe
--  (David Botton). Actually the following features are implemented
--
--  -> Coloring on the subitem - level
--  -> Several extended styles (see below) are supported
--  -> items may be displayed with icons
--  -> Column sorting is supported
--  -> You may store data of Type T (generic parameter) at subitem level
------------------------------------------------------------------------------

with GWindows.Base; use GWindows.Base;
with GWindows.Colors; use GWindows.Colors;
with GWindows.Image_Lists.Ex_Image_Lists;
use GWindows.Image_Lists.Ex_Image_Lists;
with System.Address_To_Access_Conversions;

generic
   type T is private;
package GWindows.Common_Controls.Ex_LV_Generic is

   type Ex_List_View_Control_Type is new List_View_Control_Type with private;
   type Ex_List_View_Control_Access is access all Ex_List_View_Control_Type;
   type Pointer_To_Ex_List_View_Control_Class is private;

   procedure CreateEx
     (Control    : in out Ex_List_View_Control_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Color_Mode : in     Boolean                              := True;
      Selection  : in     List_View_Control_Select_Type        := Single;
      View       : in     List_View_Control_View_Type          := List_View;
      Sort       : in     List_View_Control_Sort_Type          := No_Sorting;
      Arrange    : in     Boolean                              := True;
      Align      : in     List_View_Control_Alignment_Type     := Align_Left;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  creates an instance of ex_list_view_control_type
   --  with parameter color_mode you may control the internal
   --  color-administration
   --  false -> no coloring of items or subitems possible
   --  true -> items and subitems may be colored individually
   --  items/subitems are given via parameters

   type List_View_Extended_Style_Type is
     (Grid, Flatsb, Headerdragdrop, Fullrowselect);
   --  all styles may be combined
   --  Flatsb = Flat Scrollbar

   procedure Set_Extended_Style
     (Control : in Ex_List_View_Control_Type;
      Style   : in List_View_Extended_Style_Type);
   --  sets an extended style

   procedure Remove_Extended_Style
     (Control : in Ex_List_View_Control_Type;
      Style   : in List_View_Extended_Style_Type);
   --  removes an extended style

   procedure Clear_Extended_Styles (Control : in Ex_List_View_Control_Type);
   --  removes all extended styles

   procedure Insert_Item (Control : in out Ex_List_View_Control_Type;
                          Text    : in     GString;
                          Index   : in     Integer;
                          Icon    : in     Integer                   := 0);
   --  inserts an item
   --  the parameter icon refers to an icon's index in the controls imagelist
   --  see below (set_image_list)

   procedure Delete_Item (Control : in out Ex_List_View_Control_Type;
                          Index   : in     Integer);
   --  remove an item

   procedure Set_Sub_Item (Control   : in out Ex_List_View_Control_Type;
                           Text      : in     GString;
                           Index     : in     Integer;
                           Sub_Index : in     Integer);
   --  sets a subitem

   procedure Set_Image_List (Control    : in Ex_List_View_Control_Type;
                             Image_List : in Ex_Image_List_Type);
   --  sets the controls imagelist

   type Sort_Direction_Type is (Up, Down, auto);

   procedure Sort_Column
     (Control        : in out Ex_List_View_Control_Type;
      Column         : in     Integer;
      Sort           : in     Sort_Direction_Type       := auto;
      Up_Sort_Icon   : in     Integer                   := 0;
      Down_Sort_Icon : in     Integer                   := 1);
   --  sorts with respect to a column
   --  sort = up -> sort ascending
   --  sort = down -> sort descending
   --  sort = auto -> sorts with the known "toggle behaviour":
   --   first call of sort_column sorts ascending
   --   further calls on the SAME column reverse order

   procedure Set_Text_Color (Control : in Ex_List_View_Control_Type;
                             Color   : in Color_Type);
   --  sets textcolor for the whole control
   --  no effect when Color_mode = true!

   procedure Set_TextBk_Color (Control : in out Ex_List_View_Control_Type;
                               Color   : in     Color_Type);
   --  sets background-textcolor for the whole control
   --  no effect when Color_mode = true!

   procedure Set_Item_Color (Control    : in out Ex_List_View_Control_Type;
                             Text_Color : in     Color_Type;
                             Bk_Color   : in     Color_Type;
                             Index      : in     Integer;
                             Sub_Index  : in     Integer);
   --  sets the color individually for an item/subitem depending on parameter
   --  color_mode in createEx

   procedure Set_Item_Data
     (Control   : in out Ex_List_View_Control_Type;
      Data      : in     T;
      Index     : in     Integer;
      Sub_Index : in     Integer;
      Redraw    : in     Boolean                   := False);
   --  sets the payload data individually for an item/subitem

   function Get_Item_Data (Control   : in Ex_List_View_Control_Type;
                           Index     : in Integer;
                           Sub_Index : in Integer)
                          return T;
   --  returns payload data acossiated with item

   procedure Column_Autosize (Control : in Ex_List_View_Control_Type;
                              Column  : in Integer);
   --  passt die Spaltenbreite automatisch an

   procedure Insert_Column (Control : in out Ex_List_View_Control_Type;
                            Text    : in     GString;
                            Index   : in     Integer;
                            Width   : in     Integer);
   --  inserts a column

   type Int_Array_Type is array (Positive range <>) of Integer;
   type Int_Array_Access is access all Int_Array_Type;

   procedure Set_Columnorder (Control     : in Ex_List_View_Control_Type;
                              Columnorder : in Int_Array_Type);
   --  setzt die Spaltenreihenfolge
   --  Achtung! Int_array'first = 1, int_array'last = Spaltenanzahl!

   procedure Get_Columnorder (Control     : in     Ex_List_View_Control_Type;
                              Columnorder : in out Int_Array_Type);
   --  liefert die Spaltenreihenfolge
   --  Achtung! Int_array'first = 1, int_array'last = Spaltenanzahl!

   function Get_Column_Count (Control : in Ex_List_View_Control_Type)
                             return Integer;
   --  liefert die Spaltenanzahl

   procedure Item_At_Position (Control  : in     Ex_List_View_Control_Type;
                               Position : in     GWindows.Types.Point_Type;
                               Item     : in out Integer;
                               Subitem  : in out Integer);
   --  liefert das item, daﬂ sich unter dem Point (Position) befindet
   --  notwendige ‹berschreibung, da die Original-Funktion in
   --  GWindows.common_controls FEHLERHAFT ist !

   ---------------------
   -- event-handling --
   ---------------------

   type Header_Click_Event is access procedure
     (Window : in out Ex_List_View_Control_Type;
      Column : in     Integer);

   procedure On_Header_Click (Control : in out Ex_List_View_Control_Type;
                              Column  : in     Integer);

   procedure On_Header_Click_Handler
     (Control : in out Ex_List_View_Control_Type;
      Handler : in     Header_Click_Event);

   procedure Fire_On_Header_Click
     (Control : in out Ex_List_View_Control_Type;
      Column  : in     Integer);

   type Item_Changed_Event is access procedure
     (Window    : in out Ex_List_View_Control_Type;
      index   : in Integer);

   procedure On_Item_Changed (Control : in out Ex_List_View_Control_Type;
                              index   : in     Integer);

   procedure On_Item_Changed_Handler
     (Control : in out Ex_List_View_Control_Type;
      Handler : in     Item_Changed_Event);

   procedure Fire_On_Item_Changed
     (Control : in out Ex_List_View_Control_Type;
      index   : in     Integer);

   procedure On_Create (Control : in out Ex_List_View_Control_Type);

   procedure On_Notify
     (Window       : in out Ex_List_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out Interfaces.C.long);

   procedure On_Destroy (Window : in out Ex_List_View_Control_Type);

private

   type Sort_Type is
      record
         Sort_Direction   : Sort_Direction_Type := Up;
         Column           : Integer             := -1;
      end record;

   type Extended_Data_Type is
      record
         ID          : Integer      := -1;
         Text_Color  : Color_Type   := To_Color (0, 0, 0);
         Back_Color  : Color_Type   := To_Color (255, 255, 255);
         More_Data   : T;
      end record;

   type Extended_Data_Access is access all Extended_Data_Type;

   type Extended_Data_Array_Type is
     array (Natural range <>) of Extended_Data_Type;

   package Address_Conversion is new
     System.Address_To_Access_Conversions (Extended_Data_Array_Type);

   subtype Extended_Data_Array_Access is Address_Conversion.Object_Pointer;

   type Ex_List_View_Control_Type is new List_View_Control_Type with
      record
         On_Header_Click_Event : Header_Click_Event      := null;
         On_Item_Changed_Event : Item_Changed_Event      := null;
         Color_Mode            : Boolean                 := True;
         Sort                  : Sort_Type;
         Column_Count          : Integer                 := 0;
      end record;

   type Pointer_To_Ex_List_View_Control_Class is
     access all Ex_List_View_Control_Type'Class;

end GWindows.Common_Controls.Ex_LV_Generic;
