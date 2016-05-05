------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     Gwindows.Common_controls.Ex_Tb                       --
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
--  Toolbar with extended functions and styles
------------------------------------------------------------------------------

with GWindows.Image_Lists.Ex_Image_Lists;
use GWindows.Image_Lists.Ex_Image_Lists;

package GWindows.Common_Controls.Ex_Tb is

   type Ex_Toolbar_Control_Type is new Toolbar_Control_Type with private;
   type Ex_Toolbar_Control_Access is access all Ex_Toolbar_Control_Type;
   type Pointer_To_Ex_Toolbar_Control_Class is
     access all Ex_Toolbar_Control_Type'Class;

   type Button_Style_Type is (No_Style, Check_Style);
   --  Check_Style means that button remains in "pressed" state
   --  until pressed again.
   type Text_Position_Type is (Right, Bottom);
   --  Text_position: text at right or at bottom with respect to the
   --  buttons icon is_flat: "flat" Buttons


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
      Is_Dynamic    : in     Boolean                              := False);
   --  create ex_toolbar
   --  max_buttons: max number of buttons


   procedure Set_Image_List (Control : in out Ex_Toolbar_Control_Type;
                             List    : in     Ex_Image_List_Type);
   --  setting Image_list

   procedure Add_Button (Control      : in out Ex_Toolbar_Control_Type;
                         Image_Index  : in     Natural;
                         Command      : in     Integer;
                         Button_Style : in     Button_Style_Type;
                         Text         : in     GString               := " ";
                         Tooltip      : in     GString               := " ";
                         None_Image   : in     Boolean               := False);
   --  Inserts a button in Ex_Toolbar
   --
   --  Button_Style:
   --    no_style (standard toolbar-button) or check_style(toggle-button)
   --
   --  None_Image:
   --    if the button does not carry an image set none_image := true ->
   --    no space for an empty image is reserved.


   function Get_Tooltip (Control : in Ex_Toolbar_Control_Type;
                         command : in Integer)
                        return GString;
   --  Extract the tooltip text of the button with no. <command>

private
   type GString_Pointer is access all GString;

   type Tooltipdata_Type is
      record
         Command : Integer         := 0;
         Tooltip : GString_Pointer := null;
      end record;

   type Tooltipdata_Array_Type is
     array (Positive range <>) of Tooltipdata_Type;
   type Tooltipdata_Array_Pointer is access all Tooltipdata_Array_Type;

   type Ex_Toolbar_Control_Type is new Toolbar_Control_Type with
      record
         ToolTip_Array : Tooltipdata_Array_Pointer := null;
         Next_Tooltip  : Integer                   := 1;
      end record;

end GWindows.Common_Controls.Ex_Tb;
