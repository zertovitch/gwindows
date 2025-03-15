------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     GWindows.Common_controls.Ex_Tb                       --
--                                                                          --
--                                 S p e c                                  --
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
--  Toolbar with extended functions and styles
------------------------------------------------------------------------------

with GWindows.Image_Lists.Ex_Image_Lists;

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

   procedure Set_Image_List
     (Control : in out Ex_Toolbar_Control_Type;
      List    : in     GWindows.Image_Lists.Ex_Image_Lists.Ex_Image_List_Type);
   --  Setting Image_List.
   --  NB: it is also possible to use Set_Image_List with List: Image_List_Type
   --      from the parent type.

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
