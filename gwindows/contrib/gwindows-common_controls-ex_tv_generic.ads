------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     Gwindows.Common_controls.Ex_Tv                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 1999 - 2023 David Botton / KonAd              --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------
--    extension to the Tree_view_control_type (David Botton):
--    implemented features:
--
--  -> TreeHitTest, returns which node is at point
--  -> nodes with icons
--  -> catch the change-event
--
--  attention: since this package uses the type tree_item_node
--             from gwindows.common_controls, the client-package has to
--             with it.
------------------------------------------------------------------------------

with GWindows.Types;
with GWindows.Image_Lists.Ex_Image_Lists; use GWindows.Image_Lists.Ex_Image_Lists;
with GWindows.Colors; use GWindows.Colors;
with System.Address_To_Access_Conversions;

generic
   type T is private;
package GWindows.Common_Controls.Ex_TV_Generic is

   type Ex_Tree_View_Control_Type is new Tree_View_Control_Type with private;
   type Ex_tree_View_control_Access is access all Ex_Tree_View_Control_Type;
   type Pointer_To_Ex_tree_View_control_Class is access all Ex_Tree_View_Control_Type'Class;

   procedure CreateEx
     (Control       : in out Ex_Tree_View_Control_Type;
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
   --  create an Ex_Tree_View_control

   procedure Set_Image_List (Control    : in Ex_Tree_View_Control_Type;
                             Image_List : in Ex_Image_List_Type);
   --  sets imagelist

   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_Item_Node);
   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_View_List_Location_Type := Sort);
   --  insert an item

   procedure Set_Item_Id (Control : in Ex_Tree_View_Control_Type;
                          Node    : in Tree_Item_Node;
                          Id      : in Integer);
   function Get_Item_Id (Control : in Ex_Tree_View_Control_Type;
                         Node    : in Tree_Item_Node) return Integer;
   -- set and get the item-id

   --  Function Select_Item has been moved to parent type
   --  Tree_View_Control_Type in GWindows.Common_Controls, along with
   --  a procedure having the same name. Change: GdM 31-Jul-2019.

   procedure Set_Image (Control          : in Ex_Tree_View_Control_Type;
                        Item             : in Tree_Item_Node;
                        Image_List_Index : in Natural);
   -- sets a image for a node

   function Tree_Hit_Test (Control : in Ex_Tree_View_Control_Type;
                           pt      : in GWindows.Types.Point_Type)
                           return Tree_Item_Node;
   -- returns the node under point

   procedure Set_Line_Color (Control    : in Ex_Tree_View_Control_Type;
                             Line_Color : in Color_Type);
   -- sets the color of the node connecting lines

   procedure Set_Text_Color (Control : in Ex_Tree_View_Control_Type;
                             Color   : in Color_Type);
   -- set text-color for control

   procedure Set_Bk_Color (Control : in Ex_Tree_View_Control_Type;
                           Color   : in Color_Type);
   -- set background-color for control

   procedure Set_Item_Color (Control    : in out Ex_Tree_View_Control_Type;
                             Text_Color : in     Color_Type;
                             Bk_Color   : in     Color_Type;
                             Item       : in     Tree_Item_Node);
   -- set colors for an item

   procedure Set_Item_Data (Control   : in out Ex_Tree_View_Control_Type;
                            Data      : in     T;
                            Node      : in     Tree_Item_Node;
                            Redraw    : in     Boolean                   := False);
   -- sets the payload data individually for an Node

   function Get_Item_Data (Control   : in Ex_Tree_View_Control_Type;
                           Node      : in Tree_Item_Node)
                          return T;
   -- returns payload data acossiated with item

   -- fires, if node-selection changes
   type Change_Event is access procedure
     (Window    : in out Ex_Tree_View_Control_Type'Class;
      Node : in Tree_Item_Node);

   procedure On_change_Handler (Control : in out Ex_Tree_View_Control_Type'Class;
                                Handler : in     Change_Event);
   procedure Fire_On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                             Node : in Tree_Item_Node);
   procedure On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                        Node : in Tree_Item_Node);

   procedure On_Notify (Window       : in out Ex_Tree_View_Control_Type;
                        Message      : in     GWindows.Base.Pointer_To_Notification;
                        Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult);

   procedure On_Destroy (Window : in out Ex_Tree_View_Control_Type);

private

   type Extended_Data_Type is
      record
         ID          : Integer      := -1;
         Text_Color  : Color_Type   := To_Color (0, 0, 0);
         Back_Color  : Color_Type   := To_Color (255, 255, 255);
         More_Data   : T;
      end record;

   package Address_Conversion is new
     System.Address_To_Access_Conversions (Extended_Data_Type);

   subtype Extended_Data_Access is Address_Conversion.Object_Pointer;

   type Ex_Tree_View_Control_Type is new Tree_View_Control_Type with
      record
         On_Change_Event : Change_Event      := null;
      end record;

end GWindows.Common_Controls.Ex_TV_Generic;
