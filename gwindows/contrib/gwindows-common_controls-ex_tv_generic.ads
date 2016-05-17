------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     Gwindows.Common_controls.Ex_Tv                       --
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
--    extension to the Tree_view_control_type (David Botton):
--    implemented features:
--
-- -> TreeHitTest, returns, which node is at point
-- -> select a node
-- -> nodes with icons
-- -> catch the change-event
--
-- attention: since this package uses the type tree_item_node
--            from gwindows.common_controls, the client-package has to
--            with it.
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
   -- create an Ex_Tree_View_control

   procedure Set_Image_List(Control : in Ex_Tree_View_Control_Type;
                            Image_List : in Ex_Image_List_Type);
   -- sets imagelist

   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_Item_Node          );
   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_View_List_Location_Type := Sort );
   -- insert an item

   procedure Set_Item_Id(Control: in Ex_Tree_View_Control_Type;
                         Node : in Tree_Item_Node;
                         Id: in Integer);
   function Get_Item_Id(Control: in Ex_Tree_View_Control_Type;
                        Node : in Tree_Item_Node) return Integer;
   -- set and get the item-id

   function Select_Item (Control : in Ex_Tree_View_Control_Type;
                         Item :  in Tree_Item_Node)
                        return Boolean;
   -- select a node, returns true if success

   procedure Set_Image(Control : in Ex_Tree_View_Control_Type;
                       Item : in Tree_Item_Node;
                       Image_List_Index : in Natural);
   -- sets a image for a node

   function Tree_Hit_Test(Control : in Ex_Tree_View_Control_Type;
                          pt : in GWindows.Types.Point_Type)
                         return Tree_Item_Node;
   -- returns the node under point

   procedure Set_Line_Color(Control: in Ex_Tree_View_Control_Type;
                            Line_Color : in Color_Type);
   -- sets the color of the node connecting lines

   procedure Set_Text_Color(Control: in Ex_Tree_View_Control_Type;
                            Color : in Color_Type);
   -- set text-color for control

   procedure Set_Bk_Color(Control: in Ex_Tree_View_Control_Type;
                          Color : in Color_Type);
   -- set background-color for control

   procedure Set_Item_Color(Control: in out Ex_Tree_View_Control_Type;
                            Text_Color : in Color_Type;
                            Bk_Color : in Color_Type;
                            Item : in Tree_Item_Node);
   -- set colors for an item

   procedure Set_Item_Data (Control   : in out Ex_Tree_View_Control_Type;
                            Data      : in     T;
                            Node      : in     Tree_Item_Node;
                            Redraw    : in     Boolean                   :=False);
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
                                Handler : in     Change_Event         );
   procedure Fire_On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                             Node : in Tree_Item_Node);
   procedure On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                        Node : in Tree_Item_Node);

   procedure On_Notify (Window       : in out Ex_Tree_View_Control_Type;
                        Message      : in     GWindows.Base.Pointer_To_Notification;
                        Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult                      );

   procedure On_Destroy (Window : in out Ex_Tree_View_Control_Type);

private

   type Extended_Data_Type is
      record
         ID          : Integer      := -1;
         Text_Color  : Color_Type   := To_Color(0, 0, 0);
         Back_Color  : Color_Type   := To_Color(255, 255, 255);
         More_Data   : T;
      end record;

   package Address_Conversion is new
     System.Address_To_Access_Conversions(Extended_Data_Type);

   subtype Extended_Data_Access is Address_Conversion.Object_Pointer;

   type Ex_Tree_View_Control_Type is new Tree_View_Control_Type with
      record
         On_Change_Event : Change_Event      := null;
      end record;

end GWindows.Common_Controls.Ex_TV_Generic;
