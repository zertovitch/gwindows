------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     Gwindows.Common_controls.Ex_Tv                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 1999 - 2019 David Botton / KonAd              --
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

with System;
with GWindows.Types;
use GWindows.Types;
with GWindows.Base;
use GWindows.Base;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GWindows.GStrings; use GWindows.GStrings;

package body GWindows.Common_Controls.Ex_TV_Generic is

   Tv_First           : constant := 16#1100#;
   Tvm_Hittest        : constant := Tv_First + 17;
   Tvm_Setimagelist   : constant := Tv_First + 9;
   TVM_SETLINECOLOR   : constant := Tv_First + 40;
   TVM_SETTEXTCOLOR   : constant := Tv_First + 30;
   TVM_SETBKCOLOR     : constant := Tv_First + 29;
   TVM_GETITEMA       : constant := Tv_First + 12;
   TVM_GETITEMW        : constant := Tv_First + 62;
   Tvsil_Normal       : constant := 0;
   TVM_INSERTITEMA    : constant := Tv_First + 0;
   TVM_INSERTITEMW    : constant := Tv_First + 50;
   Tvm_Setitema       : constant := Tv_First + 13;
   Tvm_Setitemw       : constant := Tv_First + 63;
   Tvif_Image         : constant := 16#0002#;
   Tvif_Selectedimage : constant := 16#0020#;
   TVIF_HANDLE        : constant := 16#0010#;
   TVIF_TEXT          : constant := 1;
   TVIF_PARAM         : constant := 4;
   TVN_FIRST          : constant := -400;
   TVN_SELCHANGEDA    : constant := TVN_FIRST - 2;
   TVN_SELCHANGEDW    : constant := TVN_FIRST - 51;
   Cdds_Prepaint                : constant := 16#0001#;
   Cdds_Item                    : constant := 16#00010000#;
   Cdds_Itemprepaint            : constant := Cdds_Item + Cdds_Prepaint;
   --  Cdds_Subitem                 : constant := 16#00020000#;
   Cdrf_Notifyitemdraw          : constant := 16#00000020#;
   Cdrf_Newfont                 : constant := 2;
   --  Cdrf_Dodefault               : constant := 0;
   --  Cdrf_Skipdefault             : constant := 4;

   type Tvitem is
      record
         Mask           : Interfaces.C.unsigned := 0;
         Hitem          : Tree_Item_Node        := 0;
         State          : Interfaces.C.unsigned := 0;
         State_Mask     : Interfaces.C.unsigned := 0;
         Text           : LPTSTR                := null;
         Textmax        : Integer               := 0;
         Image          : Integer               := 0;
         Selected_Image : Integer               := 0;
         Children       : Integer               := 0;
         Lparam         : System.Address;
      end record;

   type Nmcustomdraw_Type is
      record
         Hdr         : Notification;
         Dwdrawstage : Interfaces.C.long;
         Hdc         : GWindows.Types.Handle;
         Rect        : GWindows.Types.Rectangle_Type;
         Dwitemspec  : Interfaces.C.long;
         Uitemstate  : Interfaces.C.unsigned;
         Litemlparam : System.Address;
      end record;

   --  type Pointer_To_Nmcustomdraw_Type is access all Nmcustomdraw_Type;

   type NMTVCUSTOMDRAW_type is
     record
        Nmcd : Nmcustomdraw_Type;
        ClrText : Color_Type;
        ClrTextBk : Color_Type;
        ILevel : Interfaces.C.int;
     end record;

   type Pointer_To_NmTvcustomdraw_Type is access all NMTVCUSTOMDRAW_type;

   function Message_To_NmTvCustomdraw_Pointer is
      new Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                               Pointer_To_NmTvcustomdraw_Type);

   procedure Do_On_Redraw_Items (Tvcd_Ptr : in Pointer_To_NmTvcustomdraw_Type;
                                 control : in Ex_Tree_View_Control_Type;
                                 Return_Value : out GWindows.Types.Lresult);
   function Get_Lparam (Control : in Ex_Tree_View_Control_Type;
                        Item : in Tree_Item_Node) return System.Address;

   procedure Free is new Ada.Unchecked_Deallocation (Extended_Data_Type,
                                                     Extended_Data_Access);
   procedure Make_Free (Tree : in Ex_Tree_View_Control_Type;
                        Node : in Tree_Item_Node);

   --------------
   -- CreateEx --
   --------------

   procedure CreateEx (Control       : in out Ex_Tree_View_Control_Type;
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
                       Is_Dynamic    : in     Boolean                              := False) is
      use type Interfaces.C.unsigned;

      Tvs_Hasbuttons  : constant := 16#0001#;
      Tvs_Haslines    : constant := 16#0002#;
      Tvs_Linesatroot : constant := 16#0004#;
      --  TVS_EDITLABELS          : constant := 16#0008#;
      --  TVS_DISABLEDRAGDROP     : constant := 16#0010#;
        TVS_SHOWSELALWAYS       : constant := 16#0020#;
      --  TVS_RTLREADING          : constant := 16#0040#;
      --  TVS_NOTOOLTIPS          : constant := 16#0080#;
      --  TVS_CHECKBOXES          : constant := 16#0100#;
      --  TVS_TRACKSELECT         : constant := 16#0200#;
      Tvs_Singleexpand : constant := 16#0400#;
      --  TVS_INFOTIP             : constant := 16#0800#;
      --  TVS_FULLROWSELECT       : constant := 16#1000#;
      --  TVS_NOSCROLL            : constant := 16#2000#;
      --  TVS_NONEVENHEIGHT       : constant := 16#4000#;
      Styles : Interfaces.C.unsigned := TVS_SHOWSELALWAYS;
   begin
      if Lines then
         Styles := Styles or Tvs_Haslines;
      end if;

      if Single_Expand then
         Styles := Styles or Tvs_Singleexpand;
      end if;

      if Buttons then
         Styles := Styles or Tvs_Hasbuttons;
      end if;

      if Lines_At_Root then
         Styles := Styles or Tvs_Linesatroot;
      end if;

      Create_Control (Control, Parent,   -- fires, if node-selection changes
         "SysTreeView32",
         "",
         Left, Top, Width, Height,
         0, Styles,
         Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Common_Control_Type (Control));
      end if;
   end CreateEx;

   --------------------
   -- set_image_list --
   --------------------

   procedure Set_Image_List (Control    : in     Ex_Tree_View_Control_Type;
                             Image_List : in     Ex_Image_List_Type) is
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := Tvm_Setimagelist;
                             Wparam : Integer           := Tvsil_Normal;
                             Lparam : GWindows.Types.Handle := Handle (Image_List));
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Image_List;

   -----------------
   -- Insert_Item --
   -----------------

   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_Item_Node) is
      use Interfaces.C;
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);
      type Tvinsertstruct is
         record
            Hparent : Tree_Item_Node := Parent_Node;
            Hafter  : Tree_Item_Node := Where;
            Item    : Tvitem;
         end record;

      Ts : Tvinsertstruct;

      function Sendmessagea (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := TVM_INSERTITEMA;
                             Wparam : Integer           := 0;
                             Lparam : Tvinsertstruct    := Ts)
                            return Tree_Item_Node;
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      function Sendmessagew (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := TVM_INSERTITEMW;
                             Wparam : Integer           := 0;
                             Lparam : Tvinsertstruct    := Ts)
                            return Tree_Item_Node;
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Ts.Item.Mask := TVIF_TEXT or TVIF_PARAM;
      Ts.Item.Text := C_Text (0)'Unchecked_Access;
      Ts.Item.Lparam := Address_Conversion.To_Address (new Extended_Data_Type);

      case Character_Mode is
         when Unicode =>
           New_Node := Sendmessagew;
         when ANSI =>
           New_Node := Sendmessagea;
      end case;
   end Insert_Item;

   procedure Insert_Item (Control     : in out Ex_Tree_View_Control_Type;
                          Text        : in     GString;
                          Parent_Node : in     Tree_Item_Node;
                          New_Node    :    out Tree_Item_Node;
                          Where       : in     Tree_View_List_Location_Type := Sort) is

      Tvi_Root  : constant := 16#FFFF0000#;
      Tvi_First : constant := 16#FFFF0001#;
      Tvi_Last  : constant := 16#FFFF0002#;
      Tvi_Sort  : constant := 16#FFFF0003#;

      type Where_Value is array (Tree_View_List_Location_Type) of Tree_Item_Node;

      Values : constant Where_Value :=
        (First     => Tvi_First,
         Last      => Tvi_Last,
         Sort      => Tvi_Sort,
         As_A_Root => Tvi_Root);
   begin
      Insert_Item (Control, Text, Parent_Node, New_Node, Values (Where));
   end Insert_Item;

   -----------------
   -- set_item_id --
   -----------------

   procedure Set_Item_Id (Control : in Ex_Tree_View_Control_Type;
                         Node : in Tree_Item_Node;
                         Id : in Integer) is
      Data_access : Extended_Data_Access;
   begin
      Data_access := Address_Conversion.To_Pointer (Get_Lparam (Control, Node));
      Data_access.ID := Id;
   end Set_Item_Id;

   -----------------
   -- get_item_id --
   -----------------

   function Get_Item_Id (Control : in Ex_Tree_View_Control_Type;
                        Node : in Tree_Item_Node) return Integer is
      Data_access : Extended_Data_Access;
   begin
      Data_access := Address_Conversion.To_Pointer (Get_Lparam (Control, Node));
      return Data_access.ID;
   end Get_Item_Id;

   ---------------
   -- set_image --
   ---------------

   procedure Set_Image (Control          : in     Ex_Tree_View_Control_Type;
                        Item             : in     Tree_Item_Node;
                        Image_List_Index : in     Natural) is
      use Interfaces.C;
      procedure Sendmessagea (Hwnd   : GWindows.Types.Handle := Handle (Control);
                              Umsg   : Interfaces.C.int  := Tvm_Setitema;
                              Wparam : Integer           := 0;
                              Lparam : System.Address);
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessagew (Hwnd   : GWindows.Types.Handle := Handle (Control);
                              Umsg   : Interfaces.C.int  := Tvm_Setitemw;
                              Wparam : Integer           := 0;
                              Lparam : System.Address);
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);

      Tv : Tvitem;
   begin
      Tv.Hitem := Item;
      Tv.Mask := Tvif_Image + Tvif_Selectedimage;
      Tv.Image := Image_List_Index;
      Tv.Selected_Image := Image_List_Index;

      case Character_Mode is
         when Unicode =>
            Sendmessagew (Lparam => Tv'Address);
         when ANSI =>
            Sendmessagea (Lparam => Tv'Address);
      end case;

   end Set_Image;

   -------------------
   -- Tree_hit_test --
   -------------------

   function Tree_Hit_Test (Control : in     Ex_Tree_View_Control_Type;
                           pt      : in     Point_Type)
                          return Tree_Item_Node is

      type Tv_Hit_Test_Info_Type is
         record
            Point : Point_Type     := pt;
            Flags : Integer;
            Hitem : Tree_Item_Node;
         end record;

      Hit_Test_Structur : Tv_Hit_Test_Info_Type;

      procedure Sendmessage (
                             Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := Tvm_Hittest;
                             Wparam : Integer           := 0;
                             Lparam : System.Address    := Hit_Test_Structur'Address);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      Sendmessage;
      return Hit_Test_Structur.Hitem;
   end Tree_Hit_Test;

   --------------------
   -- set_line_color --
   --------------------

   procedure Set_Line_Color (Control : in Ex_Tree_View_Control_Type;
                             Line_Color : in Color_Type)
   is
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := TVM_SETLINECOLOR;
                             Wparam : Integer           := 0;
                             Lparam : Color_Type        := Line_Color);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Line_Color;

   --------------------
   -- set_Text_color --
   --------------------

   procedure Set_Text_Color(Control: in Ex_Tree_View_Control_Type;
                            Color : in Color_Type)is
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := TVM_SETTEXTCOLOR;
                             Wparam : Integer           := 0;
                             Lparam : Color_Type        := Color );
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Text_Color;

   ----------------------
   -- set_Bk_color --
   ----------------------

   procedure Set_Bk_Color(Control: in Ex_Tree_View_Control_Type;
                              Color : in Color_Type)is
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Control);
                             Umsg   : Interfaces.C.int  := TVM_SETBKCOLOR;
                             Wparam : Integer           := 0;
                             Lparam : Color_Type        := Color );
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Bk_Color;

   --------------------
   -- set_item_color --
   --------------------

   procedure Set_Item_Color(Control: in out Ex_Tree_View_Control_Type;
                            Text_Color : in Color_Type;
                            Bk_Color : in Color_Type;
                            Item : in Tree_Item_Node)is
      Data_access : Extended_Data_Access;
   begin
      Data_access := Address_Conversion.To_Pointer(Get_Lparam(Control, Item));
      Data_access.Text_Color := Text_Color;
      Data_access.Back_Color := Bk_Color;
   end Set_Item_Color;

   -------------------
   -- set_Item_data --
   -------------------

   procedure Set_Item_Data (Control   : in out Ex_Tree_View_Control_Type;
                            Data      : in     T;
                            Node      : in     Tree_Item_Node;
                            Redraw    : in     Boolean                   :=False)
   is
      Data_access : Extended_Data_Access;
   begin
      Data_access := Address_Conversion.To_Pointer(Get_Lparam(Control, Node));
      Data_access.More_Data := Data;
      if Redraw then
         GWindows.Common_Controls.Redraw(Tree_View_Control_Type(Control));
      end if;

   exception
      when others =>
         null;
   end Set_Item_Data;

   -------------------
   -- get_Item_data --
   -------------------

   function Get_Item_Data (Control   : in Ex_Tree_View_Control_Type;
                           Node: in Tree_Item_Node)
                          return T is
      Data_access : Extended_Data_Access;
   begin
      Data_access := Address_Conversion.To_Pointer(Get_Lparam(Control, Node));
      return Data_access.More_Data;
   end Get_Item_Data;

   --------------------
   -- event-handling --
   --------------------

   -----------------------
   -- On_change_Handler --
   -----------------------

   procedure On_change_Handler (Control : in out Ex_Tree_View_Control_Type'Class;
                                Handler : in     Change_Event         ) is
   begin
      Control.On_Change_Event := Handler;
   end On_change_Handler;

   --------------------
   -- Fire_On_change --
   --------------------

   procedure Fire_On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                             Node : in Tree_Item_Node) is
   begin
      if Control.On_Change_Event /= null then
         Control.On_Change_Event (Control, Node);
      end if;
   end Fire_On_Change;

   --------------
   -- on_change--
   --------------

   procedure On_Change (Control : in out Ex_Tree_View_Control_Type'Class;
                        Node : in Tree_Item_Node)is
   begin
      Fire_On_Change (Control, Node);
   end On_Change;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify (Window       : in out Ex_Tree_View_Control_Type;
                        Message      : in     GWindows.Base.Pointer_To_Notification;
                        Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult                      ) is
      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      Nm_ChangedA    : constant := TVN_SELCHANGEDA;
      Nm_Changedw    : constant := TVN_SELCHANGEDW;
      Nm_CustomDraw  : constant := -12;
   begin
      case Message.Code is
         when Nm_ChangedA =>
            declare
               Nmtv_Ptr : Pointer_To_NmTreeView_type :=
                 Message_To_NmtreeView_Pointer(Message);
            begin
               On_Change(Ex_Tree_View_Control_Type'Class (Window),
                         Nmtv_Ptr.ItemNew.Hitem);
            end;
         when Nm_Changedw =>
            declare
               Nmtv_Ptr : Pointer_To_NmTreeView_type :=
                 Message_To_NmtreeView_Pointer(Message);
            begin
               On_Change(Ex_Tree_View_Control_Type'Class (Window),
                         Nmtv_Ptr.ItemNew.Hitem);
            end;
         when Nm_CustomDraw =>
               declare
                  Tvcd_Ptr : Pointer_To_NmTvcustomdraw_Type :=
                    Message_To_NmTvCustomdraw_Pointer(Message);
               begin
                  Do_On_Redraw_Items(Tvcd_Ptr, Window, Return_Value);
               end;
         when others =>
            GWindows.Common_Controls.On_Notify(
               Tree_View_Control_Type(Window), Message, Control, Return_Value
            );
      end case;

   end On_Notify;

   ----------------
   -- on_destroy --
   ----------------

   procedure On_Destroy (Window : in out Ex_Tree_View_Control_Type)is
   begin
      Make_Free(Window, Get_Root_Item(Window));
   end On_Destroy;

   --------------------
   -- body functions --
   --------------------

   ---------------
   -- make_free --
   ---------------

   procedure Make_Free(Tree: in Ex_Tree_View_Control_Type;
                       Node: in Tree_Item_Node)is
      New_Node: Tree_Item_Node;
      Data_Access: Extended_Data_Access;
   begin
      New_Node := Get_First_Child_Item(Tree, Node);

      while New_Node /= 0 loop
         Make_Free(Tree, New_Node);
         New_Node := Get_Next_Item(Tree, New_Node);
      end loop;

      Data_Access := Address_Conversion.To_Pointer(Get_Lparam(Tree, Node));
      Free(Data_Access);
   end Make_Free;

   ----------------
   -- get_lparam --
   ----------------

   function Get_Lparam (Control : in Ex_Tree_View_Control_Type;
                        Item : in Tree_Item_Node) return System.Address
   is
      procedure Sendmessagea (Hwnd   : GWindows.Types.Handle := Handle (Control);
                              Umsg   : Interfaces.C.int  := TVM_GETITEMA;
                              Wparam : Integer           := 0;
                              Lparam : System.Address);
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessagew (Hwnd   : GWindows.Types.Handle := Handle (Control);
                              Umsg   : Interfaces.C.int  := TVM_GETITEMW;
                              Wparam : Integer           := 0;
                              Lparam : System.Address);
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);

      Tv : Tvitem;
   begin
      Tv.Hitem := Item;
      Tv.Mask := TVIF_HANDLE;

      case Character_Mode is
         when Unicode =>
            Sendmessagew (Lparam => Tv'Address);
         when ANSI =>
            Sendmessagea (Lparam => Tv'Address);
      end case;

      return Tv.Lparam;
   end Get_Lparam;

   ------------------------
   -- do_on_redraw_Items --
   ------------------------

procedure Do_On_Redraw_Items (Tvcd_Ptr : in Pointer_To_NmTvcustomdraw_Type;
                                control : in Ex_Tree_View_Control_Type;
                                Return_Value : out GWindows.Types.Lresult) is
   pragma Unreferenced (control);
   begin
      case Tvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
               Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.long (Cdds_Itemprepaint) =>
            declare
               Data_Access : Extended_Data_Access;
            begin
               Data_Access := Address_Conversion.To_Pointer (Tvcd_Ptr.Nmcd.Litemlparam);
               Tvcd_Ptr.ClrText := Data_Access.Text_Color;
               Tvcd_Ptr.ClrTextBk := Data_Access.Back_Color;
            end;
               Return_Value := Cdrf_Newfont;
         when others =>
            null;
      end case;
   end Do_On_Redraw_Items;

end GWindows.Common_Controls.Ex_TV_Generic;
