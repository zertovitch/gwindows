------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     gwindows.common_controls.ex_lv                       --
--                                                                          --
--                                 B o d y                                  --
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

with System;
with GWindows.GStrings;
with GWindows.Types; use GWindows.Types;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body GWindows.Common_Controls.Ex_LV is

   Lvm_First                    : constant := 16#1000#;
   Lvm_Setimagelist             : constant := Lvm_First + 3;
   Lvm_Setextendedlistviewstyle : constant := Lvm_First + 54;
   Lvm_Sortitems                : constant := Lvm_First + 48;
   Lvm_Finditema                : constant := Lvm_First + 13;
   Lvm_Finditemw                : constant := Lvm_First + 83;
   Lvm_Setitema                 : constant := Lvm_First + 6;
   Lvm_Setitemw                 : constant := Lvm_First + 76;
   Lvm_Settextcolor             : constant := Lvm_First + 36;
   LVM_SETTEXTBKCOLOR           : constant := Lvm_First + 38;
   Lvm_Redrawitems              : constant := Lvm_First + 21;
   Lvm_Getitema                 : constant := Lvm_First + 5;
   Lvm_Getitemw                 : constant := Lvm_First + 75;
   Lvm_Insertitema              : constant := Lvm_First + 7;
   Lvm_Insertitemw              : constant := Lvm_First + 77;
   LVM_SETCOLUMNWIDTH           : constant := Lvm_First + 30;
   LVM_GETHEADER                : constant := Lvm_First + 31;
   LVM_SETCOLUMNORDERARRAY      : constant := Lvm_First + 58;
   LVM_GETCOLUMNORDERARRAY      : constant := Lvm_First + 59;
   Lvs_Ex_Gridlines             : constant := 1;
   Lvs_Ex_Flatsb                : constant := 256;
   Lvs_Ex_Headerdragdrop        : constant := 16;
   Lvs_Ex_Fullrowselect         : constant := 32;
   Lvsil_Small                  : constant := 1;
   Lvfi_Param                   : constant := 1;
   Lvif_Param                   : constant := 16#0004#;
   Lvif_Text                    : constant := 16#0001#;
   Lvif_Image                   : constant := 16#0002#;
--   Ccm_First                    : constant := 16#2000#;
--   Ccm_Setversion               : constant := Ccm_First + 7;
   Cdds_Prepaint                : constant := 16#0001#;
   Cdds_Item                    : constant := 16#00010000#;
   Cdds_Itemprepaint            : constant := Cdds_Item + Cdds_Prepaint;
   Cdds_Subitem                 : constant := 16#00020000#;
   Cdrf_Notifyitemdraw          : constant := 16#00000020#;
   Cdrf_Notifysubitemdraw       : constant := 16#00000020#;
   Cdrf_Newfont                 : constant := 2;
--   Cdrf_Dodefault               : constant := 0;
--   Cdrf_Skipdefault             : constant := 4;
   HDM_FIRST                    : constant := 16#1200#;
   HDM_GETITEMA                 : constant := HDM_FIRST + 3;
   HDM_GETITEMW                 : constant := HDM_FIRST + 11;
   HDM_SETITEMA                 : constant := HDM_FIRST + 4;
   HDM_SETITEMW                 : constant := HDM_FIRST + 12;
   HDI_FORMAT                   : constant := 4;
   HDI_IMAGE                    : constant := 16#20#;
   --   HDF_SORTUP                   : constant := 16#400#;
   --              erst ab Version 6 (XP) verwendbar
   --   HDF_SORTDOWN                 : constant := 16#200#;
   --              erst ab Version 6 (XP) verwendbar
   HDF_STRING                   : constant := 16#4000#;
   HDF_BITMAP_ON_RIGHT          : constant := 16#1000#;
   HDF_IMAGE                    : constant := 16#800#;

   type Lptstr is access all GChar_C;

   type Nmlistview_Type is
      record
         Hdr       : GWindows.Base.Notification;
         Iitem     : Interfaces.C.int;
         Isubitem  : Interfaces.C.int;
         Unewstate : Interfaces.C.int;
         Uoldstate : Interfaces.C.int;
         Uchanged  : Interfaces.C.int;
         Point     : GWindows.Types.Point_Type;
         Lparam    : System.Address;
      end record;

   type Pointer_To_Nmlistview_Type is access all Nmlistview_Type;

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

--   type Pointer_To_Nmcustomdraw_Type is access all Nmcustomdraw_Type;

   type Nmlvcustomdraw_Type is
      record
         Nmcd      : Nmcustomdraw_Type;
         Clrtext   : Color_Type;
         Clrtextbk : Color_Type;
         Isubitem  : Interfaces.C.int;
      end record;

   type Pointer_To_Nmlvcustomdraw_Type is access all Nmlvcustomdraw_Type;

   type Findinfo_Type is
      record
         Flags       : Interfaces.C.unsigned := Lvfi_Param;
         Psz         : Lptstr                := null;
         Lparam      : System.Address;
         Point       : Point_Type;
         Vkdirection : Interfaces.C.unsigned := 0;
      end record;

   type Sort_Data_Type is
      record
         Control_Handle : GWindows.Types.Handle;
         Column         : Integer;
         Factor : Integer;
      end record;

   type Lvitem is
      record
         Mask      : Interfaces.C.unsigned := 0;
         Item      : Integer               := 0;
         Subitem   : Integer               := 0;
         State     : Interfaces.C.unsigned := 0;
         Statemask : Interfaces.C.unsigned := 0;
         Text      : Lptstr                := null;
         Textmax   : Integer               := 0;
         Image     : Integer;
         Lparam    : System.Address;
         Indent    : Integer;
      end record;

   type Hditem is
      record
         Mask       : Interfaces.C.unsigned := 0;
         Cxy        : Interfaces.C.int      := 0;
         pszText    : Lptstr                := null;
         HBitmap    : Interfaces.C.long     := 0;
         CchTextMax : Interfaces.C.int      := 0;
         Fmt        : Interfaces.C.int      := 0;
         Lparam     : Interfaces.C.int      := 0;
         IImage     : Interfaces.C.int      := 0;
         IOrder     : Interfaces.C.int      := 0;
         Typ        : Interfaces.C.unsigned := 0;
         PvFilter   : System.Address;
      end record;

   type Hditem_Pointer is access all Hditem;

   -----------------
   -- body spec's --
   -----------------

   function Message_To_Nmlvcustomdraw_Pointer is
      new Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                                    Pointer_To_Nmlvcustomdraw_Type);

   function Message_To_Nmlistview_Pointer is
      new Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                                    Pointer_To_Nmlistview_Type);

   function Get_Lparam (Control   : in Ex_List_View_Control_Type;
                        Index     : in Natural;
                        Sub_Index : in Natural)
                       return System.Address;

   procedure Do_On_Redraw_Items
     (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
      Control      : in out Ex_List_View_Control_Type;
      Return_Value : in out Interfaces.C.long);

   function Compare (Lparam1    : in System.Address;
                     Lparam2    : in System.Address;
                     Lparamsort : in Sort_Data_Type)
                    return Interfaces.C.int;
   pragma Export (Stdcall, Compare, "compare");

   procedure Free is new Ada.Unchecked_Deallocation
     (Extended_Data_Array_Type,
      Extended_Data_Array_Access);

   --------------
   -- createEx --
   --------------

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
      Is_Dynamic : in     Boolean                              := False)
   is
      use type Interfaces.C.unsigned;

      Lvs_Icon      : constant := 16#0000#;
      Lvs_Report    : constant := 16#0001#;
      Lvs_Smallicon : constant := 16#0002#;
      Lvs_List      : constant := 16#0003#;
      --  LVS_TYPEMASK            : constant := 16#0003#;
      Lvs_Singlesel : constant := 16#0004#;
      LVS_SHOWSELALWAYS       : constant := 16#0008#;
      Lvs_Sortascending  : constant := 16#0010#;
      Lvs_Sortdescending : constant := 16#0020#;
      --  LVS_SHAREIMAGELISTS     : constant := 16#0040#;
      --  LVS_NOLABELWRAP         : constant := 16#0080#;
      Lvs_Autoarrange : constant := 16#0100#;
      --  LVS_EDITLABELS          : constant := 16#0200#;
      --  LVS_NOSCROLL            : constant := 16#2000#;
      --  LVS_TYPESTYLEMASK       : constant := 16#Fc00#;
      Lvs_Aligntop  : constant := 16#0000#;
      Lvs_Alignleft : constant := 16#0800#;
      --  LVS_ALIGNMASK           : constant := 16#0c00#;
      --  LVS_NOCOLUMNHEADER      : constant := 16#4000#;
      Lvs_Nosortheader : constant := 16#8000#;
      --  LVS_Ownerdrawfixed           : constant := 16#0400#;

      Styles : Interfaces.C.unsigned := LVS_SHOWSELALWAYS;
   begin
      if Selection = Single then
         Styles := Styles or Lvs_Singlesel;
      end if;

      case View is
         when Icon_View =>
            Styles := Styles or Lvs_Icon;
         when Small_Icon_View =>
            Styles := Styles or Lvs_Smallicon;
         when List_View =>
            Styles := Styles or Lvs_List;
         when Report_View =>
            Styles := Styles or Lvs_Report;
      end case;

      if Sort /= Sort_Custom then
         Styles := Styles or Lvs_Nosortheader;
      else
         if Sort = Sort_Ascending then
            Styles := Styles or Lvs_Sortascending;
         elsif Sort = Sort_Descending then
            Styles := Styles or Lvs_Sortdescending;
         end if;
      end if;

      if Arrange then
         Styles := Styles or Lvs_Autoarrange;
      end if;

      if Align = Align_Left then
         Styles := Styles or Lvs_Alignleft;
      elsif Align = Align_Top then
         Styles := Styles or Lvs_Aligntop;
      end if;

      Control.Color_Mode := Color_Mode;

      Create_Control (Control, Parent,
         "SysListView32",
         "",
         Left, Top, Width, Height,
         10, Styles,
         Is_Dynamic => Is_Dynamic);

      if Show then
         GWindows.Common_Controls.Show (Common_Control_Type (Control));
      end if;

   end CreateEx;

   --------------------------
   -- set_extended_style --
   --------------------------

   procedure Set_Extended_Style
     (Control : in Ex_List_View_Control_Type;
      Style   : in List_View_Extended_Style_Type)
   is
      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setextendedlistviewstyle;
         Wparam : Integer;
         Lparam : Integer);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case Style is
         when Grid =>
            Sendmessage (Wparam => Lvs_Ex_Gridlines,
                         Lparam => Lvs_Ex_Gridlines);
         when Flatsb =>
            Sendmessage (Wparam => Lvs_Ex_Flatsb,
                         Lparam => Lvs_Ex_Flatsb);
         when Headerdragdrop =>
            Sendmessage (Wparam => Lvs_Ex_Headerdragdrop,
                         Lparam => Lvs_Ex_Headerdragdrop);
         when Fullrowselect =>
            Sendmessage (Wparam => Lvs_Ex_Fullrowselect,
                         Lparam => Lvs_Ex_Fullrowselect);
      end case;
   end Set_Extended_Style;

   --------------------------
   -- remove_extended_style --
   --------------------------

   procedure Remove_Extended_Style
     (Control : in Ex_List_View_Control_Type;
      Style   : in List_View_Extended_Style_Type)
   is
      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setextendedlistviewstyle;
         Wparam : Integer;
         Lparam : Integer           := 0);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      case Style is
         when Grid =>
            Sendmessage (Wparam => Lvs_Ex_Gridlines);
         when Flatsb =>
            Sendmessage (Wparam => Lvs_Ex_Flatsb);
         when Headerdragdrop =>
            Sendmessage (Wparam => Lvs_Ex_Headerdragdrop);
         when Fullrowselect =>
            Sendmessage (Wparam => Lvs_Ex_Fullrowselect);
      end case;
   end Remove_Extended_Style;

   --------------------------
   -- clear_extended_style --
   --------------------------

   procedure Clear_Extended_Styles (Control : in Ex_List_View_Control_Type)
   is
      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setextendedlistviewstyle;
         Wparam : Integer           := 0;
         Lparam : Integer           := 0);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Clear_Extended_Styles;

   -----------------
   -- insert_item --
   -----------------

   procedure Insert_Item (Control : in out Ex_List_View_Control_Type;
                          Text    : in     GString;
                          Index   : in     Integer;
                          Icon    : in     Integer                   := 0)
   is
      use Interfaces.C;

      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);
      Item : Lvitem;

      procedure Sendmessagea
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Insertitema;
         Wparam : Integer           := 0;
         Lparam : Lvitem            := Item);
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessagew
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Insertitemw;
         Wparam : Integer           := 0;
         Lparam : Lvitem            := Item);
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);

      Array_Access : Extended_Data_Array_Access;
   begin
      Item.Mask  := Lvif_Text or Lvif_Image or Lvif_Param;
      Item.Item  := Index;
      Item.Image := Icon;
      Item.Text  := C_Text (0)'Unchecked_Access;

      Array_Access :=
        new Extended_Data_Array_Type (0 .. Control.Column_Count - 1);
      Item.Lparam  := Address_Conversion.To_Address (Array_Access);

      case Character_Mode is
         when Unicode =>
            Sendmessagew;
         when ANSI =>
            Sendmessagea;
      end case;

   exception
      when others =>
         null;
   end Insert_Item;

   -----------------
   -- delete_Item --
   -----------------

   procedure Delete_Item (Control : in out Ex_List_View_Control_Type;
                          Index   : in     Integer)
   is
      Lparam : System.Address;
      Array_Access : Extended_Data_Array_Access;
   begin
      Lparam := Get_Lparam (Control, Index, 0);
      Array_Access := Address_Conversion.To_Pointer (Lparam);
      Delete_Item (List_View_Control_Type (Control), Index);
      Free (Array_Access);
   end Delete_Item;

   ------------------
   -- Set_Sub_Item --
   ------------------

   procedure Set_Sub_Item
     (Control   : in out Ex_List_View_Control_Type;
      Text      : in     GString;
      Index     : in     Integer;
      Sub_Index : in     Integer)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

      Item : Lvitem;

      procedure Sendmessagea
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setitema;
         Wparam : Integer           := 0;
         Lparam : Lvitem            := Item);
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessagew
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setitemw;
         Wparam : Integer           := 0;
         Lparam : Lvitem            := Item);
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Item.Mask := Lvif_Text;
      Item.Item := Index;
      Item.Subitem := Sub_Index;
      Item.Text := C_Text (0)'Unchecked_Access;

      case Character_Mode is
         when Unicode =>
            Sendmessagew;
         when ANSI =>
            Sendmessagea;
      end case;

   exception
      when others =>
         null;
   end Set_Sub_Item;

   --------------------
   -- set_image_list---
   --------------------

   procedure Set_Image_List (Control    : in Ex_List_View_Control_Type;
                             Image_List : in Ex_Image_List_Type)
   is


      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Setimagelist;
         Wparam : Integer           := Lvsil_Small;
         Lparam : Interfaces.C.long := Handle (Image_List));
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Image_List;

   -----------------
   -- Sort_Column --
   -----------------

   procedure Sort_Column
     (Control        : in out Ex_List_View_Control_Type;
      Column         : in     Integer;
      Sort           : in     Sort_Direction_Type       := auto;
      Up_Sort_Icon   : in     Integer                   := 0;
      Down_Sort_Icon : in     Integer                   := 1)
   is
      use Interfaces.C;

      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Sortitems;
         Wparam : Sort_Data_Type;
         Lparam : System.Address);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);

      function Sendmessage_Getheader
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := LVM_GETHEADER;
         Wparam : Integer           := 0;
         Lparam : Integer           := 0)
        return Interfaces.C.long;
      pragma Import (Stdcall, Sendmessage_Getheader,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessage_HdmsetitemA
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := HDM_SETITEMA;
         Wparam : Integer           := 0;
         Lparam : Hditem_Pointer    := null);
      pragma Import (Stdcall, Sendmessage_HdmsetitemA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessage_HdmsetitemW
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := HDM_SETITEMW;
         Wparam : Integer           := 0;
         Lparam : Hditem_Pointer    := null);
      pragma Import (Stdcall, Sendmessage_HdmsetitemW,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessage_HdmgetitemA
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := HDM_GETITEMA;
         Wparam : Integer           := 0;
         Lparam : Hditem_Pointer    := null);
      pragma Import (Stdcall, Sendmessage_HdmgetitemA,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessage_HdmgetitemW
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := HDM_GETITEMW;
         Wparam : Integer           := 0;
         Lparam : Hditem_Pointer    := null);
      pragma Import (Stdcall, Sendmessage_HdmgetitemW,
                       "SendMessage" & Character_Mode_Identifier);

      Sort_Data : Sort_Data_Type := (Control_Handle => Handle (Control),
                                     Column         => Column,
                                     Factor         => 0);

      Header_Handle    : GWindows.Types.Handle;
      Header_Item_ptr  : Hditem_Pointer := new Hditem;
   begin
      -----------------------------------------------------------------------
      --  Header holen
      Header_Handle := Sendmessage_Getheader;
      --  aktuelle Itemdaten holen
      case Character_Mode is
         when Unicode =>
            Sendmessage_HdmgetitemW (Hwnd   => Header_Handle,
                                     Wparam => Control.Sort.Column,
                                     Lparam => Header_Item_ptr);
         when ANSI =>
            Sendmessage_HdmgetitemA (Hwnd => Header_Handle,
                                     Wparam => Control.Sort.Column,
                                     Lparam => Header_Item_ptr);
      end case;

      --  neu setzen, um Image aus header zu entfernen
      Header_Item_ptr.Mask := HDI_FORMAT;
      Header_Item_ptr.Fmt := HDF_STRING;

      case Character_Mode is
         when Unicode =>
            Sendmessage_HdmsetitemW (Hwnd   => Header_Handle,
                                     Wparam => Control.Sort.Column,
                                     Lparam => Header_Item_ptr);
         when ANSI =>
            Sendmessage_HdmsetitemA (Hwnd   => Header_Handle,
                                     Wparam => Control.Sort.Column,
                                     Lparam => Header_Item_ptr);
      end case;

      -----------------------------------------------------------------------

      case Sort is
         when auto =>
            --  choose sorting automatically
            if
              Column = Control.Sort.Column
              and
              Control.Sort.Sort_Direction = Up
            then
               Control.Sort.Sort_Direction := Down;
               Sort_Data.Factor := -1;
            else
               Control.Sort.Sort_Direction := Up;
               Control.Sort.Column := Column;
               Sort_Data.Factor := 1;
            end if;
         when Up =>
            Control.Sort.Sort_Direction := Up;
            Control.Sort.Column := Column;
            Sort_Data.Factor := 1;
         when Down =>
            Control.Sort.Sort_Direction := Down;
            Control.Sort.Column := Column;
            Sort_Data.Factor := -1;
      end case;

      --  sort with callback-function

      Sendmessage (Wparam => Sort_Data,
                   Lparam => Compare'Address);

      -----------------------------------------------------------------------
      --  neues sort_icon setzen
      Header_Item_ptr.Mask := HDI_FORMAT + HDI_IMAGE;
      Header_Item_ptr.Fmt := HDF_STRING + HDF_IMAGE + HDF_BITMAP_ON_RIGHT;

      if Control.Sort.Sort_Direction = Up then
         Header_Item_ptr.IImage := Interfaces.C.int (Up_Sort_Icon);
      else
         Header_Item_ptr.IImage := Interfaces.C.int (Down_Sort_Icon);
      end if;


      case Character_Mode is
         when Unicode =>
            Sendmessage_HdmsetitemW (Hwnd   => Header_Handle,
                                     Wparam => Column,
                                     Lparam => Header_Item_ptr);
         when ANSI =>
            Sendmessage_HdmsetitemA (Hwnd   => Header_Handle,
                                     Wparam => Column,
                                     Lparam => Header_Item_ptr);
      end case;


      -----------------------------------------------------------------------
   end Sort_Column;

   --------------------
   -- set_text_color --
   --------------------

   procedure Set_Text_Color (Control : in Ex_List_View_Control_Type;
                             Color   : in Color_Type)
   is
      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Settextcolor;
         Wparam : Interfaces.C.int  := 0;
         Lparam : Color_Type        := Color);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Text_Color;

   ----------------------
   -- set_TextBk_color --
   ----------------------

   procedure Set_TextBk_Color (Control : in out Ex_List_View_Control_Type;
                               Color   : in     Color_Type)
   is
      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := LVM_SETTEXTBKCOLOR;
         Wparam : Interfaces.C.int  := 0;
         Lparam : Color_Type        := Color);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
      Redraw (Control);
   end Set_TextBk_Color;

   --------------------
   -- set_Item_color --
   --------------------

   procedure Set_Item_Color (Control    : in out Ex_List_View_Control_Type;
                             Text_Color : in     Color_Type;
                             Bk_Color   : in     Color_Type;
                             Index      : in     Integer;
                             Sub_Index  : in     Integer)
   is

      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Redrawitems;
         Wparam : Interfaces.C.int  := Interfaces.C.int (Index);
         Lparam : Interfaces.C.int  := Interfaces.C.int (Index));
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);

      Array_access : Extended_Data_Array_Access;
   begin
      Array_access :=
        Address_Conversion.To_Pointer (Get_Lparam (Control, Index, 0));
      Array_access (Sub_Index).Text_Color := Text_Color;
      Array_access (Sub_Index).Back_Color := Bk_Color;

      --  redraw items
      Sendmessage;

   exception
      when others =>
         null;
   end Set_Item_Color;

   ---------------------
   -- column_autosize --
   ---------------------

   procedure Column_Autosize (Control : in Ex_List_View_Control_Type;
                              Column  : in Integer)
   is
      use Interfaces.C;

      procedure Sendmessage
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := LVM_SETCOLUMNWIDTH;
         Wparam : Interfaces.C.int  := Interfaces.C.int (Column);
         Lparam : Interfaces.C.int  := -2);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Column_Autosize;

   ----------------------
   -- Item_At_Position --
   ----------------------

   procedure Item_At_Position (Control  : in     Ex_List_View_Control_Type;
                               Position : in     GWindows.Types.Point_Type;
                               Item     : in out Integer;
                               Subitem  : in out Integer)
   is
      use GWindows.Types;

      type Lvhittestinfo is
         record
            Pt      : GWindows.Types.Point_Type := Position;
            Flags   : Natural                   := 0;
            Item    : Integer                   := 0;
            Subitem : Integer                   := 0;
         end record;

      Lvm_Subitemhittest : constant := Lvm_First + 57;

      procedure Sendmessage
        (Hwnd   :        Interfaces.C.long :=
           Handle (List_View_Control_Type (Control));
         Umsg   :        Interfaces.C.int  := Lvm_Subitemhittest;
         Wparam :        Interfaces.C.long := 0;
         Lparam : in out Lvhittestinfo);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);

      Hittestinfo : Lvhittestinfo;
   begin
      Sendmessage (Lparam => Hittestinfo);

      Item := Hittestinfo.Item;
      Subitem := Hittestinfo.Subitem;
   end Item_At_Position;

   ---------------------
   -- set_columnorder --
   ---------------------

   procedure Set_Columnorder (Control     : in Ex_List_View_Control_Type;
                              Columnorder : in Int_Array_Type)
   is

      procedure Sendmessage
        (Hwnd   : Interfaces.C.long :=
           Handle (List_View_Control_Type (Control));
         Umsg   : Interfaces.C.int  := LVM_SETCOLUMNORDERARRAY;
         Wparam : Interfaces.C.int  := Interfaces.C.int (Columnorder'Last);
         Lparam : System.Address    := Columnorder'Address);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Columnorder;

   ---------------------
   -- Get_Columnorder --
   ---------------------

   procedure Get_Columnorder (Control     : in     Ex_List_View_Control_Type;
                              Columnorder : in out Int_Array_Type)
   is

      procedure Sendmessage
        (Hwnd   : Interfaces.C.long :=
           Handle (List_View_Control_Type (Control));
         Umsg   : Interfaces.C.int  := LVM_GETCOLUMNORDERARRAY;
         Wparam : Interfaces.C.int  := Interfaces.C.int (Columnorder'Last);
         Lparam : System.Address    := Columnorder'Address);
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Get_Columnorder;

   -------------------
   -- insert_column --
   -------------------

   procedure Insert_Column (Control : in out Ex_List_View_Control_Type;
                            Text    : in     GString;
                            Index   : in     Integer;
                            Width   : in     Integer)
   is
   begin
      Control.Column_Count := Control.Column_Count + 1;
      Insert_Column (List_View_Control_Type (Control), Text, Index, Width);
   end Insert_Column;

   ----------------------
   -- get_column_count --
   ----------------------

   function Get_Column_Count (Control : in Ex_List_View_Control_Type)
                             return Integer
   is
   begin
      return Control.Column_Count;
   end Get_Column_Count;


   --------------------
   -- event-handling --
   --------------------

   --------------------
   -- on_Header_Click--
   --------------------

   procedure On_Header_Click
     (Control : in out Ex_List_View_Control_Type;
      Column  : in     Integer)
   is
   begin
      Fire_On_Header_Click (Control, Column);
   end On_Header_Click;

   -----------------------------
   -- On_Header_Click_Handler --
   -----------------------------

   procedure On_Header_Click_Handler
     (Control : in out Ex_List_View_Control_Type;
      Handler : in     Header_Click_Event)
   is
   begin
      Control.On_Header_Click_Event := Handler;
   end On_Header_Click_Handler;

   --------------------------
   -- Fire_On_Header_Click --
   --------------------------

   procedure Fire_On_Header_Click (Control : in out Ex_List_View_Control_Type;
                                   Column  : in     Integer)
   is
   begin
      if Control.On_Header_Click_Event /= null then
         Control.On_Header_Click_Event (Control, Column);
      end if;
   end Fire_On_Header_Click;

   --------------------
   -- on_Item_Changed--
   --------------------
   procedure On_Item_Changed (Control : in out Ex_List_View_Control_Type;
                              index   : in     Integer)
   is
   begin
      Fire_On_Item_Changed (Control, index);
   end On_Item_Changed;

   -----------------------------
   -- on_Item_Changed_Handler--
   -----------------------------

   procedure On_Item_Changed_Handler
     (Control : in out Ex_List_View_Control_Type;
      Handler : in     Item_Changed_Event)
   is
   begin
      Control.On_Item_Changed_Event := Handler;
   end On_Item_Changed_Handler;

   --------------------------
   -- Fire_On_Item_Changed --
   --------------------------

   procedure Fire_On_Item_Changed (Control : in out Ex_List_View_Control_Type;
                                   index   : in     Integer)
   is
   begin
      if Control.On_Item_Changed_Event /= null then
         Control.On_Item_Changed_Event (Control, index);
      end if;
   end Fire_On_Item_Changed;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Control : in out Ex_List_View_Control_Type) is
   begin
      Border (Control);
      Tab_Stop (Control);
   end On_Create;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Ex_List_View_Control_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out Interfaces.C.long)
   is

      use Interfaces.C;

      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      Nm_Outofmemory  : constant := -1;
      Nm_Click        : constant := -2;
      Nm_Dblclk       : constant := -3;
      Nm_Return       : constant := -4;
      Nm_Rclick       : constant := -5;
      Nm_Rdblclk      : constant := -6;
      Nm_Setfocus     : constant := -7;
      Nm_Killfocus    : constant := -8;
      Nm_Hover        : constant := -13;
      Nm_Header_Click : constant := -108;
      Nm_Customdraw   : constant := -12;
      Nm_Item_Changed : constant := -101;
   begin
      case Message.Code is
         when Nm_Outofmemory =>
            On_Out_Of_Memory (Common_Control_Type'Class (Window));
         when Nm_Click =>
            On_Click (Common_Control_Type'Class (Window));
         when Nm_Dblclk =>
            On_Double_Click (Common_Control_Type'Class (Window));
         when Nm_Return =>
            On_Return (Common_Control_Type'Class (Window));
         when Nm_Rclick =>
            On_Right_Click (Common_Control_Type'Class (Window));
         when Nm_Rdblclk =>
            On_Right_Double_Click (Common_Control_Type'Class (Window));
         when Nm_Setfocus =>
            On_Focus (Common_Control_Type'Class (Window));
         when Nm_Killfocus =>
            On_Lost_Focus (Common_Control_Type'Class (Window));
         when Nm_Hover =>
            On_Hover (Common_Control_Type'Class (Window));
         when Nm_Header_Click =>
            declare
               Nmlistview_Pointer : Pointer_To_Nmlistview_Type :=
                 Message_To_Nmlistview_Pointer (Message);
            begin
               On_Header_Click (Ex_List_View_Control_Type'Class (Window),
                                Integer (Nmlistview_Pointer.Isubitem));
            end;
         when Nm_Customdraw =>
            if Window.Color_Mode  then
               declare
                  Lvcd_Ptr : Pointer_To_Nmlvcustomdraw_Type :=
                    Message_To_Nmlvcustomdraw_Pointer (Message);
               begin
                  Do_On_Redraw_Items (Lvcd_Ptr, Window, Return_Value);
               end;
            end if;
         when Nm_Item_Changed =>
            declare
               Nmlistview_Pointer : Pointer_To_Nmlistview_Type :=
                 Message_To_Nmlistview_Pointer (Message);
            begin
               On_Item_Changed (Ex_List_View_Control_Type'Class (Window),
                                Integer (Nmlistview_Pointer.Iitem));
            end;
         when others =>
            null;
      end case;

      On_Notify (List_View_Control_Type (Window),
                 Message, Control, Return_Value);
   end On_Notify;


   --------------------------------------------------------------------------
   --  body-functions
   --------------------------------------------------------------------------

   ----------------
   -- Get_Lparam --
   ----------------

   function Get_Lparam (Control   : in Ex_List_View_Control_Type;
                        Index     : in Natural;
                        Sub_Index : in Natural)
                       return System.Address
   is
      Item : Lvitem;

      procedure Sendmessagea
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Getitema;
         Wparam : Integer           := 0;
         Lparam : System.Address    := Item'Address);
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      procedure Sendmessagew
        (Hwnd   : Interfaces.C.long := Handle (Control);
         Umsg   : Interfaces.C.int  := Lvm_Getitemw;
         Wparam : Integer           := 0;
         Lparam : System.Address    := Item'Address);
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);

   begin
      Item.Mask := Lvif_Param;
      Item.Item := Index;
      Item.Subitem := Sub_Index;

      case Character_Mode is
         when Unicode =>
            Sendmessagew;
         when ANSI =>
            Sendmessagea;
      end case;

      return Item.Lparam;
   end Get_Lparam;

   ------------------------
   -- Do_On_Redraw_Items --
   ------------------------

   procedure Do_On_Redraw_Items
     (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
      Control      : in out Ex_List_View_Control_Type;
      Return_Value : in out Interfaces.C.long)
   is
      use Interfaces.C;
   begin
      --  set color in redraw according to color_mode
      case Lvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
               Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.long (Cdds_Itemprepaint) =>
               Return_Value := Cdrf_Notifysubitemdraw;
         when Interfaces.C.long (Cdds_Itemprepaint + Cdds_Subitem) =>
            declare
               Array_Access : Extended_Data_Array_Access;
            begin
               Array_Access :=
                 Address_Conversion.To_Pointer
                   (Get_Lparam (Control,
                                Integer (Lvcd_Ptr.Nmcd.Dwitemspec),
                                0));

               Lvcd_Ptr.Clrtext :=
                 Array_Access (Integer (Lvcd_Ptr.Isubitem)).Text_Color;
               Lvcd_Ptr.Clrtextbk :=
                 Array_Access (Integer (Lvcd_Ptr.Isubitem)).Back_Color;
            end;

            Return_Value := Cdrf_Newfont;
         when others =>
            null;
      end case;
   end Do_On_Redraw_Items;

   -------------
   -- compare --
   -------------

   function Compare (Lparam1    : in System.Address;
                     Lparam2    : in System.Address;
                     Lparamsort : in Sort_Data_Type)
                    return Interfaces.C.int
   is

      function Sendmessagea
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := Lvm_Finditema;
         Wparam : Integer           := -1;
         Lparam : System.Address)
        return Interfaces.C.int;
      pragma Import (Stdcall, Sendmessagea,
                       "SendMessage" & Character_Mode_Identifier);

      function Sendmessagew
        (Hwnd   : Interfaces.C.long := 0;
         Umsg   : Interfaces.C.int  := Lvm_Finditemw;
         Wparam : Integer           := -1;
         Lparam : System.Address)
        return Interfaces.C.int;
      pragma Import (Stdcall, Sendmessagew,
                       "SendMessage" & Character_Mode_Identifier);

      Findinfo  : Findinfo_Type;
      Indexa    : Integer;
      Indexb    : Integer;
      Class_Ptr : Pointer_To_Base_Window_Class;
   begin

      --  get index
      case Character_Mode is
         when Unicode =>
            Findinfo.Lparam := Lparam1;
            Indexa := Integer
              (Sendmessagew (Hwnd   => Lparamsort.Control_Handle,
                             Lparam => Findinfo'Address));
            Findinfo.Lparam := Lparam2;
            Indexb := Integer
              (Sendmessagew (Hwnd   => Lparamsort.Control_Handle,
                             Lparam => Findinfo'Address));
         when ANSI =>
            Findinfo.Lparam := Lparam1;
            Indexa := Integer
              (Sendmessagea (Hwnd   => Lparamsort.Control_Handle,
                             Lparam => Findinfo'Address));
            Findinfo.Lparam := Lparam2;
            Indexb := Integer
              (Sendmessagea (Hwnd   => Lparamsort.Control_Handle,
                             Lparam => Findinfo'Address));
      end case;

      Class_Ptr := Window_From_Handle (Lparamsort.Control_Handle);

      declare
         Texta : GString := Text (Ex_List_View_Control_Type (Class_Ptr.all),
                                  Indexa,
                                  Lparamsort.Column);

         Textb : GString := Text (Ex_List_View_Control_Type (Class_Ptr.all),
                                  Indexb,
                                  Lparamsort.Column);
      begin

         --  comparision
         if Texta = Textb then
            return Interfaces.C.int (0);
         end if;
         if Texta < Textb then
            return Interfaces.C.int (-1 * Lparamsort.Factor);
         else
            return Interfaces.C.int (1 * Lparamsort.Factor);
         end if;
      end;
   end Compare;

   ----------------
   -- on_destroy --
   ----------------

   procedure On_Destroy (Window : in out Ex_List_View_Control_Type)is
      Array_Access : Extended_Data_Array_Access;
   begin
      for I in 0 .. Item_Count (Window) - 1 loop
         Array_Access := Address_Conversion.To_Pointer (Get_Lparam (Window,
                                                                    I,
                                                                    0));
         Free (Array_Access);
      end loop;

      On_Destroy (List_View_Control_Type (Window));
   end On_Destroy;

end GWindows.Common_Controls.Ex_LV;
