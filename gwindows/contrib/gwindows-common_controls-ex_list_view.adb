with System;
with Ada.unchecked_conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;
--with Ada.Text_Io; use Ada.Text_Io;

with GWindows.GStrings;
with GWindows.Drawing;

package body GWindows.Common_Controls.Ex_List_View is

   Lvm_First                    : constant := 16#1000#;
   Lvs_Ex_Gridlines             : constant := 1;
   Lvs_Ex_Headerdragdrop        : constant := 16;
   Lvs_Ex_Fullrowselect         : constant := 32;
   --  Lvs_Ex_Flatsb                : constant := 256;
   Lvm_Getitema                 : constant := Lvm_First + 5;
   Lvm_Setitema                 : constant := Lvm_First + 6;
   --  Lvm_Insertitema              : constant := Lvm_First + 7;
   Lvm_Finditema                : constant := Lvm_First + 13;
   Lvm_Sortitems                : constant := Lvm_First + 48;
   Lvm_Setextendedlistviewstyle : constant := Lvm_First + 54;
   Lvm_Getitemw                 : constant := Lvm_First + 75;
   Lvm_Setitemw                 : constant := Lvm_First + 76;
   --  Lvm_Insertitemw              : constant := Lvm_First + 77;
   Lvm_Finditemw                : constant := Lvm_First + 83;
   LVM_GETCOLUMNA               : constant := LVM_FIRST + 25;
   LVM_GETCOLUMNW               : constant := LVM_FIRST + 95;
   --LVM_SETCOLUMNA               : constant := LVM_FIRST + 26;
   --LVM_SETCOLUMNW               : constant := LVM_FIRST + 96;
   --  LVM_GETSUBITEMRECT           : constant := LVM_FIRST + 56;
   LVM_SETCOLUMNWIDTH           : constant := LVM_FIRST + 30;
   LVN_FIRST                    : constant := -100;
   LVN_INSERTITEM               : constant := LVN_FIRST - 2;
   LVN_DELETEITEM               : constant := LVN_FIRST - 3;
   Lvfi_Param                   : constant := 1;
   --  Lvif_Text                    : constant := 16#0001#;
   --  Lvif_Image                   : constant := 16#0002#;
   Lvif_Param                   : constant := 16#0004#;
   LVCF_TEXT                    : constant := 16#0004#;
   LVM_SETBKCOLOR               : constant := LVM_FIRST + 1;
   LVM_REDRAWITEMS              : Constant := LVM_FIRST + 21;
   LVM_GETHEADER                : constant := LVM_FIRST + 31;
   Lvm_Settextcolor             : constant := Lvm_First + 36;
   LVM_SETTEXTBKCOLOR           : constant := LVM_FIRST + 38;
   Cdrf_Notifyitemdraw          : constant := 16#00000020#;
   Cdds_Prepaint                : constant := 16#0001#;
   Cdds_Item                    : constant := 16#00010000#;
   Cdds_Itemprepaint            : constant := Cdds_Item + Cdds_Prepaint;
   Cdrf_Newfont                 : constant := 2;
   Cdrf_Notifysubitemdraw       : constant := 16#00000020#;
   Cdds_Subitem                 : constant := 16#00020000#;
   Nm_Header_Click              : constant := - 108;
   --  HDI_width                   : constant := 1;
   --  HDI_height                   : constant := HDI_WIDTH;
   HDI_TEXT                   : constant := 2;
   HDI_FORMAT                   : constant := 4;
   HDF_SORTUP                   : constant := 16#400#;
   HDF_SORTDOWN                 : constant := 16#200#;
   HDF_string                   : constant := 16#4000#;
   --  HDF_right                   : constant := 1;
   --  HDF_fixedwidth                   : constant := 16#100#;
   HDF_OWNERDRAW                : constant := 16#8000#;
   HDM_FIRST                    : constant := 16#1200#;
   HDM_GETITEMA                 : constant := HDM_FIRST + 3;
   HDM_GETITEMW                 : constant := HDM_FIRST + 11;
   HDM_SETITEMA                 : constant := HDM_FIRST + 4;
   HDM_SETITEMW                 : constant := HDM_FIRST + 12;
   HDM_GETITEMCOUNT             : constant := HDM_FIRST;
   --  HDM_GETITEMRECT              : constant := HDM_FIRST + 7;

   type Lvitem is
      Record
         Mask      : Interfaces.C.Unsigned := 0;
         Item      : Interfaces.C.Int               := 0;
         Subitem   : Interfaces.C.Int               := 0;
         State     : Interfaces.C.Unsigned := 0;
         Statemask : Interfaces.C.Unsigned := 0;
         Text      : Lptstr                := null;
         Textmax   : Interfaces.C.Int               := 0;
         Image     : Interfaces.C.Int;
         Lparam    : Gwindows.Types.Lparam;
         Indent    : Interfaces.C.Int;
      end record;
   type Lvitem_Access is access all Lvitem;

   type Nmcustomdraw_Type is
      record
         Hdr         : Gwindows.Base.Notification;
         Dwdrawstage : Interfaces.C.Long;
         Hdc         : Gwindows.Types.Handle;
         Rect        : Gwindows.Types.Rectangle_Type;
         Dwitemspec  : Interfaces.C.Long;
         Uitemstate  : Interfaces.C.Unsigned;
         Litemlparam : Gwindows.Types.lparam;
      end record;
   --  type Pointer_To_Nmcustomdraw_Type is access all Nmcustomdraw_Type;
   type Nmlvcustomdraw_Type is
      record
         Nmcd      : Nmcustomdraw_Type;
         Clrtext   : Color_Type;
         Clrtextbk : Color_Type;
         Isubitem  : Interfaces.C.Int;
      end record;
   type Pointer_To_Nmlvcustomdraw_Type is access all Nmlvcustomdraw_Type;

   type Nmlistview_Type is
      record
         Hdr       : Gwindows.Base.Notification;
         Iitem     : Interfaces.C.Int;
         Isubitem  : Interfaces.C.Int;
         Unewstate : Interfaces.C.Int;
         Uoldstate : Interfaces.C.Int;
         Uchanged  : Interfaces.C.Int;
         Point     : Gwindows.Types.Point_Type;
         Lparam    : Gwindows.Types.lparam;
      end record;
   type Pointer_To_Nmlistview_Type is access all Nmlistview_Type;

   type Lvcolumn_type is
      record
         Mask    : Interfaces.C.Unsigned := 0;
         Format  : Interfaces.C.Unsigned := 0;
         Width   : Integer               := 0;
         Text    : Lptstr                := null;
         Textmax : Integer               := 0;
         Subitem : Integer               := 0;
         Image   : Integer               := 0;
         Order   : Integer               := 0;
      end record;
   type Lvcolumn_Pointer is access all Lvcolumn_type;

   type Findinfo_Type is
      record
         Flags       : Interfaces.C.Unsigned := Lvfi_Param;
         Psz         : Lptstr                := null;
         Lparam      : Gwindows.Types.lparam;
         Point       : Gwindows.Types.Point_Type;
         Vkdirection : Interfaces.C.Unsigned := 0;
      end record;

   type Hditem_type is
      record
         Mask      : Interfaces.C.Unsigned := 0;
         Cxy       : Interfaces.C.Int      := 0;
         pszText   : Lptstr                := null;
         HBitmap   : Interfaces.C.Long     := 0;
         CchTextMax: Interfaces.C.Int      := 0;
         Fmt       : Interfaces.C.Int      := 0;
         Lparam    : System.Address;
         IImage    : Interfaces.C.Int      := 0;
         IOrdegr    : Interfaces.C.Int      := 0;
         Typ       : Interfaces.C.Unsigned := 0;
         PvFilter  : System.Address        ;
      end record;
   type Hditem_Pointer is access all Hditem_type;

   type Drawitem_type is
      record
         CtlType       : Interfaces.C.Unsigned;
         CtlID         : Interfaces.C.Unsigned;
         CtlItemID     : Interfaces.C.Unsigned;
         CtlItemAction : Interfaces.C.Unsigned;
         CtlItemState  : Interfaces.C.Unsigned;
         HwndItem      : Gwindows.Types.handle;
         Hdc           : Gwindows.Types.handle;
         RcItem        : Gwindows.Types.Rectangle_Type;
         ItemData      : System.Address;
      end record;
   type drawitem_Pointer is access all Drawitem_type;

   type Buffer is new GString_C (0 .. 255);
   type PBuffer is access all Buffer;

   function lvitem_To_Lparam is new Ada.Unchecked_Conversion (Lvitem_access, Gwindows.Types.Lparam);
   --  function Lparam_To_tvitem is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Lvitem_access);

   function internal_To_Lparam is new Ada.Unchecked_Conversion (internal_access, Gwindows.Types.Lparam);
   function Lparam_To_internal is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, internal_access);

   function Lvcolumn_To_lparam is new Ada.Unchecked_Conversion (Lvcolumn_Pointer, Gwindows.Types.Lparam);
   --  function Lparam_To_lvcolumn is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Lvcolumn_pointer);

   function hditem_To_Lparam is new Ada.Unchecked_Conversion (Hditem_pointer, Gwindows.Types.Lparam);
   --  function Lparam_To_hditem is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Hditem_pointer);

   --  function drawitem_To_Lparam is new Ada.Unchecked_Conversion (drawitem_pointer, Gwindows.Types.Lparam);
   function Lparam_To_drawitem is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, drawitem_pointer);

   function Address_To_lparam is new Ada.Unchecked_Conversion (System.Address, Gwindows.Types.Lparam);

   function color_To_Lparam is new Ada.Unchecked_Conversion (Color_type, Gwindows.Types.Lparam);

   function Message_To_Nmlvcustomdraw_Pointer is new Ada.Unchecked_Conversion(Gwindows.Base.Pointer_To_Notification,
                                                                              Pointer_To_Nmlvcustomdraw_Type);
   function Message_To_Nmlistview_Pointer is new Ada.Unchecked_Conversion(Gwindows.Base.Pointer_To_Notification,
                                                                          Pointer_To_Nmlistview_Type);

   function Handle_To_wparam is new Ada.Unchecked_Conversion (Gwindows.Types.Handle, Gwindows.Types.wparam);

   function To_PBuffer is new Ada.Unchecked_Conversion (LPTSTR, PBuffer);

   procedure Free_Color_array is new Ada.Unchecked_Deallocation(internal_color_Array_Type,
                                                                Internal_color_Array_Access);
   procedure Free_internal is new Ada.Unchecked_Deallocation(internal_Type,
                                                             Internal_Access);

   function Sendmessage (Hwnd   : Gwindows.Types.handle;
                         Umsg   : Interfaces.C.Int;
                         Wparam : Gwindows.Types.Wparam := 0;
                         Lparam : Gwindows.Types.Lparam := 0) return Gwindows.Types.lparam;
   pragma Import (Stdcall, Sendmessage, "SendMessage" & Character_Mode_Identifier);

   procedure Sendmessage_proc (Hwnd   : Gwindows.Types.handle;
                               Umsg   : Interfaces.C.Int;
                               Wparam : Gwindows.Types.Wparam := 0;
                               Lparam : Gwindows.Types.Lparam := 0);
   pragma Import (Stdcall, Sendmessage_proc, "SendMessage" & Character_Mode_Identifier);

   procedure Redraw_subitem (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                             Control      : in out Ex_List_View_Control_Type;
                             Return_Value : in out Gwindows.Types.lresult               );

   procedure Redraw_item (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                          Control      : in out Ex_List_View_Control_Type;
                          Return_Value : in out Gwindows.Types.lresult              );

   procedure On_Free_Payload(Control: in out Ex_List_View_Control_Type;
                             Payload: out Data_access);

   procedure On_Header_Click (Control : in out Ex_List_View_Control_Type;
                              Column  : in     Integer                    );

   function Column_text(Control: in Ex_List_View_Control_Type;
                        Column: in Natural) Return Gstring;

   procedure Header_sorticon(Control: in out Ex_List_View_Control_Type;
                             Column: in Natural;
                             Direction: in Integer;
                             Enable: in Boolean := true);
   procedure Ownerdraw_flag(Control: in out Ex_List_View_Control_Type;
                            Column: in Natural;
                            Enable: in Boolean := true);
   procedure Draw_sorticon(Control: in out Ex_List_View_Control_Type;
                           Drawitem: in Drawitem_Type;
                           Direction: in Integer);

   -----------------------------------------------------------------------------------------
   function Get_Comctl_Version return natural is

      function Getmodulehandle(LpModulname: in Lptstr) return Gwindows.Types.Handle;
      pragma Import(Stdcall, Getmodulehandle, "GetModuleHandle" & Character_Mode_Identifier);

      type Dllversioninfo is
         record
            Cbsize: Interfaces.C.Long := 0;
            majorversion: Interfaces.C.long := 0;
            minorversion: Interfaces.C.long := 0;
            buildnumber: Interfaces.C.long := 0;
            platformid: Interfaces.C.long := 0;
         end record;
      type Dllversioninfo_Pointer is access all Dllversioninfo;

      type Dll_Get_Version_func is access
        function (Versioninfo: in dllversioninfo_pointer) return Interfaces.C.Unsigned_long;
      pragma Convention(C, Dll_Get_Version_func);

      use Interfaces.C;

      function getprocaddress(hmodule: in Gwindows.Types.Handle;
                              Lpprocname: in Char_array)
                             return dll_get_version_func;
      pragma Import(Stdcall, getprocaddress, "GetProcAddress");

      libname: Gstring_C := Gwindows.Gstrings.To_Gstring_C("comctl32");
      Procname: aliased constant Char_Array := To_C("DllGetVersion");
      ModHandle: Gwindows.Types.Handle;
      FuncPtr: Dll_Get_Version_Func;
      Info: aliased Dllversioninfo;
      Ret_Func: Interfaces.C.Unsigned_long;
      pragma Unreferenced (Ret_Func);

   begin
      -- get dll
      modhandle := Getmodulehandle(Lpmodulname => Libname(0)'Unchecked_Access);
      -- get dllgetversion
      funcPtr := Getprocaddress(Hmodule => Modhandle,
                                Lpprocname => procname);
      -- call dllgetversion
      Info.Cbsize := Info'Size / 8;
      Ret_Func := FuncPtr(Info'unchecked_access) ;
      return Natural(Info.MajorVersion);
   exception
      when others =>
         return 0;
   end get_comctl_version;
   -----------------------------------------------------------------------------------------
   function Create_Internal(Control: in Ex_List_View_Control_Type) return Internal_Access is
      Int: Internal_Access;
      nullColors: constant Internal_Color_Type := (Textcolor => Nullcolor,
                                                   Backcolor => Nullcolor);
   begin
      Int := new Internal_Type;
      if Column_Count(Control) > 0 then
         Int.Colors := new Internal_Color_Array_Type(0..Column_Count(Control)-1);
         Int.Colors.all := (others => nullcolors);
      end if;

      return Int;
   end Create_Internal;
   -----------------------------------------------------------------------------------------
   procedure On_Create(Control: in out Ex_List_View_Control_Type)is
   begin
      Gwindows.Common_Controls.On_Create(Control => List_View_Control_Type(Control));
      -- pen for sort
      Gwindows.Drawing_Objects.Create_pen(Pen => Control.Sort_Object.Sort_Pen,
                                          Style => Gwindows.Drawing_Objects.Solid,
                                          Width => 1,
                                          Color => Sort_Icon_Pen_Color);
      -- brush for sort
      Gwindows.Drawing_Objects.Create_Solid_Brush (Brush => Control.Sort_Object.Sort_Brush,
                                                   Color => Sort_Icon_Brush_color);

      -- comctlversion for drawing sorticons
      Control.Comctl_Version := Get_Comctl_Version;

   end On_Create;
   -----------------------------------------------------------------------------------------
   function Get_internal (Control   : in     Ex_List_View_Control_Type;
                          Index     : in     Natural)
                         return Internal_access is

      Item : Lvitem;
      L_Umsg: Interfaces.C.Int;
   begin
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.Int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            L_Umsg := Lvm_Getitemw;
         when Ansi =>
            L_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(hwnd   => Handle(Control),
                       Umsg   => L_Umsg,
                       Wparam => 0,
                       Lparam => lvitem_To_Lparam(Item'Unrestricted_Access));

      return Lparam_To_Internal(Item.Lparam);
   exception
      when others =>
         return null;
   end Get_internal;
   -----------------------------------------------------------------------------------------
   procedure Set_Internal_color (Control   : in     Ex_List_View_Control_Type;
                                 Index     : in     Natural;
                                 Sub_Index : in     Natural;
                                 colors: in Internal_Color_Type)is

      Item : Lvitem;
      get_Umsg: Interfaces.C.Int;
      internal: Internal_access := null;
      nullColors: constant Internal_Color_Type := (Textcolor => Nullcolor,
                                                   Backcolor => nullcolor);
   begin
      -- get the lparam
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.Int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            get_Umsg := Lvm_Getitemw;
         when Ansi =>
            get_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(hwnd => Handle(Control),
                       Umsg => get_Umsg,
                       Wparam => 0,
                       Lparam => lvitem_To_Lparam(Item'Unrestricted_Access));

      Internal := Lparam_To_Internal(Item.Lparam);

      -- range?
      if Internal.Colors.all'Last < Sub_Index then
         declare
            Tmp_Colors: Internal_Color_Array_Access := new Internal_Color_Array_Type(0..Column_Count(Control)-1);
         begin
            Tmp_Colors.all := (others => nullcolors);
            Tmp_Colors(0..Internal.Colors.all'Last) := Internal.Colors.all;
            -- free the old array
            Free_Color_Array(Internal.Colors);
            Internal.colors := Tmp_Colors;
         end;
      end if;

      Internal.Colors(Sub_Index)  := Colors;

   end Set_Internal_color;
   -----------------------------------------------------------------------------------------
   procedure Set_Internal_payload (Control   : in     Ex_List_View_Control_Type;
                                   Index     : in     Natural;
                                   payload: in Data_access)is

      Item : Lvitem;
      get_Umsg: Interfaces.C.Int;
      internal: Internal_access := null;
   begin
      -- get the lparam
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.Int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            get_Umsg := Lvm_Getitemw;
         when Ansi =>
            get_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(hwnd => Handle(Control),
                       Umsg => get_Umsg,
                       Wparam => 0,
                       Lparam => lvitem_To_Lparam(Item'Unrestricted_Access));

      Internal := Lparam_To_Internal(Item.Lparam);

      -- set payload
      Internal.User_Data := Payload;

   end Set_Internal_payload;
   -----------------------------------------------------------------------------------------
   procedure On_Message(control       : in out Ex_List_View_Control_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     Gwindows.Types.wparam;
                        lParam       : in     Gwindows.Types.lparam;
                        Return_Value : in out Gwindows.Types.lresult)is
      use Interfaces.C;
      WM_DRAWITEM                : constant := 16#002B#;
      ODT_HEADER                 : constant := 100;
   begin
      case Message is
         when WM_DRAWITEM =>
            -- ab comctl 6 direkt hdm_setitem rufen für sorticons
            declare
               Drawitem: Drawitem_Pointer := null;
            begin
               Drawitem := Lparam_To_Drawitem(Lparam);
               if Drawitem /= null and then Drawitem.all.Ctltype = ODT_HEADER then
                  Draw_sorticon(Control, Drawitem.all, Control.Sort_Object.Sort_direction);
               end if;
               Return_Value := 1;
            end;
         when others =>
            Gwindows.Common_Controls.On_Message(List_View_Control_Type(control),Message,Wparam, Lparam, Return_Value);
      end case;

   end On_Message;
   --------------------------------------------------------------------------------------------
   procedure On_Notify (Window       : in out Ex_List_View_Control_Type;
                        Message      : in     Gwindows.Base.Pointer_To_Notification;
                        Control      : in     Gwindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out gwindows.Types.lresult    ) is

      use Interfaces.C;

      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      Nm_Customdraw   : constant := - 12;
   begin
      -- customdraw subitem
      if Message.Code = Nm_Customdraw and then
        Window.Color_Mode = subitem then
         declare
            Lvcd_Ptr : constant Pointer_To_Nmlvcustomdraw_Type :=
              Message_To_Nmlvcustomdraw_Pointer (Message);
         begin
            Redraw_subitem(Lvcd_Ptr, Window, Return_Value);
         end;
         -- customdraw item
      elsif Message.Code = Nm_Customdraw and then
        Window.Color_Mode = item_alternately then
         declare
            Lvcd_Ptr : constant Pointer_To_Nmlvcustomdraw_Type :=
              Message_To_Nmlvcustomdraw_Pointer (Message);
         begin
            Redraw_item(Lvcd_Ptr, Window, Return_Value);
         end;
         -- header click
      elsif Message.Code = Nm_Header_Click then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type :=
              Message_To_Nmlistview_Pointer(Message);
         begin
            On_Header_Click (Window,
                             Integer(Nmlistview_Pointer.Isubitem));
         end;
      elsif Message.Code = Lvn_Insertitem then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type := Message_To_Nmlistview_Pointer(Message);
            Item : Lvitem;
            L_Umsg: Interfaces.C.Int;
         begin
            -- setitem
            Item.Mask := Lvif_param;
            Item.Item := Nmlistview_Pointer.Iitem;
            Item.Lparam := internal_To_Lparam(Create_Internal(window));
            case Character_Mode is
               when Unicode =>
                  l_Umsg := Lvm_Setitemw;
               when Ansi =>
                  l_Umsg := Lvm_Setitema;
            end case;
            Sendmessage_proc(hwnd => Handle(window),
                             Umsg => l_Umsg,
                             Wparam => 0,
                             Lparam => lvitem_To_Lparam(Item'Unrestricted_Access));
         exception
            when others =>
               null;
         end;
      elsif Message.Code = Lvn_deleteitem then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type := Message_To_Nmlistview_Pointer(Message);
         begin
            if window.Color_Mode = Item_Alternately then
               -- redraw on items
              Sendmessage_proc (Hwnd => Handle(window),
                                Umsg => LVM_REDRAWITEMS,
                                Wparam => Gwindows.Types.Wparam(Nmlistview_Pointer.Iitem),
                                Lparam => Gwindows.Types.Lparam(Item_Count(window)-1));
            end if;
         end;
      else
         Gwindows.Common_Controls.On_Notify(List_View_Control_Type(Window), Message, Control, Return_Value);
      end if;

   end On_Notify;
   -----------------------------------------------------------------------------------------
   procedure On_Destroy (control : in out Ex_List_View_Control_Type)is
      Int: Internal_Access;
   begin
      for Index in 0..Item_Count(Control)-1 loop
         Int := Get_Internal(Control, Index);
         if Int /= null then
            if Int.Colors /= null then
               Free_Color_Array(Int.Colors);
            end if;
            -- free the payload-data
            On_Free_Payload(Control => Control,
                            Payload => Int.User_Data);
         end if;
         Free_Internal(Int);
      end loop;
      On_Destroy(List_View_Control_Type(control));
   end On_Destroy;
   -----------------------------------------------------------------------------------------
   procedure Redraw_subitem (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                             Control      : in out Ex_List_View_Control_Type;
                             Return_Value : in out Gwindows.Types.lresult              ) is
      use Interfaces.C;
   begin
      -- set color in redraw according to color_mode
      case Lvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
            Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.Long(Cdds_Itemprepaint) =>
            Return_Value := Cdrf_Notifysubitemdraw;
         when Interfaces.C.Long(Cdds_Itemprepaint + Cdds_Subitem) =>
            declare
               internal: constant Internal_Access :=
                  Get_Internal(Control => Control,
                               Index => Integer(Lvcd_Ptr.Nmcd.Dwitemspec));
            begin
               if Internal /= null and then
                 Internal.Colors /= null and then
                 Internal.Colors'Last >=  Integer(Lvcd_Ptr.Isubitem) and then
                 Internal.colors(Integer(Lvcd_Ptr.Isubitem)).TextColor /= NullColor then
                  Lvcd_Ptr.Clrtext := Internal.colors(Integer(Lvcd_Ptr.Isubitem)).TextColor;
               else
                  Lvcd_Ptr.Clrtext := Control.Control_TextColor;
               end if;
               if Internal /= null and then
                 Internal.Colors /= null and then
                 Internal.Colors'Last >=  Integer(Lvcd_Ptr.Isubitem) and then
                 Internal.colors(Integer(Lvcd_Ptr.Isubitem)).backColor /= NullColor then
                  Lvcd_Ptr.Clrtextbk := Internal.colors(Integer(Lvcd_Ptr.Isubitem)).backColor;
               else
                  Lvcd_Ptr.Clrtextbk := Control.Control_Backcolor;
               end if;
            end;
            Return_Value := Cdrf_Newfont;
         when others =>
            Return_Value := Cdrf_Newfont;
      end case;
   end Redraw_subitem;
   -----------------------------------------------------------------------------------------
   procedure Redraw_item (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                          Control      : in out Ex_List_View_Control_Type;
                          Return_Value : in out Gwindows.Types.lresult              ) is
      use Interfaces.C;
   begin
      case Lvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
            Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.Long(Cdds_Itemprepaint) =>
            Return_Value := Cdrf_Notifysubitemdraw;
            Lvcd_Ptr.Clrtext := Control.Control_textcolor;
            if Integer(Lvcd_Ptr.Nmcd.Dwitemspec) mod 2 = 0 then
               Lvcd_Ptr.Clrtextbk := Control.Alt_Color1;
            else
               Lvcd_Ptr.Clrtextbk := Control.Alt_Color2;
            end if;
            Return_Value := Cdrf_Newfont;
         when others =>
            Return_Value := Cdrf_Newfont;
      end case;
   end Redraw_item;
   ----------------------------------------------------------------------------------------------------
   procedure On_Header_Click (Control : in out Ex_List_View_Control_Type;
                              Column  : in     Integer                    ) is
   begin
      Sort(Control => Control, Column => Column, Direction => Auto, Show_icon => True);
   end On_Header_Click;
   ----------------------------------------------------------------------------------------------------
   procedure Draw_sorticon(control: in out Ex_List_View_Control_Type;
                           drawitem: in Drawitem_Type;
                           Direction: in integer)is
      Canvas: Gwindows.Drawing.Canvas_Type;
      paint_Left: Integer;
      Max_Width: integer;
      Columntext: constant Gstring := Column_text(Control => Control, Column => Integer(Drawitem.Ctlitemid));
      Columntext_Last: natural := Columntext'Last;
      Size: Gwindows.Types.Size_Type;
      Icon_Width, Icon_height: Natural;
   begin
      Gwindows.Drawing.Handle(Canvas, Drawitem.Hdc);


      -- get left for paint
      Paint_Left := Drawitem.Rcitem.Left + 1;

      Icon_height := Natural(Drawitem.Rcitem.Bottom / 3);
      Icon_Width := Icon_Height;
      if Icon_Width mod 2 > 0 then
         Icon_Width := Icon_Width + 1;
      end if;

      Max_Width := Drawitem.rcitem.Right - Drawitem.rcitem.Left - (4 * Icon_width);

      if Max_Width < 0 then
         return;
      end if;

      -- check string
      while Columntext_Last > 0 loop
         if Columntext_Last < Columntext'Last then
            Size := Gwindows.Drawing.Text_Output_Size (Canvas => Canvas,
                                                       Text => Columntext(1..Columntext_last) & "...");
         else
            Size := Gwindows.Drawing.Text_Output_Size (Canvas => Canvas,
                                                       Text => Columntext);
         end if;
         if Size.Width <= Max_Width then
            exit;
         end if;
         Columntext_Last := Columntext_Last - 1;
      end loop;

      -- put the string
      if Columntext_Last = Columntext'last then
         Gwindows.Drawing.Put(Canvas => Canvas,
                              X => Paint_Left + (3 * Icon_width),
                              Y => 2,
                              Text => Columntext);
      elsif Columntext_Last > 0 then
         Gwindows.Drawing.Put(Canvas => Canvas,
                              X => Paint_Left + (3 * Icon_width),
                              Y => 2,
                              Text => Columntext(1..Columntext_Last) & "...");
      end if;

      -- paint the polygon
      Gwindows.Drawing.Select_Object(Canvas => Canvas, Object => Control.sort_Object.Sort_Pen);
      Gwindows.Drawing.Select_Object(Canvas => Canvas, Object => Control.sort_Object.Sort_brush);

      declare
         Pt_Array  : Gwindows.Types.Point_Array_Type(1..3);
         Pt_Top    : constant Natural := Icon_height;
         Pt_Bottom : constant Natural := Drawitem.rcitem.bottom - Icon_height -1;
         Pt_Left   : constant Natural := Icon_width;
         Pt_Right  : constant Natural := 2 * Icon_width;
      begin
         if Icon_Height > 5 then
            Icon_Height := Icon_Height -1;
         end if;
         -- up
         if Direction = 1 then
            Pt_Array(1).X := Pt_Left + Paint_Left + Natural(Icon_width / 2); Pt_Array(1).y := Pt_Top;---1;
            Pt_Array(2).X := Pt_right + Paint_Left; Pt_Array(2).y := Pt_Bottom;---1;
            Pt_Array(3).X := Pt_Left + Paint_left; Pt_Array(3).y := Pt_Bottom;---1;
         else
            -- down
            Pt_Array(1).X := Pt_Left + Paint_Left; Pt_Array(1).y := Pt_Top;
            Pt_Array(2).X := Pt_right + Paint_Left; Pt_Array(2).y := Pt_Top;
            Pt_Array(3).X := Pt_Left + Paint_Left + Natural(Icon_width / 2); Pt_Array(3).y := Pt_bottom;
         end if;

         Gwindows.Drawing.Polygon (Canvas => Canvas, Vertices => Pt_array);
      end;

   end Draw_sorticon;
   ----------------------------------------------------------------------------------------------------
--     procedure Column_text(Control: in out Ex_List_View_Control_Type;
--                           Column: in Natural;
--                           Text: in gstring)
--     is
--        C_Text : Buffer;
--        Lv: LvColumn_type;
--        Get_Umsg: Interfaces.C.Int;
--        Set_Umsg: Interfaces.C.int;
--     begin
--        case Character_Mode is
--           when Unicode =>
--              get_Umsg := LVM_GETCOLUMNW;
--              set_Umsg := LVM_SETCOLUMNW;
--           when Ansi =>
--              get_Umsg := LVM_GETCOLUMNA;
--              set_Umsg := LVM_SETCOLUMNA;
--        end case;

--        Lv.Mask := Lvcf_Text;
--        Lv.Text := C_Text (0)'Unchecked_Access;
--        Lv.Textmax := 255;

--        -- get the lvcolumn
--        Sendmessage_proc(Hwnd => Handle(Control),
--                         Umsg => Get_uMsg,
--                         Wparam => Gwindows.Types.To_Wparam(Column),
--                         Lparam => Lvcolumn_To_Lparam(Lv'unrestricted_access));
--        -- set the new text
--        declare
--           New_C_Text: Gstring_c := Gwindows.Gstrings.To_Gstring_C (Text);
--        begin
--           -- set the lvcolumn
--           Lv.Text := New_C_Text(0)'Unchecked_Access;
--           Sendmessage_proc(Hwnd => Handle(Control),
--                            Umsg => set_uMsg,
--                            Wparam => Gwindows.Types.To_Wparam(Column),
--                            Lparam => Lvcolumn_To_Lparam(Lv'unrestricted_access));
--        end;

--     end Column_Text;
   ----------------------------------------------------------------------------------------------------
   function Column_text(Control: in Ex_List_View_Control_Type;
                        Column: in Natural) Return Gstring is
      C_Text : Buffer;
      Lv: LvColumn_type;
      Get_Umsg: Interfaces.C.Int;
   begin
      case Character_Mode is
         when Unicode =>
            get_Umsg := LVM_GETCOLUMNW;
         when Ansi =>
            get_Umsg := LVM_GETCOLUMNA;
      end case;

      Lv.Mask := Lvcf_Text;
      Lv.Text := C_Text (0)'Unchecked_Access;
      Lv.Textmax := 255;

      -- get the lvcolumn
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => Get_uMsg,
                       Wparam => Gwindows.Types.To_Wparam(Column),
                       Lparam => Lvcolumn_To_Lparam(Lv'unrestricted_access));

      return Gwindows.Gstrings.To_Gstring_From_C(Gstring_C (To_Pbuffer (Lv.Text).all));

   end Column_Text;
   ----------------------------------------------------------------------------------------------------
   procedure Header_sorticon(Control: in out Ex_List_View_Control_Type;
                             Column: in Natural;
                             Direction: in Integer;
                             Enable: in Boolean := true)is
      use Interfaces.C;
      use Gwindows.Gstrings;
      Header: Gwindows.Types.Handle;
      Hd: aliased Hditem_Type;
      C_Text: Buffer;
      L_setUmsg: Interfaces.C.Int;
      L_getUmsg: Interfaces.C.Int;
   begin
      case Character_Mode is
         when Unicode =>
            L_setUmsg := Hdm_SetitemW; L_getUmsg := Hdm_GetitemW;
         when Ansi =>
            L_setUmsg := Hdm_SetitemA; L_getUmsg := Hdm_GetitemA;
      end case;

      -- get the header
      Header := Gwindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => Lvm_Getheader,
                                                     wparam => 0,
                                                     Lparam => 0));

      -- get the item
      Hd.Mask := Hdi_Format + Hdi_Text;
      Hd.Psztext := C_Text (0)'Unchecked_Access;
      Hd.CchTextMax := 255;
      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_getUmsg,
                       Wparam => Gwindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(hd'Unchecked_Access));

      -- update item
      Hd.Mask := Hdi_Format + Hdi_Text;
      -- remove the old icon flag
      if (Unsigned(Hd.Fmt) and unsigned(Hdf_Sortup)) > 0 then
         Hd.Fmt := Hd.Fmt - Hdf_Sortup;
      end if;
      if (Unsigned(Hd.Fmt) and unsigned(Hdf_Sortdown)) > 0 then
         Hd.Fmt := Hd.Fmt - Hdf_Sortdown;
      end if;

      -- set the new icon flag
      if Enable then
         if Direction = 1 then -- up
            Hd.Fmt := Hd.Fmt + Hdf_Sortup;
         elsif Direction = -1 then -- down
            Hd.Fmt := Hd.Fmt + Hdf_sortdown;
         end if;
      end if;

      -- text
      Hd.cchtextMax := Int(To_Gstring_From_C(Gstring_C(To_Pbuffer(Hd.Psztext).all))'Last + 1);
      -- set the new item
      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_setUmsg,
                       Wparam => Gwindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(hd'Unchecked_Access));

   end Header_sorticon;

   procedure Ownerdraw_flag(Control: in out Ex_List_View_Control_Type;
                            Column: in Natural;
                            Enable: in Boolean := true)is
      use Interfaces.C;
      Header: Gwindows.Types.Handle;
      Hd: aliased Hditem_Type;
      L_Umsg: Interfaces.C.Int;
   begin
      -- get the header
      Header := Gwindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => Lvm_Getheader,
                                                     wparam => 0,
                                                     Lparam => 0));
      Hd.Mask := Hdi_Format;
      Hd.Fmt := Hdf_String;
      if Enable then
         Hd.Fmt := Hd.Fmt + Hdf_Ownerdraw;
      end if;

      case Character_Mode is
         when Unicode =>
            L_Umsg := Hdm_SetitemW;
         when Ansi =>
            L_Umsg := Hdm_SetitemA;
      end case;

      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_Umsg,
                       Wparam => Gwindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(hd'Unchecked_Access));

   end Ownerdraw_flag;
   ----------------------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------------------
   function Column_Count(Control    : in Ex_List_View_Control_Type) return Natural is
      use Gwindows.types;
      Header: Gwindows.Types.Handle;
      Count: Gwindows.Types.lparam;
   begin
      Header := Gwindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => Lvm_Getheader));

      Count := Sendmessage(Hwnd => Header,
                           Umsg => Hdm_Getitemcount);
      if To_Integer(Lresult(Count)) < 0 then
         Raise_Exception(Elv_Exception'Identity, "Error on HDM_GETITEMCOUNT.");
      end if;

      return Natural(Count);
   end Column_Count;
   ----------------------------------------------------------------------------------------------------
   procedure Autosize(Control: in out Ex_List_View_Control_Type;
                      Column : in natural;
                      Sizing: in Autosize_Type := headersize)is

      LVSCW_AUTOSIZE            : constant := -1;
      LVSCW_AUTOSIZE_USEHEADER  : constant := -2;
   begin
      case Sizing is
         when Columnsize =>
            Sendmessage_proc (Hwnd => Handle (Control),
                              Umsg => Lvm_SETCOLUMNWIDTH,
                              Wparam => Gwindows.Types.To_wparam(Column),
                              Lparam  => Gwindows.Types.To_Lparam(Lvscw_Autosize));
         when Headersize =>
            if Control.Sort_Object.Sort_Column = Column and then
              Control.Sort_Object.Icon_visible then
               -- width manually calculate
               declare
                  Canvas: Gwindows.Drawing.Canvas_Type;
                  Font: Gwindows.Drawing_Objects.Font_Type;
                  Size: Gwindows.Types.Size_Type;
                  Offset: Natural;
               begin
                  Get_Canvas(Control, canvas);
                  Get_Font(Control, Font);
                  Gwindows.Drawing.Select_Object(Canvas, Font);
                  Size := Gwindows.Drawing.Text_Output_Size (Canvas, Column_Text(Control, column));
                  Offset := (Size.Height + 4) / 3;
                  if Offset mod 2 > 0 then
                     Offset := Offset + 1;
                  end if;
                  Offset := Offset * 4;
                  Sendmessage_proc (Hwnd => Handle (Control),
                                    Umsg => Lvm_SETCOLUMNWIDTH,
                                    Wparam => Gwindows.Types.To_wparam(Column),
                                    Lparam  => Gwindows.Types.To_Lparam(Size.Width + offset));
               end;
            else
               Sendmessage_proc (Hwnd => Handle (Control),
                                 Umsg => Lvm_SETCOLUMNWIDTH,
                                 Wparam => Gwindows.Types.To_wparam(Column),
                                 Lparam  => Gwindows.Types.To_Lparam(Lvscw_Autosize_useheader));
            end if;
      end case;
   end Autosize;
   ----------------------------------------------------------------------------------------------------
   procedure Set_Extended_Style (Control : in     Ex_List_View_Control_Type;
                                 Style   : in     Extended_Style_Type ) is
   begin
      case Style is
         when Grid =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Gridlines,
                             Lparam => Lvs_Ex_Gridlines);
         when Header_Drag_Drop =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Headerdragdrop,
                             Lparam => Lvs_Ex_Headerdragdrop);
         when Full_Row_Select =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Fullrowselect,
                             Lparam => Lvs_Ex_Fullrowselect);
      end case;
   end Set_Extended_Style;
   ----------------------------------------------------------------------------------------------------
   procedure Remove_Extended_Style (Control : in     Ex_List_View_Control_Type;
                                    Style   : in     Extended_Style_Type ) is
   begin
      case Style is
         when Grid =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Gridlines,
                             Lparam => 0);
         when Header_Drag_Drop =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Headerdragdrop,
                             Lparam => 0);
         when Full_Row_Select =>
            Sendmessage_proc(Hwnd => Handle(Control),
                             Umsg => Lvm_Setextendedlistviewstyle,
                             Wparam => Lvs_Ex_Fullrowselect,
                             Lparam => 0);
      end case;
   end Remove_Extended_Style;
   ----------------------------------------------------------------------------------------------------
   function Color_Mode(Control : in Ex_List_View_Control_Type) return Color_Mode_Type is
   begin
      return Control.Color_Mode;
   end Color_Mode;
   ----------------------------------------------------------------------------------------------------
   procedure Text_Color (Control : in  out Ex_List_View_Control_Type;
                         Color   : in     Color_Type                 ) is
   begin
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => Lvm_Settextcolor,
                       Wparam => 0,
                       Lparam => Color_To_Lparam(Color));
      Control.Color_Mode := All_Items;
      Control.Control_Textcolor := Color;
   end Text_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Back_Color (Control : in  out  Ex_List_View_Control_Type;
                         Color   : in     Color_Type                 ) is
   begin
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => Lvm_Settextbkcolor,
                       Wparam => 0,
                       Lparam => Color_To_Lparam(Color));
      Control.Color_Mode := All_Items;
      Control.Control_backcolor := Color;
   end Back_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Control_Back_Color (Control : in  out  Ex_List_View_Control_Type;
                                 Color   : in     Color_Type                 ) is
   begin
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => Lvm_Setbkcolor,
                       Wparam => 0,
                       Lparam => Color_To_Lparam(Color));
   end Control_Back_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Set_Alternately_Colors(Control : in  out  Ex_List_View_Control_Type;
                                    Color1: in Color_Type;
                                    Color2:in Color_Type)is
   begin
      Control.Alt_Color1 := Color1;
      Control.Alt_Color2 := Color2;
      Control.Color_Mode := Item_Alternately;
   end Set_Alternately_Colors;
   ----------------------------------------------------------------------------------------------------
   procedure subItem_Color (Control    : in out Ex_List_View_Control_Type;
                            Text_Color : in     Color_Type := black;
                            back_Color : in     Color_Type := white;
                            Index      : in     Integer := -1;
                            Sub_Index  : in     Integer := -1            )is

      colors: Internal_Color_Type;
   begin
      if Column_Count(Control) = 0 then
         Ada.Exceptions.Raise_Exception(Elv_Exception'Identity, "No columns!");
      end if;

      if Index < 0 and Sub_Index < 0 then
         Ada.Exceptions.Raise_Exception(Elv_Exception'Identity, "No index/subindex!");
      end if;

      colors.Textcolor := Text_Color;
      colors.backcolor := back_Color;

      if Index >= 0 and Sub_Index >= 0 then -- subitem
         Set_Internal_color (Control => Control,
                             Index   => Index,
                             Sub_Index => Sub_Index,
                             colors => colors);
      elsif Index >= 0 then -- row
         for P_Sub_Index in 0..Column_Count(Control)-1 loop
            Set_Internal_color (Control => Control,
                                Index   => Index,
                                Sub_Index => P_Sub_Index,
                                colors => colors);
         end loop;
      elsif Sub_Index >= 0 then -- column
         for P_Index in 0..Item_Count(Control)-1 loop
            Set_Internal_color (Control => Control,
                                Index   => P_Index,
                                Sub_Index => Sub_Index,
                                colors => colors);
         end loop;
      end if;
      Control.Color_Mode := subitem;
   end subItem_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Color_Mode(Control : in out Ex_List_View_Control_Type;
                        Mode: in Color_Mode_type;
                        Redraw: in Boolean := true)is
   begin
      Control.Color_Mode := Mode;
      if Redraw then
         Gwindows.Common_Controls.Ex_List_View.Redraw(Control);
      end if;
   end Color_Mode;
   ----------------------------------------------------------------------------------------------------
   procedure Item_Data(Control : in Ex_List_View_Control_Type;
                       Index: in Natural;
                       Payload: in Data_access)is

   begin
      Set_Internal_Payload(Control, Index, Payload);
   end Item_Data;
   ----------------------------------------------------------------------------------------------------
   function Item_Data(Control : in Ex_List_View_Control_Type;
                      Index: in Natural) return Data_access is

   begin
      return Get_Internal(Control, Index).User_Data;
   end Item_Data;
   ----------------------------------------------------------------------------------------------------
   procedure On_Free_Payload(Control: in out Ex_List_View_Control_Type;
                             Payload: out Data_access)is
   begin
      if Control.On_Free_Payload /= null then
         Control.On_Free_Payload(Control, Payload);
      end if;
   end On_Free_Payload;
   ----------------------------------------------------------------------------------------------------
   procedure On_Free_Payload_Handler(Control: in out Ex_List_View_Control_Type;
                                     Event: in Free_Payload_Event)is
   begin
      Control.On_Free_Payload := Event;
   end On_Free_Payload_Handler;
   ----------------------------------------------------------------------------------------------------
   function On_Compare(
               Control: in Ex_List_View_Control_Type;
               Column : in Natural;
               Value1 : in GString;
               Value2 : in GString) return Integer
   is
   begin
     return Fire_On_Compare(Control, Column, Value1, Value2);
   end On_Compare;
   --
   procedure On_Compare_Handler(Control: in out Ex_List_View_Control_Type;
                                Event: in Compare_Event)is
   begin
      Control.On_Compare_Event := Event;
   end On_Compare_Handler;
   --
   function Fire_On_Compare(
               Control: in Ex_List_View_Control_Type;
               Column : in Natural;
               Value1 : in GString;
               Value2 : in GString) return Integer
   is
   begin
      if Control.On_Compare_Event /= null then
         return Control.On_Compare_Event(
            Control, Column, Value1, Value2
         );
      end if;
      if Value1 = Value2 then
         return 0;
      elsif Value1 > Value2 then
         return 1;
      else
         return -1;
      end if;
   end Fire_On_Compare;
   ----------------------------------------------------------------------------------------------------
   procedure Sort(Control: in out Ex_List_View_Control_Type;
                  Column: in Natural;
                  Direction: in Sort_Direction_Type;
                  Show_Icon: in Boolean := true)
   is
      function On_compare_internal
        (Lparam1    : in     Gwindows.Types.lparam;
         Lparam2    : in     Gwindows.Types.lparam;
         Lparamsort : in     Gwindows.Types.handle)
      return Interfaces.C.Int;
      pragma Convention (Stdcall, On_Compare_internal);
      function On_compare_internal
        (Lparam1    : in     Gwindows.Types.lparam;
         Lparam2    : in     Gwindows.Types.lparam;
         Lparamsort : in     Gwindows.Types.handle)
      return Interfaces.C.Int
      is
         use Gwindows.Types;
         Findinfo  : Findinfo_Type;
         Index1, Index2: Natural;
         L_Umsg: Interfaces.C.Int;
      begin
         -- Get the index from lparam
         case Character_Mode is
            when Unicode =>
               l_Umsg := Lvm_FindItemW;
            when Ansi =>
               l_Umsg := Lvm_FindItemA;
         end case;
         Findinfo.Lparam := Lparam1;
         Index1 := Integer(Sendmessage(Hwnd => Lparamsort,
                                       Umsg => L_Umsg,
                                       Wparam => -1,
                                       Lparam => Address_To_Lparam(Findinfo'Address)));

         Findinfo.Lparam := Lparam2;
         Index2 := Integer(Sendmessage(Hwnd => Lparamsort,
                                       Umsg => L_Umsg,
                                       Wparam => -1,
                                       Lparam => Address_To_Lparam(Findinfo'Address)));
         -- values
         declare
            Value1: constant Gstring := Text(Control => Control,
                                             item => Index1,
                                             Subitem => Control.Sort_Object.Sort_Column);
            Value2: constant Gstring := Text(Control => Control,
                                             item => Index2,
                                             Subitem => Control.Sort_Object.Sort_Column);
         begin
            -- We call the method, which is either overriden, or calls
            -- Fire_On_Compare which in turn calls the handler, if available, or
            -- applies a default alphabetical sorting.
            return Interfaces.C.Int(
               On_Compare(
                  Control => Ex_List_View_Control_Type'Class(Control),
                  Column  => Control.Sort_Object.Sort_column,
                  Value1  => Value1,
                  Value2  => Value2)
               * Control.Sort_Object.Sort_Direction
             );
         end;
      exception
         when E:others =>
            Raise_Exception(Elv_Exception'Identity, "error on on_compare: " & Exception_Information(E));
      end On_compare_internal;
   --
   begin
      -- same column, reverse sort direction
      case Direction is
         When Auto =>
            if Control.Sort_Object.Sort_Column = Column then
               Control.Sort_Object.Sort_Direction := Control.Sort_Object.Sort_Direction * (-1);
            else
               if Control.sort_Object.Sort_Column >= 0 then
                  --reset the Icon
                  if Control.Comctl_version <= 5 then
                     Ownerdraw_flag(Control, Control.Sort_Object.Sort_Column, False);
                  else
                     Header_sorticon(Control, Control.Sort_Object.Sort_Column, 0, False);
                  end if;
               end if;

               -- new column, start with sorting up
               Control.sort_Object.Sort_Column := Column;
               Control.sort_Object.Sort_Direction := 1;
            end if;
         when Up =>
            Control.sort_Object.Sort_Column := Column;
            Control.sort_Object.Sort_Direction := 1;
         when Down =>
            Control.sort_Object.Sort_Column := Column;
            Control.sort_Object.Sort_Direction := -1;
      end case;

      -- start sorting
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => LVM_SORTITEMS,
                       Wparam => Handle_To_Wparam(Handle(Control)),
                       Lparam => Address_To_Lparam(On_Compare_internal'Address));

      Control.Sort_Object.Icon_visible := Show_Icon;

      -- draw the sort icon
      if Show_Icon then
         if Control.Comctl_Version <= 5 then
            Ownerdraw_flag(Control, Column, true);
         else
            Header_sorticon(Control, Column, Control.Sort_Object.Sort_Direction, True);
         end if;
      end if;

   end Sort;

   procedure Sort_Info(Control   : in Ex_List_View_Control_Type;
                       Column    : out Integer;
                       Direction : out Sort_Direction_Type)is
   begin
      Column := Control.Sort_Object.Sort_Column;
      case Control.Sort_Object.Sort_Direction is
         when 1 =>
            Direction := Up;
         when -1 =>
            Direction := Down;
         when others =>
            Direction := Up;
      end case;
   end Sort_Info;

end GWindows.Common_Controls.Ex_List_View;
