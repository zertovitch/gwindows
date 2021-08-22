with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;

with GWindows.GStrings;
with GWindows.Drawing;

package body GWindows.Common_Controls.Ex_List_View is

   Lvm_First                    : constant := 16#1000#;
   --  Lvs_Ex_Gridlines             : constant := 1;
   --  Lvs_Ex_Headerdragdrop        : constant := 16;
   --  Lvs_Ex_Fullrowselect         : constant := 32;
   --  Lvs_Ex_Flatsb                : constant := 256;
   Lvm_Getitema                 : constant := Lvm_First + 5;
   Lvm_Setitema                 : constant := Lvm_First + 6;
   --  Lvm_Insertitema              : constant := Lvm_First + 7;
   Lvm_Finditema                : constant := Lvm_First + 13;
   Lvm_Sortitems                : constant := Lvm_First + 48;
   --  Lvm_Setextendedlistviewstyle : constant := Lvm_First + 54;
   Lvm_Getitemw                 : constant := Lvm_First + 75;
   Lvm_Setitemw                 : constant := Lvm_First + 76;
   --  Lvm_Insertitemw              : constant := Lvm_First + 77;
   Lvm_Finditemw                : constant := Lvm_First + 83;
   --  LVM_GETCOLUMNA               : constant := Lvm_First + 25;
   --  LVM_GETCOLUMNW               : constant := Lvm_First + 95;
   --  LVM_SETCOLUMNA               : constant := LVM_FIRST + 26;
   --  LVM_SETCOLUMNW               : constant := LVM_FIRST + 96;
   --  LVM_GETSUBITEMRECT           : constant := LVM_FIRST + 56;
   LVM_SETCOLUMNWIDTH           : constant := Lvm_First + 30;
   LVN_FIRST                    : constant := -100;
   LVN_INSERTITEM               : constant := LVN_FIRST - 2;
   LVN_DELETEITEM               : constant := LVN_FIRST - 3;
   Lvfi_Param                   : constant := 1;
   --  Lvif_Text                    : constant := 16#0001#;
   --  Lvif_Image                   : constant := 16#0002#;
   Lvif_Param                   : constant := 16#0004#;
   --  LVCF_TEXT                    : constant := 16#0004#;
   LVM_SETBKCOLOR               : constant := Lvm_First + 1;
   LVM_REDRAWITEMS              : constant := Lvm_First + 21;
   LVM_GETHEADER                : constant := Lvm_First + 31;
   Lvm_Settextcolor             : constant := Lvm_First + 36;
   LVM_SETTEXTBKCOLOR           : constant := Lvm_First + 38;
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
      record
         Mask      : Interfaces.C.unsigned := 0;
         Item      : Interfaces.C.int               := 0;
         Subitem   : Interfaces.C.int               := 0;
         State     : Interfaces.C.unsigned := 0;
         Statemask : Interfaces.C.unsigned := 0;
         Text      : LPTSTR                := null;
         Textmax   : Interfaces.C.int               := 0;
         Image     : Interfaces.C.int;
         Lparam    : GWindows.Types.Lparam;
         Indent    : Interfaces.C.int;
      end record;
   type Lvitem_Access is access all Lvitem;

   type DWORD_PTR is mod 2 ** Standard'Address_Size;
   --  From the Windows documentation:
   --    "An unsigned long type for pointer precision. Use when casting
   --     a pointer to a long type to perform pointer arithmetic. (Also
   --     commonly used for general 32-bit parameters that have been
   --     extended to 64 bits in 64-bit Windows.)"

   type Nmcustomdraw_Type is
      record
         Hdr         : GWindows.Base.Notification;
         Dwdrawstage : Interfaces.C.long;
         Hdc         : GWindows.Types.Handle;
         Rect        : GWindows.Types.Rectangle_Type;
         Dwitemspec  : DWORD_PTR;  --  Fix 2020-07-10: was Interfaces.C.long;
         Uitemstate  : Interfaces.C.unsigned;
         Litemlparam : GWindows.Types.Lparam;
      end record;
   --  type Pointer_To_Nmcustomdraw_Type is access all Nmcustomdraw_Type;
   type Nmlvcustomdraw_Type is
      record
         Nmcd      : Nmcustomdraw_Type;
         Clrtext   : Color_Type;
         Clrtextbk : Color_Type;
         Isubitem  : Interfaces.C.int;
      end record;
   type Pointer_To_Nmlvcustomdraw_Type is access all Nmlvcustomdraw_Type;

   type Nmlistview_Type is
      record
         Hdr       : GWindows.Base.Notification;
         Iitem     : Interfaces.C.int;
         Isubitem  : Interfaces.C.int;
         Unewstate : Interfaces.C.int;
         Uoldstate : Interfaces.C.int;
         Uchanged  : Interfaces.C.int;
         Point     : GWindows.Types.Point_Type;
         Lparam    : GWindows.Types.Lparam;
      end record;
   type Pointer_To_Nmlistview_Type is access all Nmlistview_Type;

   --  type Lvcolumn_type is
   --     record
   --        Mask    : Interfaces.C.unsigned := 0;
   --        Format  : Interfaces.C.unsigned := 0;
   --        Width   : Integer               := 0;
   --        Text    : LPTSTR                := null;
   --        Textmax : Integer               := 0;
   --        Subitem : Integer               := 0;
   --        Image   : Integer               := 0;
   --        Order   : Integer               := 0;
   --     end record;
   --  type Lvcolumn_Pointer is access all Lvcolumn_type;

   type Findinfo_Type is
      record
         Flags       : Interfaces.C.unsigned := Lvfi_Param;
         Psz         : LPTSTR                := null;
         Lparam      : GWindows.Types.Lparam;
         Point       : GWindows.Types.Point_Type;
         Vkdirection : Interfaces.C.unsigned := 0;
      end record;

   type Hditem_type is
      record
         Mask      : Interfaces.C.unsigned := 0;
         Cxy       : Interfaces.C.int      := 0;
         pszText   : LPTSTR                := null;
         HBitmap   : Interfaces.C.long     := 0;
         CchTextMax: Interfaces.C.int      := 0;
         Fmt       : Interfaces.C.int      := 0;
         Lparam    : System.Address;
         IImage    : Interfaces.C.int      := 0;
         IOrdegr    : Interfaces.C.int      := 0;
         Typ       : Interfaces.C.unsigned := 0;
         PvFilter  : System.Address        ;
      end record;
   type Hditem_Pointer is access all Hditem_type;

   type Drawitem_Type is
      record
         CtlType       : Interfaces.C.unsigned;
         CtlID         : Interfaces.C.unsigned;
         CtlItemID     : Interfaces.C.unsigned;
         CtlItemAction : Interfaces.C.unsigned;
         CtlItemState  : Interfaces.C.unsigned;
         HwndItem      : GWindows.Types.Handle;
         Hdc           : GWindows.Types.Handle;
         RcItem        : GWindows.Types.Rectangle_Type;
         ItemData      : System.Address;
      end record;
   type Drawitem_Pointer is access all Drawitem_Type;

   type Buffer is new GString_C (0 .. 255);
   type PBuffer is access all Buffer;

   function Lvitem_To_Lparam is new Ada.Unchecked_Conversion (Lvitem_Access, GWindows.Types.Lparam);
   --  function Lparam_To_tvitem is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Lvitem_access);

   function Internal_To_Lparam is new Ada.Unchecked_Conversion (Internal_Access, GWindows.Types.Lparam);
   function Lparam_To_Internal is new Ada.Unchecked_Conversion (GWindows.Types.Lparam, Internal_Access);

   --  function Lvcolumn_To_Lparam is new Ada.Unchecked_Conversion (Lvcolumn_Pointer, GWindows.Types.Lparam);
   --  function Lparam_To_lvcolumn is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Lvcolumn_pointer);

   function Hditem_To_Lparam is new Ada.Unchecked_Conversion (Hditem_Pointer, GWindows.Types.Lparam);
   --  function Lparam_To_hditem is new Ada.Unchecked_Conversion (Gwindows.Types.Lparam, Hditem_pointer);

   --  function drawitem_To_Lparam is new Ada.Unchecked_Conversion (drawitem_pointer, Gwindows.Types.Lparam);
   function Lparam_To_Drawitem is new Ada.Unchecked_Conversion (GWindows.Types.Lparam, Drawitem_Pointer);

   function Address_To_Lparam is new Ada.Unchecked_Conversion (System.Address, GWindows.Types.Lparam);

   function Color_To_Lparam (C: Color_Type) return GWindows.Types.Lparam is
   begin
      return GWindows.Types.Lparam (C);
   end Color_To_Lparam;

   function Message_To_Nmlvcustomdraw_Pointer is new Ada.Unchecked_Conversion(GWindows.Base.Pointer_To_Notification,
                                                                              Pointer_To_Nmlvcustomdraw_Type);
   function Message_To_Nmlistview_Pointer is new Ada.Unchecked_Conversion(GWindows.Base.Pointer_To_Notification,
                                                                          Pointer_To_Nmlistview_Type);

   function Handle_To_Wparam is new Ada.Unchecked_Conversion (GWindows.Types.Handle, GWindows.Types.Wparam);

   function To_PBuffer is new Ada.Unchecked_Conversion (LPTSTR, PBuffer);

   procedure Free_Color_Array is new Ada.Unchecked_Deallocation(Internal_Color_Array_Type,
                                                                Internal_Color_Array_Access);
   procedure Free_Internal is new Ada.Unchecked_Deallocation(Internal_Type,
                                                             Internal_Access);

   function Sendmessage (Hwnd   : GWindows.Types.Handle;
                         Umsg   : Interfaces.C.int;
                         Wparam : GWindows.Types.Wparam := 0;
                         Lparam : GWindows.Types.Lparam := 0) return GWindows.Types.Lparam;
   pragma Import (Stdcall, Sendmessage, "SendMessage" & Character_Mode_Identifier);

   procedure Sendmessage_proc (Hwnd   : GWindows.Types.Handle;
                               Umsg   : Interfaces.C.int;
                               Wparam : GWindows.Types.Wparam := 0;
                               Lparam : GWindows.Types.Lparam := 0);
   pragma Import (Stdcall, Sendmessage_proc, "SendMessage" & Character_Mode_Identifier);

   procedure Redraw_subitem (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                             Control      : in out Ex_List_View_Control_Type;
                             Return_Value : in out GWindows.Types.Lresult               );

   procedure Redraw_item (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                          Control      : in out Ex_List_View_Control_Type;
                          Return_Value : in out GWindows.Types.Lresult              );

   procedure On_Free_Payload(Control: in out Ex_List_View_Control_Type;
                             Payload: out Data_Access);

   procedure On_Header_Click (Control : in out Ex_List_View_Control_Type;
                              Column  : in     Integer                    );

   procedure Header_sorticon(Control: in out Ex_List_View_Control_Type;
                             Column: in Natural;
                             Direction: in Integer;
                             Enable: in Boolean := True);
   procedure Ownerdraw_flag(Control: in out Ex_List_View_Control_Type;
                            Column: in Natural;
                            Enable: in Boolean := True);
   procedure Draw_sorticon(Control: in out Ex_List_View_Control_Type;
                           Drawitem: in Drawitem_Type;
                           Direction: in Integer);

   -----------------------------------------------------------------------------------------
   function Get_Comctl_Version return Natural is

      function Getmodulehandle(LpModulname: in LPTSTR) return GWindows.Types.Handle;
      pragma Import(Stdcall, Getmodulehandle, "GetModuleHandle" & Character_Mode_Identifier);

      type Dllversioninfo is
         record
            Cbsize: Interfaces.C.long := 0;
            majorversion: Interfaces.C.long := 0;
            minorversion: Interfaces.C.long := 0;
            buildnumber: Interfaces.C.long := 0;
            platformid: Interfaces.C.long := 0;
         end record;
      type Dllversioninfo_Pointer is access all Dllversioninfo;

      type Dll_Get_Version_Func is access
        function (Versioninfo: in Dllversioninfo_Pointer) return Interfaces.C.unsigned_long;
      pragma Convention(C, Dll_Get_Version_Func);

      use Interfaces.C;

      function Getprocaddress(hmodule: in GWindows.Types.Handle;
                              Lpprocname: in char_array)
                             return Dll_Get_Version_Func;
      pragma Import(Stdcall, Getprocaddress, "GetProcAddress");

      libname: GString_C := GWindows.GStrings.To_GString_C("comctl32");
      Procname: aliased constant char_array := To_C("DllGetVersion");
      ModHandle: GWindows.Types.Handle;
      FuncPtr: Dll_Get_Version_Func;
      pragma Unreferenced (FuncPtr);
      Info: aliased Dllversioninfo;
      Ret_Func: Interfaces.C.unsigned_long;
      pragma Unreferenced (Ret_Func);

   begin
      --  get dll
      ModHandle := Getmodulehandle(LpModulname => libname(0)'Unchecked_Access);
      --  get dllgetversion
      FuncPtr := Getprocaddress(hmodule => ModHandle,
                                Lpprocname => Procname);
      --  call dllgetversion
      Info.Cbsize := Info'Size / 8;
      return 6;
      --  Ret_Func := FuncPtr(Info'unchecked_access) ;  --  ** This hangs GNAT GPL 2015 **
      --  Put_Line("Major version...." & Info.majorversion'Img);
      --  Put_Line("Minor version...." & Info.minorversion'Img);
      --  Put_Line("Build............" & Info.buildnumber'Img);
      --  Put_Line("Platform........." & Info.platformid'Img);
      --  return Natural(Info.MajorVersion);
   exception
      when others =>
         return 0;
   end Get_Comctl_Version;
   -----------------------------------------------------------------------------------------
   function Create_Internal(Control: in Ex_List_View_Control_Type) return Internal_Access is
      Int: Internal_Access;
      nullColors: constant Internal_Color_Type := (Textcolor => NullColor,
                                                   Backcolor => NullColor);
   begin
      Int := new Internal_Type;
      if Column_Count(Control) > 0 then
         Int.Colors := new Internal_Color_Array_Type(0..Column_Count(Control)-1);
         Int.Colors.all := (others => nullColors);
      end if;

      return Int;
   end Create_Internal;
   -----------------------------------------------------------------------------------------
   procedure On_Create(Control: in out Ex_List_View_Control_Type)is
   begin
      GWindows.Common_Controls.On_Create(Control => List_View_Control_Type(Control));
      --  pen for sort
      GWindows.Drawing_Objects.Create_Pen(Pen => Control.Sort_Object.Sort_Pen,
                                          Style => GWindows.Drawing_Objects.Solid,
                                          Width => 1,
                                          Color => Sort_Icon_Pen_Color);
      --  brush for sort
      GWindows.Drawing_Objects.Create_Solid_Brush (Brush => Control.Sort_Object.Sort_Brush,
                                                   Color => Sort_Icon_Brush_Color);

      --  comctlversion for drawing sorticons
      Control.Comctl_Version := Get_Comctl_Version;

   end On_Create;
   -----------------------------------------------------------------------------------------
   function Get_Internal (Control   : in     Ex_List_View_Control_Type;
                          Index     : in     Natural)
                         return Internal_Access is

      Item : Lvitem;
      L_Umsg: Interfaces.C.int;
   begin
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            L_Umsg := Lvm_Getitemw;
         when ANSI =>
            L_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(Hwnd   => Handle(Control),
                       Umsg   => L_Umsg,
                       Wparam => 0,
                       Lparam => Lvitem_To_Lparam(Item'Unrestricted_Access));

      return Lparam_To_Internal(Item.Lparam);
   exception
      when others =>
         return null;
   end Get_Internal;
   -----------------------------------------------------------------------------------------
   procedure Set_Internal_color (Control    : in Ex_List_View_Control_Type;
                                 Index      : in Natural;
                                 Sub_Index  : in Natural;
                                 new_colors : in Internal_Color_Type)
   is
      Item : Lvitem;
      get_Umsg: Interfaces.C.int;
      internal: Internal_Access := null;
      nullColors: constant Internal_Color_Type := (Textcolor => NullColor,
                                                   Backcolor => NullColor);
   begin
      --  get the lparam
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            get_Umsg := Lvm_Getitemw;
         when ANSI =>
            get_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => get_Umsg,
                       Wparam => 0,
                       Lparam => Lvitem_To_Lparam(Item'Unrestricted_Access));

      internal := Lparam_To_Internal(Item.Lparam);

      --  range?
      if internal.Colors.all'Last < Sub_Index then
         declare
            Tmp_Colors: constant Internal_Color_Array_Access :=
               new Internal_Color_Array_Type(0..Column_Count(Control)-1);
         begin
            Tmp_Colors.all := (others => nullColors);
            Tmp_Colors(0..internal.Colors.all'Last) := internal.Colors.all;
            --  free the old array
            Free_Color_Array(internal.Colors);
            internal.Colors := Tmp_Colors;
         end;
      end if;

      internal.Colors(Sub_Index) := new_colors;

   end Set_Internal_color;
   -----------------------------------------------------------------------------------------
   procedure Set_Internal_Payload (Control : in Ex_List_View_Control_Type;
                                   Index   : in Natural;
                                   payload : in Data_Access)
   is
      Item : Lvitem;
      get_Umsg: Interfaces.C.int;
      internal: Internal_Access := null;
   begin
      --  get the lparam
      Item.Mask := Lvif_Param;
      Item.Item := Interfaces.C.int(Index);
      Item.Subitem := 0;

      case Character_Mode is
         when Unicode =>
            get_Umsg := Lvm_Getitemw;
         when ANSI =>
            get_Umsg := Lvm_Getitema;
      end case;

      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => get_Umsg,
                       Wparam => 0,
                       Lparam => Lvitem_To_Lparam(Item'Unrestricted_Access));

      internal := Lparam_To_Internal(Item.Lparam);

      --  set payload
      internal.User_Data := payload;

   end Set_Internal_Payload;
   -----------------------------------------------------------------------------------------
   procedure On_Message(control      : in out Ex_List_View_Control_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     GWindows.Types.Wparam;
                        lParam       : in     GWindows.Types.Lparam;
                        Return_Value : in out GWindows.Types.Lresult)
   is
      use Interfaces.C;
      WM_DRAWITEM                : constant := 16#002B#;
      ODT_HEADER                 : constant := 100;
   begin
      case message is
         when WM_DRAWITEM =>
            --  ab comctl 6 direkt hdm_setitem rufen für sorticons
            declare
               Drawitem: Drawitem_Pointer := null;
            begin
               Drawitem := Lparam_To_Drawitem(lParam);
               if Drawitem /= null and then Drawitem.all.CtlType = ODT_HEADER then
                  Draw_sorticon(control, Drawitem.all, control.Sort_Object.Sort_Direction);
               end if;
               Return_Value := 1;
            end;
         when others =>
            GWindows.Common_Controls.On_Message(List_View_Control_Type(control),message,wParam, lParam, Return_Value);
      end case;

   end On_Message;
   --------------------------------------------------------------------------------------------
   procedure On_Notify (Window       : in out Ex_List_View_Control_Type;
                        Message      : in     GWindows.Base.Pointer_To_Notification;
                        Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult    ) is

      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      Nm_Customdraw   : constant := - 12;
   begin
      --  customdraw subitem
      if Message.Code = Nm_Customdraw and then
        Window.Color_Mode = Subitem
      then
         declare
            Lvcd_Ptr : constant Pointer_To_Nmlvcustomdraw_Type :=
              Message_To_Nmlvcustomdraw_Pointer (Message);
         begin
            Redraw_subitem(Lvcd_Ptr, Window, Return_Value);
         end;
         --  customdraw item
      elsif Message.Code = Nm_Customdraw and then
        Window.Color_Mode = Item_Alternately
      then
         declare
            Lvcd_Ptr : constant Pointer_To_Nmlvcustomdraw_Type :=
              Message_To_Nmlvcustomdraw_Pointer (Message);
         begin
            Redraw_item(Lvcd_Ptr, Window, Return_Value);
         end;
         --  header click
      elsif Message.Code = Nm_Header_Click then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type :=
              Message_To_Nmlistview_Pointer(Message);
         begin
            On_Header_Click (Window,
                             Integer(Nmlistview_Pointer.Isubitem));
         end;
      elsif Message.Code = LVN_INSERTITEM then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type := Message_To_Nmlistview_Pointer(Message);
            Item : Lvitem;
            L_Umsg: Interfaces.C.int;
         begin
            --  setitem
            Item.Mask := Lvif_Param;
            Item.Item := Nmlistview_Pointer.Iitem;
            Item.Lparam := Internal_To_Lparam(Create_Internal(Window));
            case Character_Mode is
               when Unicode =>
                  L_Umsg := Lvm_Setitemw;
               when ANSI =>
                  L_Umsg := Lvm_Setitema;
            end case;
            Sendmessage_proc(Hwnd => Handle(Window),
                             Umsg => L_Umsg,
                             Wparam => 0,
                             Lparam => Lvitem_To_Lparam(Item'Unrestricted_Access));
         exception
            when others =>
               null;
         end;
      elsif Message.Code = LVN_DELETEITEM then
         declare
            Nmlistview_Pointer : constant Pointer_To_Nmlistview_Type := Message_To_Nmlistview_Pointer(Message);
         begin
            if Window.Color_Mode = Item_Alternately then
               --  redraw on items
              Sendmessage_proc (Hwnd => Handle(Window),
                                Umsg => LVM_REDRAWITEMS,
                                Wparam => GWindows.Types.Wparam(Nmlistview_Pointer.Iitem),
                                Lparam => GWindows.Types.Lparam(Item_Count(Window)-1));
            end if;
         end;
      else
         GWindows.Common_Controls.On_Notify(List_View_Control_Type(Window), Message, Control, Return_Value);
      end if;

   end On_Notify;
   -----------------------------------------------------------------------------------------
   procedure Destroy_Row (control : in out Ex_List_View_Control_Type; index: Integer) is
      Int: Internal_Access := Get_Internal(control, index);
   begin
      if Int /= null then
         if Int.Colors /= null then
            Free_Color_Array(Int.Colors);
         end if;
         --  free the payload-data
         On_Free_Payload(Control => control,
                         Payload => Int.User_Data);
      end if;
      Free_Internal(Int);
   end Destroy_Row;

   procedure Destroy_All_Rows (control : in out Ex_List_View_Control_Type) is
   begin
      for Index in 0..Item_Count(control)-1 loop
         Destroy_Row (control, Index);
      end loop;
   end Destroy_All_Rows;

   procedure On_Destroy (control : in out Ex_List_View_Control_Type)is
   begin
      Destroy_All_Rows (control);
      On_Destroy(List_View_Control_Type(control));  --  Call parent method
   end On_Destroy;

   procedure Delete_Item (Control : in out Ex_List_View_Control_Type;
                          Index   : in     Integer) is
   begin
      Destroy_Row(Control, Index);
      Delete_Item (List_View_Control_Type(Control), Index);  --  Call parent method
   end Delete_Item;

   procedure Clear (Control : in out Ex_List_View_Control_Type) is
   begin
      Destroy_All_Rows (Control);
      Clear (List_View_Control_Type(Control));  --  Call parent method
    end Clear;
   -----------------------------------------------------------------------------------------
   procedure Redraw_subitem (Lvcd_Ptr     : in     Pointer_To_Nmlvcustomdraw_Type;
                             Control      : in out Ex_List_View_Control_Type;
                             Return_Value : in out GWindows.Types.Lresult              ) is
   begin
      --  set color in redraw according to color_mode
      case Lvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
            Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.long (Cdds_Itemprepaint) =>
            Return_Value := Cdrf_Notifysubitemdraw;
         when Interfaces.C.long (Cdds_Itemprepaint + Cdds_Subitem) =>
            declare
               internal: constant Internal_Access :=
                  Get_Internal(Control => Control,
                               Index   => Integer (Lvcd_Ptr.Nmcd.Dwitemspec));
               i_color : constant Integer := Integer (Lvcd_Ptr.Isubitem);
            begin
               if internal /= null and then
                 internal.Colors /= null and then
                 i_color in internal.Colors'Range and then
                 internal.Colors (i_color).Textcolor /= NullColor
               then
                  Lvcd_Ptr.Clrtext := internal.Colors (i_color).Textcolor;
               else
                  Lvcd_Ptr.Clrtext := Control.Control_Textcolor;
               end if;
               if internal /= null and then
                 internal.Colors /= null and then
                 i_color in internal.Colors'Range and then
                 internal.Colors (i_color).Backcolor /= NullColor
               then
                  Lvcd_Ptr.Clrtextbk := internal.Colors (i_color).Backcolor;
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
                          Return_Value : in out GWindows.Types.Lresult              ) is
   begin
      case Lvcd_Ptr.Nmcd.Dwdrawstage is
         when Cdds_Prepaint =>
            Return_Value := Cdrf_Notifyitemdraw;
         when Interfaces.C.long(Cdds_Itemprepaint) =>
            Return_Value := Cdrf_Notifysubitemdraw;
            Lvcd_Ptr.Clrtext := Control.Control_Textcolor;
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
      --  Call Sort, defined here or overriden.
      Sort(
        Control   => Ex_List_View_Control_Type'Class(Control),
        Column    => Column,
        Direction => Auto,
        Show_Icon => True);
   end On_Header_Click;
   ----------------------------------------------------------------------------------------------------
   procedure Draw_sorticon(Control: in out Ex_List_View_Control_Type;
                           Drawitem: in Drawitem_Type;
                           Direction: in Integer)is
      Canvas: GWindows.Drawing.Canvas_Type;
      Paint_Left: Integer;
      Max_Width: Integer;
      Columntext: constant GString := Column_Text (Control => Control, Index => Integer(Drawitem.CtlItemID));
      Columntext_Last: Natural := Columntext'Last;
      Size: GWindows.Types.Size_Type;
      Icon_Width, Icon_Height: Natural;
   begin
      GWindows.Drawing.Handle(Canvas, Drawitem.Hdc);

      --  get left for paint
      Paint_Left := Drawitem.RcItem.Left + 1;

      Icon_Height := Natural(Drawitem.RcItem.Bottom / 3);
      Icon_Width := Icon_Height;
      if Icon_Width mod 2 > 0 then
         Icon_Width := Icon_Width + 1;
      end if;

      Max_Width := Drawitem.RcItem.Right - Drawitem.RcItem.Left - (4 * Icon_Width);

      if Max_Width < 0 then
         return;
      end if;

      --  check string
      while Columntext_Last > 0 loop
         if Columntext_Last < Columntext'Last then
            Size := GWindows.Drawing.Text_Output_Size (Canvas => Canvas,
                                                       Text => Columntext(1..Columntext_Last) & "...");
         else
            Size := GWindows.Drawing.Text_Output_Size (Canvas => Canvas,
                                                       Text => Columntext);
         end if;
         if Size.Width <= Max_Width then
            exit;
         end if;
         Columntext_Last := Columntext_Last - 1;
      end loop;

      --  put the string
      if Columntext_Last = Columntext'Last then
         GWindows.Drawing.Put(Canvas => Canvas,
                              X => Paint_Left + (3 * Icon_Width),
                              Y => 2,
                              Text => Columntext);
      elsif Columntext_Last > 0 then
         GWindows.Drawing.Put(Canvas => Canvas,
                              X => Paint_Left + (3 * Icon_Width),
                              Y => 2,
                              Text => Columntext(1..Columntext_Last) & "...");
      end if;

      --  paint the polygon
      GWindows.Drawing.Select_Object(Canvas => Canvas, Object => Control.Sort_Object.Sort_Pen);
      GWindows.Drawing.Select_Object(Canvas => Canvas, Object => Control.Sort_Object.Sort_Brush);

      declare
         Pt_Array  : GWindows.Types.Point_Array_Type(1..3);
         Pt_Top    : constant Natural := Icon_Height;
         Pt_Bottom : constant Natural := Drawitem.RcItem.Bottom - Icon_Height -1;
         Pt_Left   : constant Natural := Icon_Width;
         Pt_Right  : constant Natural := 2 * Icon_Width;
      begin
         if Icon_Height > 5 then
            Icon_Height := Icon_Height -1;
         end if;
         --  up
         if Direction = 1 then
            Pt_Array(1).X := Pt_Left + Paint_Left + Natural(Icon_Width / 2); Pt_Array(1).Y := Pt_Top; ---1;
            Pt_Array(2).X := Pt_Right + Paint_Left; Pt_Array(2).Y := Pt_Bottom; ---1;
            Pt_Array(3).X := Pt_Left + Paint_Left; Pt_Array(3).Y := Pt_Bottom; ---1;
         else
            --  down
            Pt_Array(1).X := Pt_Left + Paint_Left; Pt_Array(1).Y := Pt_Top;
            Pt_Array(2).X := Pt_Right + Paint_Left; Pt_Array(2).Y := Pt_Top;
            Pt_Array(3).X := Pt_Left + Paint_Left + Natural(Icon_Width / 2); Pt_Array(3).Y := Pt_Bottom;
         end if;

         GWindows.Drawing.Polygon (Canvas => Canvas, Vertices => Pt_Array);
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
   --  ** Column_Text function added to GWindows.Common_Controls, August 2018.
   --
   --  function Column_text(Control: in Ex_List_View_Control_Type;
   --                       Column: in Natural) return GString is
   --     C_Text : Buffer;
   --     Lv: Lvcolumn_type;
   --     Get_Umsg: Interfaces.C.int;
   --  begin
   --     case Character_Mode is
   --        when Unicode =>
   --           Get_Umsg := LVM_GETCOLUMNW;
   --        when ANSI =>
   --           Get_Umsg := LVM_GETCOLUMNA;
   --     end case;
   --
   --     Lv.Mask := LVCF_TEXT;
   --     Lv.Text := C_Text (0)'Unchecked_Access;
   --     Lv.Textmax := 255;
   --
   --     -- get the lvcolumn
   --     Sendmessage_proc(Hwnd => Handle(Control),
   --                      Umsg => Get_Umsg,
   --                      Wparam => GWindows.Types.To_Wparam(Column),
   --                      Lparam => Lvcolumn_To_Lparam(Lv'Unrestricted_Access));
   --
   --     return GWindows.GStrings.To_GString_From_C(GString_C (To_PBuffer (Lv.Text).all));
   --
   --  end Column_text;
   ----------------------------------------------------------------------------------------------------
   procedure Header_sorticon(Control: in out Ex_List_View_Control_Type;
                             Column: in Natural;
                             Direction: in Integer;
                             Enable: in Boolean := True)is
      use Interfaces.C;
      use GWindows.GStrings;
      Header: GWindows.Types.Handle;
      Hd: aliased Hditem_type;
      C_Text: Buffer;
      L_setUmsg: Interfaces.C.int;
      L_getUmsg: Interfaces.C.int;
   begin
      case Character_Mode is
         when Unicode =>
            L_setUmsg := HDM_SETITEMW; L_getUmsg := HDM_GETITEMW;
         when ANSI =>
            L_setUmsg := HDM_SETITEMA; L_getUmsg := HDM_GETITEMA;
      end case;

      --  get the header
      Header := GWindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => LVM_GETHEADER,
                                                     Wparam => 0,
                                                     Lparam => 0));

      --  get the item
      Hd.Mask := HDI_FORMAT + HDI_TEXT;
      Hd.pszText := C_Text (0)'Unchecked_Access;
      Hd.CchTextMax := 255;
      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_getUmsg,
                       Wparam => GWindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(Hd'Unchecked_Access));

      --  update item
      Hd.Mask := HDI_FORMAT + HDI_TEXT;
      --  remove the old icon flag
      if (unsigned(Hd.Fmt) and unsigned(HDF_SORTUP)) > 0 then
         Hd.Fmt := Hd.Fmt - HDF_SORTUP;
      end if;
      if (unsigned(Hd.Fmt) and unsigned(HDF_SORTDOWN)) > 0 then
         Hd.Fmt := Hd.Fmt - HDF_SORTDOWN;
      end if;

      --  set the new icon flag
      if Enable then
         if Direction = 1 then -- up
            Hd.Fmt := Hd.Fmt + HDF_SORTUP;
         elsif Direction = -1 then -- down
            Hd.Fmt := Hd.Fmt + HDF_SORTDOWN;
         end if;
      end if;

      --  text
      Hd.CchTextMax := int(To_GString_From_C(GString_C(To_PBuffer(Hd.pszText).all))'Last + 1);
      --  set the new item
      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_setUmsg,
                       Wparam => GWindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(Hd'Unchecked_Access));

   end Header_sorticon;

   procedure Ownerdraw_flag(Control: in out Ex_List_View_Control_Type;
                            Column: in Natural;
                            Enable: in Boolean := True)is
      use Interfaces.C;
      Header: GWindows.Types.Handle;
      Hd: aliased Hditem_type;
      L_Umsg: Interfaces.C.int;
   begin
      --  get the header
      Header := GWindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => LVM_GETHEADER,
                                                     Wparam => 0,
                                                     Lparam => 0));
      Hd.Mask := HDI_FORMAT;
      Hd.Fmt := HDF_string;
      if Enable then
         Hd.Fmt := Hd.Fmt + HDF_OWNERDRAW;
      end if;

      case Character_Mode is
         when Unicode =>
            L_Umsg := HDM_SETITEMW;
         when ANSI =>
            L_Umsg := HDM_SETITEMA;
      end case;

      Sendmessage_proc(Hwnd => Header,
                       Umsg => L_Umsg,
                       Wparam => GWindows.Types.To_Wparam(Column),
                       Lparam => Hditem_To_Lparam(Hd'Unchecked_Access));

   end Ownerdraw_flag;
   ----------------------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------------------
   function Column_Count(Control    : in Ex_List_View_Control_Type) return Natural is
      use GWindows.Types;
      Header: GWindows.Types.Handle;
      Count: GWindows.Types.Lparam;
   begin
      Header := GWindows.Types.To_Handle(Sendmessage(Hwnd => Handle(Control),
                                                     Umsg => LVM_GETHEADER));

      Count := Sendmessage(Hwnd => Header,
                           Umsg => HDM_GETITEMCOUNT);
      if To_Integer(Lresult(Count)) < 0 then
         Raise_Exception(Elv_Exception'Identity, "Error on HDM_GETITEMCOUNT.");
      end if;

      return Natural(Count);
   end Column_Count;
   ----------------------------------------------------------------------------------------------------
   procedure Autosize(Control: in out Ex_List_View_Control_Type;
                      Column : in Natural;
                      Sizing: in Autosize_Type := Headersize)is

      LVSCW_AUTOSIZE            : constant := -1;
      LVSCW_AUTOSIZE_USEHEADER  : constant := -2;
   begin
      case Sizing is
         when Columnsize =>
            Sendmessage_proc (Hwnd => Handle (Control),
                              Umsg => LVM_SETCOLUMNWIDTH,
                              Wparam => GWindows.Types.To_Wparam(Column),
                              Lparam  => GWindows.Types.To_Lparam(LVSCW_AUTOSIZE));
         when Headersize =>
            if Control.Sort_Object.Sort_Column = Column and then
              Control.Sort_Object.Icon_Visible
            then
               --  width manually calculate
               declare
                  Canvas: GWindows.Drawing.Canvas_Type;
                  Font: GWindows.Drawing_Objects.Font_Type;
                  Size: GWindows.Types.Size_Type;
                  Offset: Natural;
               begin
                  Get_Canvas(Control, Canvas);
                  Get_Font(Control, Font);
                  GWindows.Drawing.Select_Object(Canvas, Font);
                  Size := GWindows.Drawing.Text_Output_Size (Canvas, Column_Text (Control, Column));
                  Offset := (Size.Height + 4) / 3;
                  if Offset mod 2 > 0 then
                     Offset := Offset + 1;
                  end if;
                  Offset := Offset * 4;
                  Sendmessage_proc (Hwnd => Handle (Control),
                                    Umsg => LVM_SETCOLUMNWIDTH,
                                    Wparam => GWindows.Types.To_Wparam(Column),
                                    Lparam  => GWindows.Types.To_Lparam(Size.Width + Offset));
               end;
            else
               Sendmessage_proc (Hwnd => Handle (Control),
                                 Umsg => LVM_SETCOLUMNWIDTH,
                                 Wparam => GWindows.Types.To_Wparam(Column),
                                 Lparam  => GWindows.Types.To_Lparam(LVSCW_AUTOSIZE_USEHEADER));
            end if;
      end case;
   end Autosize;
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
                       Umsg => LVM_SETTEXTBKCOLOR,
                       Wparam => 0,
                       Lparam => Color_To_Lparam(Color));
      Control.Color_Mode := All_Items;
      Control.Control_Backcolor := Color;
   end Back_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Control_Back_Color (Control : in  out  Ex_List_View_Control_Type;
                                 Color   : in     Color_Type                 ) is
   begin
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => LVM_SETBKCOLOR,
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
   procedure Subitem_Color (Control    : in out Ex_List_View_Control_Type;
                            Text_Color : in     Color_Type := Black;
                            Back_Color : in     Color_Type := White;
                            Index      : in     Integer := -1;
                            Sub_Index  : in     Integer := -1            )is

      new_colors: Internal_Color_Type;
   begin
      if Column_Count(Control) = 0 then
         Ada.Exceptions.Raise_Exception(Elv_Exception'Identity, "No columns!");
      end if;

      if Index < 0 and Sub_Index < 0 then
         Ada.Exceptions.Raise_Exception(Elv_Exception'Identity, "No index/subindex!");
      end if;

      new_colors.Textcolor := Text_Color;
      new_colors.Backcolor := Back_Color;

      if Index >= 0 and Sub_Index >= 0 then -- subitem
         Set_Internal_color (Control => Control,
                             Index   => Index,
                             Sub_Index => Sub_Index,
                             new_colors => new_colors);
      elsif Index >= 0 then -- row
         for P_Sub_Index in 0..Column_Count(Control)-1 loop
            Set_Internal_color (Control => Control,
                                Index   => Index,
                                Sub_Index => P_Sub_Index,
                                new_colors => new_colors);
         end loop;
      elsif Sub_Index >= 0 then -- column
         for P_Index in 0..Item_Count(Control)-1 loop
            Set_Internal_color (Control => Control,
                                Index   => P_Index,
                                Sub_Index => Sub_Index,
                                new_colors => new_colors);
         end loop;
      end if;
      Control.Color_Mode := Subitem;
   end Subitem_Color;
   ----------------------------------------------------------------------------------------------------
   procedure Color_Mode(Control : in out Ex_List_View_Control_Type;
                        Mode: in Color_Mode_Type;
                        Redraw: in Boolean := True)is
   begin
      Control.Color_Mode := Mode;
      if Redraw then
         GWindows.Common_Controls.Ex_List_View.Redraw(Control);
      end if;
   end Color_Mode;
   ----------------------------------------------------------------------------------------------------
   procedure Item_Data(Control : in Ex_List_View_Control_Type;
                       Index: in Natural;
                       Payload: in Data_Access)is

   begin
      Set_Internal_Payload(Control, Index, Payload);
   end Item_Data;
   ----------------------------------------------------------------------------------------------------
   function Item_Data(Control : in Ex_List_View_Control_Type;
                      Index: in Natural) return Data_Access is

   begin
      return Get_Internal(Control, Index).User_Data;
   end Item_Data;
   ----------------------------------------------------------------------------------------------------
   procedure On_Free_Payload(Control: in out Ex_List_View_Control_Type;
                             Payload: out Data_Access)is
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
   function On_Compare (
               Control : in Ex_List_View_Control_Type;
               Column  : in Natural;
               Index_1 : in Natural;
               Index_2 : in Natural) return Integer
   is
   begin
     return Fire_On_Compare (Control, Column, Index_1, Index_2);
   end On_Compare;
   --
   procedure On_Compare_Handler(Control       : in out Ex_List_View_Control_Type;
                                 General_Event : in General_Compare_Event)
   is
   begin
      Control.On_General_Compare_Event := General_Event;
   end On_Compare_Handler;
   --
   function Fire_On_Compare (
               Control : in Ex_List_View_Control_Type;
               Column  : in Natural;
               Index_1 : in Natural;
               Index_2 : in Natural) return Integer
   is
   begin
      if Control.On_General_Compare_Event /= null then
         return Control.On_General_Compare_Event (
            Control, Column, Index_1, Index_2
         );
      end if;
      --  Default behaviour: alphabetic.
      declare
         Value_1 : constant GString := Text (Control => Control,
                                             Item    => Index_1,
                                             SubItem => Column);
         Value_2 : constant GString := Text (Control => Control,
                                             Item    => Index_2,
                                             SubItem => Column);
      begin
         if Value_1 = Value_2 then
            return 0;
         elsif Value_1 > Value_2 then
            return 1;
         else
            return -1;
         end if;
      end;
   end Fire_On_Compare;
   ----------------------------------------------------------------------------------------------------
   procedure Sort (Control    : in out Ex_List_View_Control_Type;
                   Column     : in Natural;
                   Direction  : in Sort_Direction_Type;
                   Show_Icon  : in Boolean := True;
                   Technique  : in Comparison_Technique_Type := As_Strings
                  )
   is
      function On_compare_internal
        (Lparam1    : in     GWindows.Types.Lparam;
         Lparam2    : in     GWindows.Types.Lparam;
         Lparamsort : in     GWindows.Types.Handle)
      return Interfaces.C.int;
      pragma Convention (Stdcall, On_compare_internal);
      function On_compare_internal
        (Lparam1    : in     GWindows.Types.Lparam;
         Lparam2    : in     GWindows.Types.Lparam;
         Lparamsort : in     GWindows.Types.Handle)
      return Interfaces.C.int
      is
         use GWindows.Types;
         Findinfo  : Findinfo_Type;
         Index1, Index2: Natural;
         L_Umsg: Interfaces.C.int;
      begin
         --  Get the index from lparam
         case Character_Mode is
            when Unicode =>
               L_Umsg := Lvm_Finditemw;
            when ANSI =>
               L_Umsg := Lvm_Finditema;
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
         case Technique is
            when As_Strings =>
               --  values
               declare
                  Value1: constant GString := Text(Control => Control,
                                                   Item => Index1,
                                                   SubItem => Control.Sort_Object.Sort_Column);
                  Value2: constant GString := Text(Control => Control,
                                                   Item => Index2,
                                                   SubItem => Control.Sort_Object.Sort_Column);
               begin
                  --  We call the method, which is either overriden, or calls
                  --  Fire_On_Compare which in turn calls the handler, if available, or
                  --  applies a default alphabetical sorting.
                  return Interfaces.C.int (
                     On_Compare (
                        Control => Ex_List_View_Control_Type'Class(Control),
                        Column  => Control.Sort_Object.Sort_Column,
                        Value1  => Value1,
                        Value2  => Value2)
                     * Control.Sort_Object.Sort_Direction
                   );
               end;
            when General =>
               return Interfaces.C.int (
                  On_Compare (
                     Control  => Ex_List_View_Control_Type'Class(Control),
                     Column   => Control.Sort_Object.Sort_Column,
                     Index_1  => Index1,
                     Index_2  => Index2)
                  * Control.Sort_Object.Sort_Direction
                );
         end case;
      exception
         when E:others =>
            Raise_Exception(Elv_Exception'Identity, "error on on_compare: " & Exception_Information(E));
      end On_compare_internal;
   --
   begin
      --  same column, reverse sort direction
      case Direction is
         when Auto =>
            if Control.Sort_Object.Sort_Column = Column then
               Control.Sort_Object.Sort_Direction := Control.Sort_Object.Sort_Direction * (-1);
            else
               if Control.Sort_Object.Sort_Column >= 0 then
                  --  reset the Icon
                  if Control.Comctl_Version <= 5 then
                     Ownerdraw_flag(Control, Control.Sort_Object.Sort_Column, False);
                  else
                     Header_sorticon(Control, Control.Sort_Object.Sort_Column, 0, False);
                  end if;
               end if;

               --  new column, start with sorting up
               Control.Sort_Object.Sort_Column := Column;
               Control.Sort_Object.Sort_Direction := 1;
            end if;
         when Up =>
            Control.Sort_Object.Sort_Column := Column;
            Control.Sort_Object.Sort_Direction := 1;
         when Down =>
            Control.Sort_Object.Sort_Column := Column;
            Control.Sort_Object.Sort_Direction := -1;
      end case;

      --  start sorting
      Sendmessage_proc(Hwnd => Handle(Control),
                       Umsg => Lvm_Sortitems,
                       Wparam => Handle_To_Wparam(Handle(Control)),
                       Lparam => Address_To_Lparam(On_compare_internal'Address));

      Control.Sort_Object.Icon_Visible := Show_Icon;

      --  draw the sort icon
      if Show_Icon then
         if Control.Comctl_Version <= 5 then
            Ownerdraw_flag(Control, Column, True);
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
