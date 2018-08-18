with GWindows.Types;                    use GWindows.Types;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Image_Lists;              use GWindows.Image_Lists;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.GStrings;                 use GWindows.GStrings;

with Interfaces.C;
with GNATOCX;
--  with GNATOCX.IDataObject_Interface;
with GNATCOM.Types;
with GNATCOM;

package body Drag_Drop_OLE_Window is

   --  Ingredients (randomly picked from MSDN, examples, etc. will be in
   --  a specific package)

   --  CF_HDROP

   --  DataObject := [ideally: empty list!]
   --  GetFileListDataObject(DirectoryListBox1.Directory, SelFileList);

   --  Effect := DROPEFFECT_NONE;
   --  DoDragDrop(DataObject, Self, DROPEFFECT_COPY, Effect);

   --  MSDN: "You also need to call the DoDragDrop, RegisterDragDrop,
   --  and RevokeDragDrop functions in drag-and-drop operations."
   --
   --  RegisterDragDrop: Registers the specified window as one that
   --  can be the *target* of an OLE drag-and-drop operation
   --
   --  HRESULT RegisterDragDrop(
   --  _In_  HWND hwnd,
   --  _In_  LPDROPTARGET pDropTarget
   --  );
   --  HRESULT RevokeDragDrop(
   --  _In_  HWND hwnd
   --  );

   subtype LPVOID is GWindows.Types.Handle;
   function OleInitialize (pvReserved : LPVOID := Null_Handle)
   return GNATCOM.Types.HRESULT;
   pragma Import (Stdcall, OleInitialize, "OleInitialize");

   --  We mimick IDataObject for IDropSource ...

   type IDropSourceVtbl;
   type Pointer_To_IDropSourceVtbl is access all IDropSourceVtbl;

   type IDropSource is
      record
         Vtbl : Pointer_To_IDropSourceVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IDropSource);

   type af_IDropSource_GiveFeedback is access
     function (Effect : GNATCOM.Types.DWORD)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDropSource_GiveFeedback);

   type af_IDropSource_QueryContinueDrag is access
     function (
        EscapePressed : GNATCOM.Types.bool;
        grfKeyState   : GNATCOM.Types.DWORD)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDropSource_QueryContinueDrag);

   type IDropSourceVtbl is
      record
         GiveFeedback        : af_IDropSource_GiveFeedback;
         QueryContinueDrag   : af_IDropSource_QueryContinueDrag;
      end record;
   pragma Convention (C_Pass_By_Copy, IDropSourceVtbl);

   type Pointer_To_IDropSource is access all IDropSource;

   function DoDragDrop
         (pDataObj    : GNATOCX.Pointer_To_IDataObject;
          pDropSource : Pointer_To_IDropSource;
          OKEffects   : GNATCOM.Types.DWORD;
          Effect      : GNATCOM.Types.Pointer_To_DWORD
         )
   return GNATCOM.Types.HRESULT;
   pragma Import (Stdcall, DoDragDrop, "DoDragDrop");

   --  DROPEFFECT_NONE   : constant := 0;
   DROPEFFECT_COPY   : constant := 1;
   --  DROPEFFECT_MOVE   : constant := 2;
   --  DROPEFFECT_LINK   : constant := 4;
   --  DROPEFFECT_SCROLL : constant := 16#8000_0000#;

   DRAGDROP_S_DROP   : constant := 16#00040100#;
   DRAGDROP_S_CANCEL : constant := 16#00040101#;

   function Drop_to_Explorer return GString is
   --
   --  Captures the Drag & Drop operations initiated by LVN_BEGINDRAG
   --  and gives back the path of an Explorer window or of the desktop
   --  if the dropping happened on thoses areas. An empty string is
   --  returned in any other case.
      res : GNATCOM.Types.HRESULT;
      DataObjVtbl : aliased GNATOCX.IDataObjectVtbl :=
      (
         QueryInterface        => null,  -- !!
         AddRef                => null,  -- !!
         Release               => null,  -- !!
         RemoteGetData         => null,  -- !!
         RemoteGetDataHere     => null,  -- !!
         QueryGetData          => null,  -- !!
         GetCanonicalFormatEtc => null,  -- !!
         RemoteSetData         => null,  -- !!
         EnumFormatEtc         => null,  -- !!
         DAdvise               => null,  -- !!
         DUnadvise             => null,  -- !!
         EnumDAdvise           => null   -- !!
      );

      DataObj     : aliased GNATOCX.IDataObject :=
                       (Vtbl => DataObjVtbl'Unchecked_Access);
      pDataObj    : GNATOCX.Pointer_To_IDataObject := DataObj'Unchecked_Access;
      --  DropSource  : aliased IDropSource := (others => null);
      pDropSource : Pointer_To_IDropSource := null;
      --  !! DropSource'Unchecked_Access;
      OKEffects   : GNATCOM.Types.DWORD := DROPEFFECT_COPY;
      Effect      : aliased GNATCOM.Types.DWORD;
      p_Effect    : GNATCOM.Types.Pointer_To_DWORD := Effect'Unchecked_Access;
   begin
      --  GNATOCX.IDataObject_Interface.Initialize (DataObj);
      res := DoDragDrop (pDataObj, pDropSource, OKEffects, p_Effect);
      case res is
         when DRAGDROP_S_DROP =>
            return "Success"; -- !!
         when DRAGDROP_S_CANCEL =>
            return "Cancelled"; -- !!
         when others =>
            return "Drop error:" & GNATCOM.Types.HRESULT'Wide_Image (res);
            --  !! raise exception here
      end case;
   end Drop_to_Explorer;

   -------------------------------------------
   --  List View with some Drag capability  --
   -------------------------------------------

   procedure Start_Drag (Window : in out LV_with_Drag)
   is
      --  Font, Mem_Font : Font_Type;
      --  Font is needed to create drag image, otherwise drag image
      --  is invisible - Windows bug since Vista!
      --  Cursor : Cursor_Type := Load_System_Cursor (IDC_HAND);
      --  Cursor_Pos : Point_Type := Get_Cursor_Position;
      --  type Point_Access is access all Point_Type;
      --  LVM_FIRST                    : constant := 16#1000#;
      --  LVM_CREATEDRAGIMAGE          : constant := LVM_FIRST + 33;

      --  Image_List_Handle : GWindows.Types.Handle;
      --  Drag_Image_List : Image_List_Type;
      Point : aliased GWindows.Types.Point_Type := Get_Cursor_Position;

      Clicked_item, Clicked_subitem : Integer := 0;

--        function Sendmessage_list
--           (Hwnd   : GWindows.Types.Handle;
--            Umsg   : Interfaces.C.int  := LVM_CREATEDRAGIMAGE;
--            Wparam : Integer;     --  The index of the item
--            Lparam : Point_Access
--           )
--        return GWindows.Types.Handle;
--        pragma Import (Stdcall, Sendmessage_list,
--                         "SendMessage" & Character_Mode_Identifier);
   begin
      Point := Get_Cursor_Position;
      Point := Point_To_Client (Window, Point);
      Item_At_Position (Window, Point, Clicked_item, Clicked_subitem);
      Text (My_Window_Type (Parent (Window).all).Status,
            "Drag starting from List View, Item Nr" &
            Image (Clicked_item + 1) &
            " and perhaps some more items...");
      --  Capture_Mouse (My_Window_Type (Parent (Window).all));
      --  ^ This is needed, otherwise dropping outside of parent window
      --    is not captured via On_Left_Mouse_Button_Up.
      --  if Cursor /= 0 then
      --    Set_Cursor (Cursor);
      --  end if;
      --  Image associated with dragging
      --  Unselect clicked item
--        Selected (Window, Clicked_item, False);
--        --  Set a provisory font
--        Get_Font (Window, Mem_Font);
--        Create_Stock_Font (Font, System);
--        Set_Font (Window, Font);
--        Point := (0, 0);
--        Image_List_Handle := Sendmessage_list (Hwnd => Handle (Window),
--                             Wparam => Clicked_item,
--                             Lparam => Point'Access
--        );
--        Selected (Window, Clicked_item, True);
--        Set_Font (Window, Mem_Font);
--        Handle (Drag_Image_List, Image_List_Handle);
--        Begin_Drag (Drag_Image_List, 0, -10, 0);
--        --  First position for dragging image
--        Drag_Enter (Window, Point.X, Point.Y);
      Window.Drop_target_path := To_GString_Unbounded (Drop_to_Explorer);
      Text (My_Window_Type (Parent (Window).all).Status,
            To_GString_From_Unbounded (Window.Drop_target_path));
   end Start_Drag;

   procedure On_Notify (
      Window       : in out LV_with_Drag;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
   )
   is
      LVN_FIRST      : constant := -100;
      LVN_BEGINDRAG  : constant := LVN_FIRST - 9;
   begin
      --  Call parent method
      On_Notify (
         List_View_Control_Type (Window),
         Message, Control, Return_Value);
      case Message.Code is
         when LVN_BEGINDRAG =>
            Start_Drag (Window);
         when others =>
            null;
      end case;
   end On_Notify;

   -------------------------------------------
   --  Tree View with some Drag capability  --
   -------------------------------------------

   procedure Start_Drag (Window : in out TV_with_Drag)
   is
      Font, Mem_Font : Font_Type;
      --  Font is needed to create drag image, otherwise drag image
      --  is invisible - Windows bug since Vista!
      IDC_HAND : constant := 32649;
      Cursor : Cursor_Type := Load_System_Cursor (IDC_HAND);
      --  Cursor_Pos : Point_Type := Get_Cursor_Position;
      --  type Point_Access is access all Point_Type;
      TVM_FIRST           : constant := 16#1100#;
      TVM_CREATEDRAGIMAGE : constant := TVM_FIRST + 18;
      function Sendmessage_tree (
         Hwnd   : GWindows.Types.Handle;
         Umsg   : Interfaces.C.int  := TVM_CREATEDRAGIMAGE;
         Wparam : Interfaces.C.int := 0;
         Lparam : Tree_Item_Node)
      return GWindows.Types.Handle;
      pragma Import (Stdcall, Sendmessage_tree,
                       "SendMessage" & Character_Mode_Identifier);

      Image_List_Handle : GWindows.Types.Handle;
      Drag_Image_List : Image_List_Type;
      Point : aliased GWindows.Types.Point_Type := Get_Cursor_Position;

      Clicked_node : Tree_Item_Node;
      Selected_node : Tree_Item_Node;

   begin
      Point := Get_Cursor_Position;
      Point := Point_To_Client (Window, Point);
      Clicked_node := Item_At_Position (Window, Point);
      Selected_node := Selected_Item (Window);
      Text (My_Window_Type (Parent (Window).all).Status,
            "Drag starting from Tree View, Node " &
            Text (Window, Clicked_node));
      Capture_Mouse (My_Window_Type (Parent (Window).all));
      --  ^ This is needed, otherwise dropping outside of parent window
      --    is not captured via On_Left_Mouse_Button_Up.
      if Cursor /= 0 then
        Set_Cursor (Cursor);
      end if;
      --  Image associated with dragging
      --  Unselect clicked item
      if Selected_node = Clicked_node then
        Select_Item (Window, 0);
      end if;
      --  Set a provisory font
      Get_Font (Window, Mem_Font);
      Create_Stock_Font (Font, System);
      Set_Font (Window, Font);
      Point := (0, 0);
      Image_List_Handle := Sendmessage_tree (
        Hwnd => Handle (Window),
        Lparam => Clicked_node
      );
      if Selected_node = Clicked_node then
        Select_Item (Window, Selected_node);
      end if;
      Set_Font (Window, Mem_Font);
      Handle (Drag_Image_List, Image_List_Handle);
      Begin_Drag (Drag_Image_List, 0, -10, 0);
      --  First position for dragging image
      Drag_Enter (Window, Point.X, Point.Y);
      Window.Drop_target_path := To_GString_Unbounded (Drop_to_Explorer);
   end Start_Drag;

   procedure On_Notify (
      Window       : in out TV_with_Drag;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
   )
   is
      TVN_FIRST      : constant := -400;
      TVN_BEGINDRAGA : constant := TVN_FIRST - 7;
      TVN_BEGINDRAGW : constant := TVN_FIRST - 56;
   begin
      --  Call parent method
      On_Notify (
         Tree_View_Control_Type (Window),
         Message, Control, Return_Value);
      case Message.Code is
         when TVN_BEGINDRAGA | TVN_BEGINDRAGW =>
            Start_Drag (Window);
         when others =>
            null;
      end case;
   end On_Notify;

   -------------------
   --  Demo window  --
   -------------------

   GUI_Font : Font_Type;

   procedure On_Create (Window : in out My_Window_Type)
   is

      procedure Dlg_to_Scn (
        --  converts dialog coords to screen (pixel) coords.
        xd, yd, wd, hd :  in Integer;
        xs, ys, ws, hs : out Integer)
      is
        baseunitX : constant := 6;
        baseunitY : constant := 13;
      begin
        xs := (xd * baseunitX) / 4;
        ws := (wd * baseunitX) / 4;
        ys := (yd * baseunitY) / 8;
        hs := (hd * baseunitY) / 8;
      end Dlg_to_Scn;

      x, y, w, h : Integer;
   begin
      Set_Font (Window, GUI_Font);
      Dlg_to_Scn (0, 0, 267, 223, x, y, w, h);
      Move (Window, x, y);
      Client_Area_Size (Window, w, h);
      Dlg_to_Scn (120, 29, 138, 110, x, y, w, h);
      Create (Window.Some_list, Window, x, y, w, h, Multiple,
         List_View, No_Sorting, False, Align_Left);
      Dlg_to_Scn (9, 29, 102, 110, x, y, w, h);
      Create (Window.Some_tree, Window, x, y, w, h);
      Dlg_to_Scn (10, 166, 247, 37, x, y, w, h);
      Create (Window.Some_edit_box, Window, "", x, y, w, h);
      Create (Window.Status, Window, "No drag so far");
      Dlg_to_Scn (10, 10, 109, 8, x, y, w, h);
      Create_Label (Window, "Drag tree node; drop area, too:", x, y, w, h);
      Dlg_to_Scn (9, 147, 100, 8, x, y, w, h);
      Create_Label (Window, "Drop area:", x, y, w, h);
      Dlg_to_Scn (120, 10, 109, 8, x, y, w, h);
      Create_Label (Window, "Drag list items; drop area, too:", x, y, w, h);
   end On_Create;

   procedure On_Left_Mouse_Button_Up (Window : in out My_Window_Type;
                                      X      : in     Integer;
                                      Y      : in     Integer;
                                      Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (X, Y, Keys);
      --  L_Rect : GWindows.Types.Rectangle_Type := (others => 0);
      --  L_Point : GWindows.Types.Point_Type;
      --  Node : Tree_Item_Node;
      --  Flags: Hittest_Flag_Type;
      --  Target_In_Tree: Boolean;
   begin
     --         GWindows.Image_Lists.Drag_Leave(main);
     --         GWindows.Image_Lists.End_Drag;
     Text (Window.Status, "Drag stopped (button up)");
     Release_Mouse;
   end On_Left_Mouse_Button_Up;

begin
   if OleInitialize not in GNATCOM.S_OK .. GNATCOM.S_FALSE then
      Message_Box ("", "Error with OleInitialize");
      --  !! raise exception here instead of msgbox
   end if;
   Create_Stock_Font (GUI_Font, Default_GUI);
end Drag_Drop_OLE_Window;
