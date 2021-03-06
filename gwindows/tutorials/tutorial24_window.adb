with GWindows.Types;                    use GWindows.Types;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.GStrings;                 use GWindows.GStrings;

with Interfaces.C;

package body Tutorial24_Window is

   -------------------------------------------
   --  List View with some Drag capability  --
   -------------------------------------------

   procedure Start_Drag_LV (Window : in out LV_with_Drag)
   is
      --  * Font, Mem_Font : Font_Type;
      --  * --  Font is needed to create drag image, otherwise drag image
      --  * --  is invisible - Windows bug since Vista!
      Cursor : Cursor_Type := Load_System_Cursor (IDC_HAND);
      --  Cursor_Pos : Point_Type := Get_Cursor_Position;
      use GWindows;
      type Point_Access is access all Point_Type;
      LVM_FIRST                    : constant := 16#1000#;
      LVM_CREATEDRAGIMAGE          : constant := LVM_FIRST + 33;

      Image_List_Handle : GWindows.Types.Handle;
      Point : aliased GWindows.Types.Point_Type := Get_Cursor_Position;

      Clicked_item, Clicked_subitem : Integer := 0;

      Main : My_Window_Type renames My_Window_Type (Parent (Window).all);

      function Sendmessage_list
         (Hwnd   : GWindows.Types.Handle;
          Umsg   : Interfaces.C.int  := LVM_CREATEDRAGIMAGE;
          Wparam : Integer;     --  The index of the item
          Lparam : Point_Access --  Initial location of the upper-left corner
                                --  of the image, in view coordinates.
         )
      return GWindows.Types.Handle;
      pragma Import (Stdcall, Sendmessage_list,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Point := Get_Cursor_Position;
      Point := Point_To_Client (Window, Point);
      Item_At_Position (Window, Point, Clicked_item, Clicked_subitem);
      if Clicked_item < 0 then
        Text (Main.Status,
            "Drag attempted from List View, but ivalid item (index" &
            Image (Clicked_item) &
            ") - aborting...");
        return;
      end if;
      Text (Main.Status,
            "Drag starting from List View, Item Nr" &
            Image (Clicked_item + 1) &
            " and perhaps some more items...");
      Capture_Mouse (Main);
      if Cursor /= 0 then
        Set_Cursor (Cursor);
      end if;
      --  Image associated with dragging
      --  % --  Unselect clicked item:
      --  % Selected (Window, Clicked_item, False);
      --  % Window.Redraw;
      --  * --  Set a provisory font
      --  * Get_Font (Window, Mem_Font);
      --  * Create_Stock_Font (Font, System);
      --  * Set_Font (Window, Font);
      Point := (0, 0);
      --  Window.Set_Image_List (Normal, Main.Drag_Image_List);  --  Needed ?
      Image_List_Handle := Sendmessage_list (Hwnd => Handle (Window),
                           Wparam => Clicked_item,
                           Lparam => Point'Access
      );
      --  % --  Re-select clicked item:
      --  % Selected (Window, Clicked_item, True);
      --  % Window.Redraw;
      --  * Set_Font (Window, Mem_Font);
      Handle (Main.Drag_Image_List, Image_List_Handle);
      Main.Drag_Image_List.Begin_Drag (Index => 0, X => 10, Y => -20);
      --  First position for dragging image
      Drag_Enter (Main, Point.X, Point.Y);
      Main.Dragging := True;
   end Start_Drag_LV;

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
            Start_Drag_LV (Window);
         when others =>
            null;
      end case;
   end On_Notify;

   -------------------------------------------
   --  Tree View with some Drag capability  --
   -------------------------------------------

   procedure Start_Drag_TV (Window : in out TV_with_Drag)
   is
      --  * Font, Mem_Font : Font_Type;
      --  * --  Font is needed to create drag image, otherwise drag image
      --  * --  is invisible - Windows bug since Vista!
      IDC_HAND : constant := 32649;
      Cursor : Cursor_Type := Load_System_Cursor (IDC_HAND);
      --  Cursor_Pos : Point_Type := Get_Cursor_Position;
      use GWindows;
      --  type Point_Access is access all Point_Type;
      TVM_FIRST           : constant := 16#1100#;
      TVM_CREATEDRAGIMAGE : constant := TVM_FIRST + 18;
      function Sendmessage_tree (
         Hwnd   : GWindows.Types.Handle;
         Umsg   : Interfaces.C.int  := TVM_CREATEDRAGIMAGE;
         Wparam : Interfaces.C.int := 0;
         --  !! Lparam is perhaps wrong, MSDN: "Handle to the item that
         --     receives the new dragging bitmap."
         Lparam : Tree_Item_Node)
      return GWindows.Types.Handle;
      pragma Import (Stdcall, Sendmessage_tree,
                       "SendMessage" & Character_Mode_Identifier);

      Image_List_Handle : GWindows.Types.Handle;
      Point : aliased GWindows.Types.Point_Type := Get_Cursor_Position;

      Clicked_node : Tree_Item_Node;
      Selected_node : Tree_Item_Node;

      Main : My_Window_Type renames My_Window_Type (Parent (Window).all);
   begin
      Point := Get_Cursor_Position;
      Point := Point_To_Client (Window, Point);
      Clicked_node := Item_At_Position (Window, Point);
      Selected_node := Selected_Item (Window);
      Text (Main.Status,
            "Drag starting from Tree View, Node " &
            Text (Window, Clicked_node));
      Capture_Mouse (Main);
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
      --  * --  Set a provisory font
      --  * Get_Font (Window, Mem_Font);
      --  * Create_Stock_Font (Font, System);
      --  * Set_Font (Window, Font);
      Point := (0, 0);
      Window.Set_Image_List (Main.Drag_Image_List);  --  Needed ?
      Image_List_Handle := Sendmessage_tree (
        Hwnd => Handle (Window),
        Lparam => Clicked_node
      );
      if Selected_node = Clicked_node then
        Select_Item (Window, Selected_node);
      end if;
      --  * Set_Font (Window, Mem_Font);
      Handle (Main.Drag_Image_List, Image_List_Handle);
      Main.Drag_Image_List.Begin_Drag (Index => 0, X => 0, Y => 0);
      --  First position for dragging image
      Drag_Enter (Main, Point.X, Point.Y);
      Main.Dragging := True;
      Drag_Show_Image;
   end Start_Drag_TV;

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
            Start_Drag_TV (Window);
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
      if Window.Dragging then
         Drag_Leave (Window);
         End_Drag;
         Window.Drag_Image_List.Destroy;
         Window.Dragging := False;
         Text (Window.Status, "Drag stopped (button up)");
         Release_Mouse;
      else
         Text (Window.Status, "Button up (no drag)");
      end if;
   end On_Left_Mouse_Button_Up;

   procedure On_Mouse_Move (Window : in out My_Window_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (Keys);
   begin
      if Window.Dragging then
        Drag_Move (Window, X, Y);
      end if;
   end On_Mouse_Move;

begin
   Create_Stock_Font (GUI_Font, Default_GUI);
end Tutorial24_Window;
