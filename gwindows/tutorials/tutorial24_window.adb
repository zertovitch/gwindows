with GWindows.Types; use GWindows.Types;
with GWindows.Cursors; use GWindows.Cursors;
with Interfaces.C;
with GWindows.Base; use GWindows.Base;
with GWindows.Image_Lists; use GWindows.Image_Lists;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Static_Controls; use GWindows.Static_Controls;

package body Tutorial24_Window is

   procedure Start_Drag (Window : in out LV_with_Drag)
   is
      Font : Font_Type;
      --  Font is needed to create drag image, otherwise drag image
      --  is invisible - Windows bug since Vista!
      IDC_HAND : constant := 32649;
      Cursor : Cursor_Type := Load_System_Cursor (IDC_HAND);
      --  Cursor_Pos : Point_Type := Get_Cursor_Position;
      use GWindows;
      type Point_Access is access all Point_Type;
      LVM_FIRST                    : constant := 16#1000#;
      LVM_CREATEDRAGIMAGE          : constant := LVM_FIRST + 33;

      Image_List_Handle : GWindows.Types.Handle;
      Drag_Image_List : Image_List_Type;
      Point : aliased GWindows.Types.Point_Type := Get_Cursor_Position;

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
      Text (My_Window_Type (Parent (Window).all).Status,
            "Drag starting from ListView");
      Capture_Mouse (My_Window_Type (Parent (Window).all));
      --  ^ This is needed, otherwise dropping outside of parent window
      --    is not captured via On_Left_Mouse_Button_Up.
      if Cursor /= 0 then
        Set_Cursor (Cursor);
      end if;
      --  Image associated with dragging
      Create_Stock_Font (Font, Default_GUI);
      Set_Font (Window, Font); -- cf Font declaration for explanation
      Image_List_Handle := Sendmessage_list (Hwnd => Handle (Window),
                           Wparam => 0,
                           Lparam => Point'Access
      );
      Handle (Drag_Image_List, Image_List_Handle);
      Begin_Drag (Drag_Image_List, 0, -10, 0);
      Point := Get_Cursor_Position;
      Point := Point_To_Client (Window, Point);
      --  First position for dragging image
      Drag_Enter (Window, Point.X, Point.Y);
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
     --          Gwindows.Image_Lists.Drag_Leave(main);
     --         Gwindows.Image_Lists.End_Drag;
     Text (Window.Status, "Drag stopped (button up)");
     Release_Mouse;
   end On_Left_Mouse_Button_Up;

begin
   Create_Stock_Font (GUI_Font, Default_GUI);
end Tutorial24_Window;
