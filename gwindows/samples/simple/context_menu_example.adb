with GWindows.Application,
     GWindows.Base,
     GWindows.Menus,
     GWindows.Message_Boxes,
     GWindows.Common_Controls,
     GWindows.Windows.Main;

procedure Context_Menu_Example is  --  "Context Menu" version of Menu_Example
   use GWindows.Windows.Main;
   use GWindows.Menus;
   use GWindows.Common_Controls;

   Top : GWindows.Windows.Main.Main_Window_Type;
   LV : List_View_Control_Type;
   Context_Menu : constant Menu_Type := Create_Popup;

   ID_OPEN : constant := 100;
   ID_SAVE : constant := 101;
   ID_EXIT : constant := 999;

   procedure Do_Right_Click
      (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      Immediate_Popup_Menu (Context_Menu, Top);
   end Do_Right_Click;

   procedure Do_Menu (Window : in out GWindows.Base.Base_Window_Type'Class;
                      Item   : in     Integer)
   is
   pragma Unreferenced (Window);
      use GWindows.Message_Boxes;
   begin
      case Item is
         when ID_EXIT =>
            GWindows.Application.End_Application;
         when ID_OPEN =>
            Text (Top, "Context Menu Example - My File");
            State (Context_Menu, Command, ID_SAVE, Enabled);
            Text (Context_Menu, Command, ID_SAVE, "&Save My File");
         when ID_SAVE =>
            Message_Box (Top, "Save...", "Your file has been saved.",
                         OK_Box, Information_Icon);
         when others =>
            null;
      end case;
   end Do_Menu;

begin
   Create (Top, "Context Menu Example");
   Size (Top, 500, 300);
   Visible (Top);

   Append_Item (Context_Menu, "&Open...", ID_OPEN);
   Append_Item (Context_Menu, "&Save", ID_SAVE);
   Append_Separator (Context_Menu);
   Append_Item (Context_Menu, "E&xit", ID_EXIT);

   Create (LV, Top, 20, 20, 300, 200);
   Insert_Item (LV, "Right-click in this box!", 0);

   --  Here, callbacks are bound to the right click
   --  and menu select messages.
   --  Alternatively (for larger applications), you can derive a new
   --  type from List_View_Control_Type and Main_Window_Type and override
   --  the On_Right_Click and On_Menu_Select methods, instead
   --  of using callbacks.
   On_Right_Click_Handler (LV, Do_Right_Click'Unrestricted_Access);
   On_Menu_Select_Handler (Top, Do_Menu'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Context_Menu_Example;
