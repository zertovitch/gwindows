with GWindows.Application,
     GWindows.Base,
     GWindows.Menus,
     GWindows.Message_Boxes,
     GWindows.Windows.Main;

procedure Menu_Example is
   use GWindows.Windows.Main;
   use GWindows.Menus;

   Top : GWindows.Windows.Main.Main_Window_Type;

   Top_Menu  : Menu_Type;
   File_Menu : Menu_Type;

   ID_OPEN : constant := 100;
   ID_SAVE : constant := 101;
   ID_EXIT : constant := 999;

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
            Text (Top, "Menu Example - My File");
            State (Top_Menu, Command, ID_SAVE, Enabled);
            Text (Top_Menu, Command, ID_SAVE, "&Save My File");
         when ID_SAVE =>
            Message_Box (Top, "Save...", "Your file has been saved.",
                         OK_Box, Information_Icon);
         when others =>
            null;
      end case;
   end Do_Menu;

begin
   Create (Top, "Menu Example");
   Size (Top, 500, 200);
   Visible (Top);

   Top_Menu := Create_Menu;
   File_Menu := Create_Popup;

   Append_Menu (Top_Menu, "&File", File_Menu);

   Append_Item (File_Menu, "&Open...", ID_OPEN);
   Append_Item (File_Menu, "&Save", ID_SAVE);
   Append_Separator (File_Menu);
   Append_Item (File_Menu, "E&xit", ID_EXIT);

   Menu (Top, Top_Menu);
   State (Top_Menu, Command, ID_SAVE, Grayed);

   --  Here, a callback is bound to the menu select messages.
   --  Alternatively (for larger applications), you can derive
   --  a new type from Main_Window_Type and override
   --  the On_Menu_Select method, instead of using callbacks.
   On_Menu_Select_Handler (Top, Do_Menu'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Menu_Example;
