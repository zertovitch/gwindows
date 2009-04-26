with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Menus; use GWindows.Menus;
with GWindows.Application;
with GWindows.Base;

procedure Tutorial11 is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;
   Main_Menu   : Menu_Type := Create_Menu;
   File_Menu   : Menu_Type := Create_Popup;
   Sub_Menu    : Menu_Type := Create_Popup;

   ID_Exit     : constant := 100;
   ID_Open     : constant := 101;
   ID_Save     : constant := 102;
   ID_About    : constant := 103;

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
      pragma Unreferenced (Window);
   begin
      case Item is
         when ID_Exit =>
            GWindows.Application.End_Application;
         when others =>
            null;
      end case;
   end Do_Menu_Select;

   procedure Do_Context_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer)
   is
   begin
      Display_Context_Menu (Main_Window_Type (Window), File_Menu, 0, X, Y);
   end Do_Context_Menu;

begin
   Create (Main_Window, "My First Window");
   Visible (Main_Window, True);

   Append_Item (File_Menu, "&Open", ID_Open);
   Append_Item (File_Menu, "&Save", ID_Save);
   Append_Separator (File_Menu);

   Append_Item (File_Menu, "E&xit", ID_Exit);
   Append_Menu (Main_Menu, "&Files", File_Menu);

   Append_Item (Sub_Menu, "A&bout", ID_About);
   Append_Menu (File_Menu, "Other", Sub_Menu);

   Menu (Main_Window, Main_Menu);

   On_Menu_Select_Handler (Main_Window, Do_Menu_Select'Unrestricted_Access);
   On_Context_Menu_Handler (Main_Window, Do_Context_Menu'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Tutorial11;
