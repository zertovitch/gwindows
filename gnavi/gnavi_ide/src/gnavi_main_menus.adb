with Standard_IDs;
with GNAVI_IDs;

package body GNAVI_Main_Menus is

   function Setup_Base_Menus return Base_Menus is
      use GWindows.Menus;
      use Standard_IDs;
      use GNAVI_IDs;

      M : Base_Menus;
   begin
      M.Main_Menu    := Create_Menu;
      M.File_Menu    := Create_Menu;
      M.Project_Menu := Create_Menu;
      M.Help_Menu    := Create_Menu;


      Append_Item (M.File_Menu, "&New Window...", ID_FILE_NEW);
      Append_Item (M.File_Menu, "&Open Window", ID_FILE_EDIT);
      Append_Item (M.File_Menu, "&Add Window...", ID_FILE_OPEN);
      Append_Item (M.File_Menu, "&Remove Window", ID_FILE_DELETE);
      Append_Separator (M.File_Menu);
      Append_Item (M.File_Menu, "Ne&w File", ID_PFILE_NEW);
      Append_Item (M.File_Menu, "Open &File...", ID_PFILE_EDIT);
      Append_Item (M.File_Menu, "&Save File", ID_PFILE_SAVE);
      State (M.File_Menu, Command, ID_PFILE_SAVE, Grayed);
      Append_Item (M.File_Menu, "Sa&ve as...", ID_PFILE_SAVE_AS);
      State (M.File_Menu, Command, ID_PFILE_SAVE_AS, Grayed);
      Append_Item (M.File_Menu, "Open &Project File", ID_PFILE_OPEN);
      Append_Item (M.File_Menu, "Add F&ile to Project...", ID_PFILE_ADD);
      Append_Item (M.File_Menu, "R&emove File from Project", ID_PFILE_DELETE);
      Append_Separator (M.File_Menu);
      Append_Item (M.File_Menu, "E&xit", ID_APP_EXIT);

      Append_Menu (M.Main_Menu, "&File", M.File_Menu);


      Append_Item (M.Project_Menu, "&New Project...", ID_PROJECT_NEW);
      Append_Item (M.Project_Menu, "&Open Project...", ID_PROJECT_OPEN);
      Append_Item (M.Project_Menu, "&Close Project", ID_PROJECT_CLOSE);
      Append_Separator (M.Project_Menu);
      Append_Item (M.Project_Menu, "&Build", ID_COMPILE);
      Append_Item (M.Project_Menu, "&Run", ID_RUN);
      Append_Menu (M.Main_Menu, "&Project", M.Project_Menu);


      Append_Item (M.Help_Menu, "&About...", ID_APP_ABOUT);
      Append_Item (M.Help_Menu, "&Help", ID_HELP_INDEX);
      Append_Menu (M.Main_Menu, "&Help", M.Help_Menu);

      return M;
   end Setup_Base_Menus;

   function Setup_Editor_Menus return Base_Menus is
      use GWindows.Menus;
      use Standard_IDs;
      use GNAVI_IDs;

      M : Base_Menus := Setup_Base_Menus;
      T : constant GWindows.GCharacter := GWindows.GCharacter'Val (9);
   begin
      M.Edit_Menu := Create_Menu;
      Append_Item (M.Edit_Menu, "&Undo" & T & "Ctrl+Z", ID_EDIT_UNDO);
      Append_Item (M.Edit_Menu, "&Redo" & T & "Ctrl+Y", ID_EDIT_REDO);
      Append_Separator (M.Edit_Menu);
      Append_Item (M.Edit_Menu, "Cu&t" & T & "Ctrl+X", ID_EDIT_CUT);
      Append_Item (M.Edit_Menu, "&Copy" & T & "Ctrl+C", ID_EDIT_COPY);
      Append_Item (M.Edit_Menu, "&Paste" & T & "Ctrl+V", ID_EDIT_PASTE);
      Append_Item (M.Edit_Menu, "&Delete" & T & "Del", ID_EDIT_DELETE);
      Append_Separator (M.Edit_Menu);
      Append_Item (M.Edit_Menu, "&Select All" & T & "Ctrl+A",
                   ID_EDIT_SELECTALL);

      Insert_Menu (M.Main_Menu,
                   Position,
                   2,
                   "&Edit",
                   M.Edit_Menu);

      M.Window_Menu := Create_Menu;

      Append_Item (M.Window_Menu, "&Cascade", ID_WINDOW_CASCADE);
      Append_Item (M.Window_Menu, "Tile &Horizontal", ID_WINDOW_TILE_HORZ);
      Append_Item (M.Window_Menu, "&Tile Vertical", ID_WINDOW_TILE_VERT);
      Append_Item (M.Window_Menu, "Close &All", ID_WINDOW_CLOSE_ALL);
      M.Windows_Menu := 4;
      Insert_Menu (M.Main_Menu,
                   Position,
                   M.Windows_Menu,
                   "&Window",
                   M.Window_Menu);

      return M;
   end Setup_Editor_Menus;

   function Menu_Description (Command : Integer) return GWindows.GString
   is
      use GNAVI_IDs;
      use Standard_IDs;
   begin
      case Command is
         when ID_FILE_NEW =>
            return "Create new window";
         when ID_FILE_OPEN =>
            return "Add existing window to project";
         when ID_FILE_EDIT =>
            return "Open and edit window";
         when ID_FILE_DELETE =>
            return "Remove window from project";
         when ID_PFILE_NEW =>
            return "Create new file";
         when ID_PFILE_OPEN =>
            return "Open project file";
         when ID_PFILE_ADD =>
            return "Add existing file to project";
         when ID_PFILE_SAVE =>
            return "Save file";
         when ID_PFILE_SAVE_AS =>
            return "Save file as";
         when ID_PFILE_EDIT =>
            return "Open and edit file";
         when ID_PFILE_DELETE =>
            return "Remove file from project";
         when ID_COMPILE =>
            return "Build project";
         when ID_RUN =>
            return "Run application";
         when ID_PROJECT_NEW =>
            return "Create new project";
         when ID_PROJECT_OPEN =>
            return "Open existing project";
         when ID_PROJECT_CLOSE =>
            return "Close project";
         when ID_APP_EXIT =>
            return "Exit application";
         when ID_APP_ABOUT =>
            return "About application";
         when ID_HELP_INDEX =>
            return "Open help window";
         when ID_WINDOW_NEW =>
            return "Create new window";
         when ID_WINDOW_ARRANGE =>
            return "Arrange icons";
         when ID_WINDOW_CASCADE =>
            return "Cascade windows";
         when ID_WINDOW_TILE_HORZ =>
            return "Tile windows horizontaly";
         when ID_WINDOW_TILE_VERT =>
            return "Tile windows verticaly";
         when ID_WINDOW_SPLIT =>
            return "Split window";
         when ID_WINDOW_CLOSE_ALL =>
            return "Close all windows";
         when 5000 .. 6000 =>
            return "Activate selected window";
         when others =>
            return "";
      end case;
   end Menu_Description;

   procedure Set_Menu_States (Menu : in out GWindows.Menus.Menu_Type) is
      use GWindows.Menus;
      use GNAVI_IDs;
      use Standard_IDs;
   begin
      if Project_Loaded then
         State (Menu, Command, ID_FILE_NEW, Enabled);
         State (Menu, Command, ID_FILE_OPEN, Enabled);
         State (Menu, Command, ID_PFILE_ADD, Enabled);
         State (Menu, Command, ID_PROJECT_CLOSE, Enabled);
         State (Menu, Command, ID_COMPILE, Enabled);
         State (Menu, Command, ID_RUN, Enabled);
      else
         State (Menu, Command, ID_FILE_NEW, Grayed);
         State (Menu, Command, ID_FILE_OPEN, Grayed);
         State (Menu, Command, ID_PFILE_ADD, Grayed);
         State (Menu, Command, ID_PROJECT_CLOSE, Grayed);
         State (Menu, Command, ID_COMPILE, Grayed);
         State (Menu, Command, ID_RUN, Grayed);
      end if;

      if Window_Selected then
         State (Menu, Command, ID_FILE_DELETE, Enabled);
         State (Menu, Command, ID_FILE_EDIT, Enabled);
      else
         State (Menu, Command, ID_FILE_DELETE, Grayed);
         State (Menu, Command, ID_FILE_EDIT, Grayed);
      end if;

      if File_Selected then
         State (Menu, Command, ID_PFILE_OPEN, Enabled);
         State (Menu, Command, ID_PFILE_DELETE, Enabled);
      else
         State (Menu, Command, ID_PFILE_OPEN, Grayed);
         State (Menu, Command, ID_PFILE_DELETE, Grayed);
      end if;

   end Set_Menu_States;

end GNAVI_Main_Menus;
