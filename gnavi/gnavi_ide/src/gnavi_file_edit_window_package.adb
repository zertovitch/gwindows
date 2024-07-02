with GNAVI_Common;
with GNAVI_Main_Menus;
with GNAVI_Main_Package;

with GNAVI_IDs;
with Standard_IDs;

with GWindows.Colors;
with GWindows.Menus;

package body GNAVI_File_Edit_Window_Package is

   procedure On_Create
     (Window : in out GNAVI_File_Edit_Window_Type) is separate;

   -- On_Menu_Select added by GdM, July 2012
   -- Probably should also be separate, like On_Create,
   -- and generated GNAVI
   procedure On_Menu_Select (Window : in out GNAVI_File_Edit_Window_Type;
                             Item   : in     Integer) is
   begin
      Handle_Menu (Window, Item);
   end On_Menu_Select;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Scintilla;
      use GWindows.Colors;
      use GNAVI_Main_Menus;

      This : GNAVI_File_Edit_Window_Type
        renames GNAVI_File_Edit_Window_Type (Window);

      M : constant Base_Menus := Setup_Editor_Menus;
   begin
      MDI_Menu (This, M.Main_Menu, M.Windows_Menu);

      --  Set up editor
      Set_EOL_Mode (This.Edit_Box, SC_EOL_CRLF);
      Set_Tab_Width (This.Edit_Box, GNAVI_Common.TAB_WIDTH);
      Set_Use_Tabs (This.Edit_Box, False);
      Set_Edge_Column (This.Edit_Box, 80);
      Set_Edge_Mode (This.Edit_Box, EDGE_LINE);
      --  SetIndentationGuides (This.Edit_Box, True);

      Set_Lexer (This.Edit_Box, SCLEX_ADA);
      Set_Key_Words (This.Edit_Box, 0, GNAVI_Common.Key_Words);

      Style_Set_Fore (This.Edit_Box, STYLE_DEFAULT, Black);
      Style_Set_Back (This.Edit_Box, STYLE_DEFAULT, White);
      Style_Set_Size (This.Edit_Box, STYLE_DEFAULT, 10);
      Style_Set_Font (This.Edit_Box, STYLE_DEFAULT, "Courier");
      Style_Clear_All (This.Edit_Box);

      Style_Set_Fore (This.Edit_Box, SCE_ADA_DEFAULT, Black);
      Style_Set_Back (This.Edit_Box, SCE_ADA_DEFAULT, White);
      Style_Set_Size (This.Edit_Box, SCE_ADA_DEFAULT, 10);
      Style_Set_Font (This.Edit_Box, SCE_ADA_DEFAULT, "Courier");


      Style_Set_Fore (This.Edit_Box, SCE_ADA_COMMENTLINE, Red);
      Style_Set_Fore (This.Edit_Box, SCE_ADA_NUMBER, Blue);
      Style_Set_Fore (This.Edit_Box, SCE_ADA_WORD, Dark_Green);
      Style_Set_Fore (This.Edit_Box, SCE_ADA_STRING, Dark_Red);
      Style_Set_Fore (This.Edit_Box, SCE_ADA_CHARACTER, Blue);
      --  Style_Set_Fore (This.Edit_Box, SCE_ADA_OPERATOR, Black);
      Style_Set_Fore (This.Edit_Box, SCE_ADA_IDENTIFIER, Black);

      Style_Set_Fore (This.Edit_Box, SCE_ADA_STRINGEOL, White);
      Style_Set_Back (This.Edit_Box, SCE_ADA_STRINGEOL, Red);
   end Do_Create;

   procedure New_File
   is
      use type GWindows.Base.Pointer_To_Base_Window_Class;

      P : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        GNAVI_Main_Package.MDI_Active_Window
        (GNAVI_Main_Package.GNAVI_Main);

      Z : Boolean := False;

      New_Window : constant GNAVI_File_Edit_Window_Access :=
        new GNAVI_File_Edit_Window_Type;
   begin
      if P /= null then
         Z := GWindows.Windows.Zoom
           (GWindows.Windows.Window_Type (P.all));
      end if;

      Create_MDI_Child (New_Window.all,
                        GNAVI_Main_Package.GNAVI_Main,
                        Is_Dynamic => True);

      Zoom (New_Window.all, Z);
      Text (New_Window.all, "New File");
      Visible (New_Window.all);
   end New_File;

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type)
   is
      pragma Unreferenced (Item);
      use GWindows.Menus;
      use GWindows.Scintilla;
      use type GWindows.Windows.Hover_Item_Type;
      use Standard_IDs;
      use GNAVI_IDs;

      This : GNAVI_File_Edit_Window_Type
        renames GNAVI_File_Edit_Window_Type (Window);

      M : constant Menu_Type := GWindows.Windows.Menu
        (GWindows.Windows.Window_Type (Controlling_Parent (This).all));
   begin
      if Kind = GWindows.Windows.Menu then
         State (M, Command, ID_EDIT_UNDO, Enabled);
         State (M, Command, ID_EDIT_REDO, Enabled);
         State (M, Command, ID_EDIT_COPY, Enabled);
         State (M, Command, ID_EDIT_CUT, Enabled);
         State (M, Command, ID_EDIT_PASTE, Enabled);
         State (M, Command, ID_EDIT_DELETE, Enabled);
         State (M, Command, ID_EDIT_SELECTALL, Enabled);

         if Can_Paste (This.Edit_Box) then
            State (M, Command, ID_EDIT_PASTE, Enabled);
         else
            State (M, Command, ID_EDIT_PASTE, Grayed);
         end if;

         if Can_Undo (This.Edit_Box) then
            State (M, Command, ID_EDIT_UNDO, Enabled);
         else
            State (M, Command, ID_EDIT_UNDO, Grayed);
         end if;

         if Can_Redo (This.Edit_Box) then
            State (M, Command, ID_EDIT_REDO, Enabled);
         else
            State (M, Command, ID_EDIT_REDO, Grayed);
         end if;
      end if;
   end Check_Menu;

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
      use Standard_IDs;
      use GNAVI_IDs;
      use GWindows.Scintilla;

      This : GNAVI_File_Edit_Window_Type
        renames GNAVI_File_Edit_Window_Type (Window);
   begin
      case Item is
         when ID_FILE_SAVE =>
            --  Save_All_Views (This);
            null;
         when ID_EDIT_COPY =>
            Copy (This.Edit_Box);
         when ID_EDIT_PASTE =>
            Paste (This.Edit_Box);
         when ID_EDIT_CUT =>
            Cut (This.Edit_Box);
         when ID_EDIT_UNDO =>
            Undo (This.Edit_Box);
         when ID_EDIT_REDO =>
            Redo (This.Edit_Box);
         when ID_EDIT_SELECTALL =>
            Select_All (This.Edit_Box);
         when ID_EDIT_DELETE =>
            Clear (This.Edit_Box);
         when others =>
            null;
      end case;
   end Handle_Menu;

end GNAVI_File_Edit_Window_Package;
