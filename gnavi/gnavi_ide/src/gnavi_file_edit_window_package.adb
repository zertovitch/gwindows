with Ada.Unchecked_Conversion;

with GNAVI_Main_Package;
with GNAVI_Main_Menus;

with GNAVI_IDs;
with Standard_IDs;

with GWindows.Menus;
with GWindows.Colors;

with GNAT.OS_Lib;

package body GNAVI_File_Edit_Window_Package is

   TAB_WIDTH : constant := 3;

   Key_Words : constant GWindows.GString :=
     "abort abstract accept access aliased all array at begin body case " &
     "constant declare delay delta digits do else elsif end entry exception " &
     "exit for function generic goto if in is limited loop new null of " &
     "others out package pragma private procedure protected raise range " &
     "record renames requeue return reverse select separate subtype tagged " &
     "task terminate then type until use when while with";

   function To_Integer is
      new Ada.Unchecked_Conversion (GNAT.OS_Lib.OS_Time, Integer);

   procedure On_Create
     (Window : in out GNAVI_File_Edit_Window_Type) is separate;

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
      SetEOLMode (This.Edit_Box, SC_EOL_CRLF);
      SetTabWidth (This.Edit_Box, TAB_WIDTH);
      SetUseTabs (This.Edit_Box, False);
      SetEdgeColumn (This.Edit_Box, 80);
      SetEdgeMode (This.Edit_Box, EDGE_LINE);
      --  SetIndentationGuides (This.Edit_Box, True);

      SetLexer (This.Edit_Box, SCLEX_ADA);
      SetKeyWords (This.Edit_Box, 0, Key_Words);

      StyleSetFore (This.Edit_Box, STYLE_DEFAULT, Black);
      StyleSetBack (This.Edit_Box, STYLE_DEFAULT, White);
      StyleSetSize (This.Edit_Box, STYLE_DEFAULT, 10);
      StyleSetFont (This.Edit_Box, STYLE_DEFAULT, "Courier");
      StyleClearAll (This.Edit_Box);

      StyleSetFore (This.Edit_Box, SCE_ADA_DEFAULT, Black);
      StyleSetBack (This.Edit_Box, SCE_ADA_DEFAULT, White);
      StyleSetSize (This.Edit_Box, SCE_ADA_DEFAULT, 10);
      StyleSetFont (This.Edit_Box, SCE_ADA_DEFAULT, "Courier");


      StyleSetFore (This.Edit_Box, SCE_ADA_COMMENTLINE, Red);
      StyleSetFore (This.Edit_Box, SCE_ADA_NUMBER, Blue);
      StyleSetFore (This.Edit_Box, SCE_ADA_WORD, Dark_Green);
      StyleSetFore (This.Edit_Box, SCE_ADA_STRING, Dark_Red);
      StyleSetFore (This.Edit_Box, SCE_ADA_CHARACTER, Blue);
      --  StyleSetFore (This.Edit_Box, SCE_ADA_OPERATOR, Black);
      StyleSetFore (This.Edit_Box, SCE_ADA_IDENTIFIER, Black);

      StyleSetFore (This.Edit_Box, SCE_ADA_STRINGEOL, White);
      StyleSetBack (This.Edit_Box, SCE_ADA_STRINGEOL, Red);
   end Do_Create;

   procedure New_File
   is
      use type GWindows.Base.Pointer_To_Base_Window_Class;

      P : GWindows.Base.Pointer_To_Base_Window_Class :=
        GNAVI_Main_Package.MDI_Active_Window
        (GNAVI_Main_Package.GNAVI_Main);

      Z : Boolean := False;

      New_Window : GNAVI_File_Edit_Window_Access :=
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

         if CanPaste (This.Edit_Box) then
            State (M, Command, ID_EDIT_PASTE, Enabled);
         else
            State (M, Command, ID_EDIT_PASTE, Grayed);
         end if;

         if CanUndo (This.Edit_Box) then
            State (M, Command, ID_EDIT_UNDO, Enabled);
         else
            State (M, Command, ID_EDIT_UNDO, Grayed);
         end if;

         if CanRedo (This.Edit_Box) then
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
            SelectAll (This.Edit_Box);
         when ID_EDIT_DELETE =>
            Clear (This.Edit_Box);
         when others =>
            null;
      end case;
   end Handle_Menu;

end GNAVI_File_Edit_Window_Package;
