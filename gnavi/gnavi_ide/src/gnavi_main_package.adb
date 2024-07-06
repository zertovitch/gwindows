with GWindows.Common_Dialogs;
with GWindows.GStrings;
with GWindows.Menus;
with GWindows.Message_Boxes;
with GWindows.Drawing_Objects;
with GWindows.Image_Lists;

with GNATCOM.Initialize;

with GNAVI_Help_Window_Package;
with GNAVI_Project_Window_Package;
with GNAVI_New_Project_Package;
with GNAVI_Controls_Window_Package;
with GNAVI_File_Edit_Window_Package;
with GNAVI_Project;
with GNAVI_Controls;

with Standard_IDs;
with GNAVI_IDs;

with GNAVI_Main_Menus;

package body GNAVI_Main_Package is

   procedure On_Create (Window : in out GNAVI_Main_Type) is separate;

   --  On_Menu_Select added by GdM, July 2012
   --  Probably should also be separate, like On_Create,
   --  and generated GNAVI
   procedure On_Menu_Select (Window : in out GNAVI_Main_Type;
                             Item   : in     Integer) is
   begin
      Handle_Menu (Window, Item);
   end On_Menu_Select;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   List : GWindows.Image_Lists.Image_List_Type;

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
      use GWindows.Common_Controls;
      use GNAVI_Main_Menus;
      use GWindows.Image_Lists;
      use GNAVI_IDs;
      use Standard_IDs;

      This : GNAVI_Main_Type renames GNAVI_Main;

      M : constant Base_Menus := Setup_Base_Menus;

   begin
      MDI_Menu (GNAVI_Main, M.Main_Menu, M.Windows_Menu);

      Small_Icon (GNAVI_Main, "MAIN_ICON");
      Large_Icon (GNAVI_Main, "MAIN_ICON");

      Create (List, "MAIN_TOOLBAR", 16);
      Set_Image_List (This.Top_Tools, List);
      Add_Button (This.Top_Tools, 0, ID_PROJECT_NEW);
      Add_Button (This.Top_Tools, 1, ID_PROJECT_OPEN);
      Add_Button (This.Top_Tools, 8, ID_RUN);
      Add_Button (This.Top_Tools, 7, ID_HELP_INDEX);

      GNAVI_Controls.Init;
   end Do_Create;

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
      use Standard_IDs;
      use GNAVI_IDs;

      This : GNAVI_Main_Type renames GNAVI_Main_Type (Window);
   begin
      case Item is
         when ID_FILE_NEW =>
            GNAVI_Project_Window_Package.New_Window;
         when ID_FILE_OPEN =>
            GNAVI_Project_Window_Package.Add_Existing_Window;
         when ID_FILE_EDIT =>
            GNAVI_Project_Window_Package.Edit_Window;
         when ID_FILE_DELETE =>
            GNAVI_Project_Window_Package.Delete_Window;
         when ID_PFILE_NEW =>
            GNAVI_File_Edit_Window_Package.New_File;
         when ID_PROJECT_NEW =>
            Do_New_GNAVI_Project (Window);
         when ID_PROJECT_OPEN =>
            Do_Open_GNAVI_Project (Window);
         when ID_PROJECT_CLOSE =>
            Do_Close_GNAVI_Project (Window);
         when ID_APP_EXIT =>
            Close (This);
         when ID_COMPILE =>
            Do_Compile (This);
         when ID_RUN =>
            Do_Run (This);
         when ID_APP_ABOUT =>
            GNAVI_Help_Window_Package.Display_About;
         when ID_HELP_INDEX =>
            GNAVI_Help_Window_Package.Display_Help;
         when ID_WINDOW_CASCADE =>
            MDI_Cascade (This);
         when ID_WINDOW_TILE_HORZ =>
            MDI_Tile_Horizontal (This);
         when ID_WINDOW_TILE_VERT =>
            MDI_Tile_Vertical (This);
         when ID_WINDOW_CLOSE_ALL =>
            MDI_Close_All (This);
         when others =>
            null;
      end case;
   end Handle_Menu;

   procedure Do_New_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
      use GNAVI_New_Project_Package;
   begin
      Center (GNAVI_New_Project);
      Show (GNAVI_New_Project);
   end Do_New_GNAVI_Project;

   procedure Do_Open_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Common_Dialogs;
      use GWindows.GStrings;

      use GNAVI_Project_Window_Package;

      This : GNAVI_Main_Type renames GNAVI_Main_Type (Window);

      File_Name  : GWindows.GString_Unbounded;
      File_Title : GWindows.GString_Unbounded;
      Success    : Boolean := False;
   begin
      Open_File (Window,
                 "Open Project...",
                 File_Name,
                 (1 => (To_GString_Unbounded ("Projects (*.gnp)"),
                        To_GString_Unbounded ("*.gnp"))),
                 "gnp",
                 File_Title,
                 Success);

      if Success then
         MDI_Close_All (This);
         Load_Project (To_GString_From_Unbounded (File_Name));
      end if;
   end Do_Open_GNAVI_Project;

   procedure Do_Save_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      GNAVI_Project_Window_Package.Save_Project;
   end Do_Save_GNAVI_Project;

   procedure Do_Close_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Main_Type renames GNAVI_Main_Type (Window);
      use GNAVI_Project_Window_Package;
   begin
      Close (GNAVI_Project_Window);
      This.Text ("GNAVI - Open Source Visual RAD");
   end Do_Close_GNAVI_Project;

   procedure Do_Drop_GNAVI_Project
     (Window     : in out GWindows.Base.Base_Window_Type'Class;
      File_Names : in     GWindows.Windows.Array_Of_File_Names)
   is
      This : GNAVI_Main_Type renames GNAVI_Main_Type (Window);
      use GWindows.Message_Boxes, GWindows.GStrings;
   begin
      if File_Names'Length > 1 then
         Message_Box
            (This,
             "Files dropped",
             "Cannot drop more than one project.",
             OK_Box,
             Error_Icon);
      else
         MDI_Close_All (This);
         GNAVI_Project_Window_Package.Load_Project
            (To_GString_From_Unbounded (File_Names (File_Names'First)));
      end if;
   end Do_Drop_GNAVI_Project;

   procedure Do_Compile
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
      use GNAVI_Project_Window_Package;
   begin
      if GNAVI_Main_Menus.Project_Loaded then
         GNAVI_Project.Run_ICG (GNAVI_Project_Window.Project);
         GNAVI_Project.Compile (GNAVI_Project_Window.Project);
      end if;
   end Do_Compile;

   procedure Do_Run
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
      use GNAVI_Project_Window_Package;
   begin
      if GNAVI_Main_Menus.Project_Loaded then
         GNAVI_Project.Run_ICG (GNAVI_Project_Window.Project);
         GNAVI_Project.Compile (GNAVI_Project_Window.Project);
         GNAVI_Project.Run (GNAVI_Project_Window.Project);
      end if;
   end Do_Run;

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type)
   is
      use GWindows.Common_Controls;
      use type GWindows.Windows.Hover_Item_Type;
      use GWindows.Menus;
      use GNAVI_Main_Menus;

      This : GNAVI_Main_Type renames GNAVI_Main_Type (Window);
      M    : GWindows.Menus.Menu_Type := Menu (This);
   begin
      if Kind = GWindows.Windows.Menu then
         Set_Menu_States (M);
      end if;

      if Kind = GWindows.Windows.Menu_Item and Item > 0 then
         Text (This.Bottom_Status, Menu_Description (Item));
      else
         Text (This.Bottom_Status, "Ready");
      end if;
   end Check_Menu;

   procedure Do_Toolbar_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
   pragma Unreferenced (Window);
   begin
      Handle_Menu (GNAVI_Main, Item);
   end Do_Toolbar_Select;

--  GNAVI: Create Global Instance
begin
   GNATCOM.Initialize.Initialize_COM;

   Create_MDI_Top (GNAVI_Main);

   GNAVI_Controls_Window_Package.Create_As_Control
     (GNAVI_Controls_Window_Package.GNAVI_Controls_Window,
      GNAVI_Main,
      "", 0, 0, 0, 0, Show => True);

   Dock_Children (GNAVI_Main);

end GNAVI_Main_Package;
