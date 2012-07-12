with GNAVI_New_Window_Package;
with GNAVI_Edit_Window_Package;
with GNAVI_Main_Package;
with GNAVI_Main_Menus;

with GNAVI_IDs;
with Standard_IDs;

with GWindows.Image_Lists;
with GWindows.Common_Controls;
with GWindows.Common_Dialogs;
with GWindows.GStrings;
with GWindows.Cursors;

with GNAT.Directory_Operations;

pragma Elaborate (GNAVI_Main_Package);

package body GNAVI_Project_Window_Package is

   procedure On_Create (Window : in out GNAVI_Project_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Private Package Methods
   -------------------------------------------------------------------------


   -------------------------------------------------------------------------
   --  Public Package Methods
   -------------------------------------------------------------------------

   procedure Load_Project (File_Name : in GWindows.GString)
   is
      use GNAVI_Project;
      use GNAT.Directory_Operations;
      use GWindows.GStrings;

   begin
      Change_Dir (Dir_Name (To_String (File_Name)));

      Dock (GNAVI_Project_Window, GWindows.Base.At_Left);
      Show (GNAVI_Project_Window);
      GNAVI_Main_Package.Dock_Children (GNAVI_Main_Package.GNAVI_Main);

      Load_Project (GNAVI_Project_Window.Project, File_Name);
      Text (GNAVI_Project_Window,
            Project_Name (GNAVI_Project_Window.Project));

      Refresh_Project;

      GNAVI_Main_Menus.Project_Loaded := True;
   end Load_Project;

   procedure Save_Project
   is
      use GNAVI_Project;
   begin
      Save_Project (GNAVI_Project_Window.Project);
   end Save_Project;

   procedure New_Window is
      use GNAVI_New_Window_Package;
   begin
      Show (GNAVI_New_Window);
   end New_Window;

   procedure Delete_Window is
      use GWindows.List_Boxes;

      Selected : constant Natural  :=
        Current (GNAVI_Project_Window.Window_Type_List);

   begin
      if Selected > 0 then
         GNAVI_Project.Delete_Window
           (GNAVI_Project_Window.Project, Selected);
      end if;

      Refresh_Project;
   end Delete_Window;

   procedure Add_Window (File_Name : in GWindows.GString)
   is
   begin
      GNAVI_Project.Add_Window (GNAVI_Project_Window.Project, File_Name);
      Refresh_Project;
   end Add_Window;

   procedure Add_Existing_Window is
      use GWindows.Common_Dialogs;
      use GWindows.GStrings;

      File_Name  : GWindows.GString_Unbounded;
      File_Title : GWindows.GString_Unbounded;
      Success    : Boolean := False;
   begin
      Open_File (GNAVI_Main_Package.GNAVI_Main,
                 "Add Window...",
                 File_Name,
                 (1 => (To_GString_Unbounded ("Windows (*.gnw)"),
                         To_GString_Unbounded ("*.gnw"))),
                 "gnw",
                 File_Title,
                 Success);

      if Success then
         GNAVI_Project.Add_Window (GNAVI_Project_Window.Project,
                                   To_GString_From_Unbounded (File_Name));
         Refresh_Project;
      end if;
   end Add_Existing_Window;

   procedure Edit_Window is
      use GNAVI_Project;
      use GWindows.List_Boxes;

      Selected : constant Natural  :=
        Current (GNAVI_Project_Window.Window_Type_List);
   begin
      if Selected > 0 then
         GNAVI_Edit_Window_Package.Open_Window
           (GNAVI_Project.Window_Name
            (GNAVI_Project_Window.Project, Selected));
      end if;
   end Edit_Window;

   procedure Refresh_Project is
      use GNAVI_Project;
   begin
      GWindows.List_Boxes.Clear (GNAVI_Project_Window.Window_Type_List);
      GNAVI_Main_Menus.Window_Selected := False;
      GNAVI_Main_Menus.File_Selected := False;

      for N in 1 .. Window_Count (GNAVI_Project_Window.Project) loop
         GWindows.List_Boxes.Add
           (GNAVI_Project_Window.Window_Type_List,
            Window_Name (GNAVI_Project_Window.Project, N));
      end loop;

      GWindows.List_Boxes.Clear (GNAVI_Project_Window.File_List);

      for N in 1 .. File_Count (GNAVI_Project_Window.Project) loop
         GWindows.List_Boxes.Add
           (GNAVI_Project_Window.File_List,
            File_Name (GNAVI_Project_Window.Project, N));
      end loop;

   end Refresh_Project;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   List : GWindows.Image_Lists.Image_List_Type;

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_IDs;
      use Standard_IDs;
      use GWindows.Image_Lists;
      use GWindows.Common_Controls;

      This : GNAVI_Project_Window_Type
        renames GNAVI_Project_Window_Type (Window);
   begin
      Create (List, "PRJ_TOOLBAR", 16);
      Set_Image_List (This.Project_Tools, List);
      Add_Button (This.Project_Tools, 0, ID_FILE_EDIT);
      Add_Button (This.Project_Tools, 1, ID_FILE_NEW);
      Add_Button (This.Project_Tools, 2, ID_FILE_OPEN);
      Add_Button (This.Project_Tools, 3, ID_FILE_DELETE);
   end Do_Create;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
      This : GNAVI_Project_Window_Type
        renames GNAVI_Project_Window_Type (Window);
   begin
      GNAVI_Project.Close (This.Project);
      Can_Close := False;
      GNAVI_Main_Menus.Project_Loaded := False;
      GNAVI_Main_Menus.Window_Selected := False;

      Dock (GNAVI_Project_Window, GWindows.Base.None);
      Hide (This);
      GNAVI_Main_Package.Dock_Children (GNAVI_Main_Package.GNAVI_Main);

      GWindows.List_Boxes.Clear (This.Window_Type_List);

      GNAVI_Main_Package.MDI_Close_All (GNAVI_Main_Package.GNAVI_Main);

   end Do_Close;

   procedure Do_New_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      New_Window;
   end Do_New_Window;

   procedure Do_Delete_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Delete_Window;
   end Do_Delete_Window;

   procedure Do_Edit_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Edit_Window;
   end Do_Edit_Window;

   procedure Do_Add_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Add_Existing_Window;
   end Do_Add_Window;

   procedure Do_Window_Selection
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
   begin
      if Current (GNAVI_Project_Window.Window_Type_List) > 0 then
         GNAVI_Main_Menus.Window_Selected := True;
      else
         GNAVI_Main_Menus.Window_Selected := False;
      end if;
   end Do_Window_Selection;

   procedure Do_Toolbar_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
   begin
      GNAVI_Main_Package.Handle_Menu (GNAVI_Main_Package.GNAVI_Main, Item);
   end Do_Toolbar_Select;

   procedure Do_File_Edit
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      -- Edit_File;
      null;
   end Do_File_Edit;

   procedure Do_File_Selection
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
   begin
      if Current (GNAVI_Project_Window.File_List) > 0 then
         GNAVI_Main_Menus.File_Selected := True;
      else
         GNAVI_Main_Menus.File_Selected := False;
      end if;
   end Do_File_Selection;

--  GNAVI: Create Global Instance
begin
   Create_As_Control (GNAVI_Project_Window,
                      GNAVI_Main_Package.GNAVI_Main,
                      "", 0, 0, 0, 0, Show => False);
end GNAVI_Project_Window_Package;
