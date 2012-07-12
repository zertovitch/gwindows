with GNAVI_Project_Classes;
with GNAVI_Project_Window_Package;

with GWindows.GStrings;
with GWindows.Message_Boxes;

with GNAT.Directory_Operations;

package body GNAVI_New_Project_Package is

   procedure On_Create (Window : in out GNAVI_New_Project_Type) is separate;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Packing_Boxes;
   begin
      Dock_Children (GNAVI_New_Project);
      Pack (GNAVI_New_Project.New_Project_Box);
      Pack (GNAVI_New_Project.Button_Pack_Box);

      GNAVI_Project_Classes.Init;

      for N in 1 .. GNAVI_Project_Classes.Count loop
         GWindows.List_Boxes.Add (GNAVI_New_Project.Project_Type_List,
                                  GNAVI_Project_Classes.Display_Name (N));
      end loop;

      GWindows.List_Boxes.Current (GNAVI_New_Project.Project_Type_List, 1);
      Do_Select (Window);
      GWindows.Edit_Boxes.Focus (GNAVI_New_Project.Project_Name_Box);
   end Do_Create;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
   begin
      Hide (GNAVI_New_Project);
      Can_Close := False;
   end Do_Close;

   procedure Do_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Edit_Boxes.Text
        (GNAVI_New_Project.Description_Box,
         GNAVI_Project_Classes.Description
           (GWindows.List_Boxes.Current
              (GNAVI_New_Project.Project_Type_List)));
   end Do_Select;

   procedure Do_OK
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.GStrings;
      use GNAVI_Project_Window_Package;

   begin
      Close (GNAVI_New_Project);

      declare
         use GNAT.Directory_Operations;
         use GWindows.Message_Boxes;

         Prj_Dir : constant String :=
           To_String (GWindows.Edit_Boxes.Text
                      (GNAVI_New_Project.Project_Dir_Box)) &
           Dir_Separator &
           To_String (GWindows.Edit_Boxes.Text
                      (GNAVI_New_Project.Project_Name_Box));
      begin
         Make_Dir (Prj_Dir);
         Change_Dir (Prj_Dir);
      exception
         when Directory_Error =>
            Message_Box ("Directory Error",
                         "Unable to create project directroy at:" &
                         To_GString_From_String (Prj_Dir),
                         Icon => Error_Icon);
            Show (GNAVI_New_Project);
            return;
      end;

      GNAVI_Project_Classes.Generate_Project
        (GWindows.List_Boxes.Current (GNAVI_New_Project.Project_Type_List),
         GWindows.Edit_Boxes.Text (GNAVI_New_Project.Project_Name_Box));

      declare
         File_Name : GWindows.GString :=
           GWindows.Edit_Boxes.Text (GNAVI_New_Project.Project_Name_Box) &
           ".gnp";
      begin
         To_Lower (File_Name);
         Load_Project (File_Name);
      end;

   end Do_OK;

--  GNAVI: Create Global Instance
begin
   Create (GNAVI_New_Project);
end GNAVI_New_Project_Package;

