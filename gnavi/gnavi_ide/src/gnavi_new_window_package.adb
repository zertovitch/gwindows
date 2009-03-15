with GNAVI_Main_Package;
with GNAVI_Window_Classes;
with GNAVI_Project_Window_Package;

with GWindows.GStrings;

package body GNAVI_New_Window_Package is

   procedure On_Create (Window : in out GNAVI_New_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
   begin
      Hide (GNAVI_New_Window);
      Can_Close := False;
   end Do_Close;

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Dock_Children (GNAVI_New_Window);
      GWindows.Packing_Boxes.Pack (GNAVI_New_Window.New_Window_Box);
      GWindows.Packing_Boxes.Pack (GNAVI_New_Window.Button_Pack_Box);

      GNAVI_Window_Classes.Init;

      for N in 1 .. GNAVI_Window_Classes.Count loop
         GWindows.List_Boxes.Add (GNAVI_New_Window.Window_Type_List,
                                  GNAVI_Window_Classes.Display_Name (N));
      end loop;

      GWindows.List_Boxes.Current (GNAVI_New_Window.Window_Type_List, 1);
      Do_Select (Window);
      GWindows.Edit_Boxes.Focus (GNAVI_New_Window.Window_Name_Box);
   end Do_Create;

   procedure Do_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Edit_Boxes.Text
        (GNAVI_New_Window.Description_Box,
         GNAVI_Window_Classes.Description
           (GWindows.List_Boxes.Current (GNAVI_New_Window.Window_Type_List)));
   end Do_Select;

   procedure Do_OK
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.GStrings;
   begin
      Close (GNAVI_New_Window);

      GNAVI_Window_Classes.Generate_Window
        (GWindows.List_Boxes.Current (GNAVI_New_Window.Window_Type_List),
         GWindows.Edit_Boxes.Text (GNAVI_New_Window.Window_Name_Box));

      declare
         File_Name : GWindows.GString :=
           GWindows.Edit_Boxes.Text (GNAVI_New_Window.Window_Name_Box) &
           ".gnw";
      begin
         To_Lower (File_Name);
         GNAVI_Project_Window_Package.Add_Window (File_Name);
      end;
   end Do_OK;

--  GNAVI: Create Global Instance
begin
   Create_As_Dialog (GNAVI_New_Window,
                     GNAVI_Main_Package.GNAVI_Main);
end GNAVI_New_Window_Package;
