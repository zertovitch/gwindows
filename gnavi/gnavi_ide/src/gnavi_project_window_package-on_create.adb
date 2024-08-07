--  This file contains only automatically generated code
--  and may be regenerated without notice.
--  DO NOT MODIFY IT !
--  Change the GNAVI item & properties instead.
--  Generated by: GNAVI ICG (Interactive Code Generator).

with GWindows.Application;
with GWindows.Drawing_Objects;

separate (GNAVI_Project_Window_Package)
procedure On_Create (Window : in out GNAVI_Project_Window_Type)
is
begin
   Visible (Window, False);
   Width (Window, 260);
   Set_Standard_Font (Window, GWindows.Drawing_Objects.Default_GUI);
   On_Create_Handler (Window, Do_Create'Access);
   On_Close_Handler (Window, Do_Close'Access);
   GWindows.GControls.GSize_Bars.Create (Window.Right_Size_Bar,
      Parent => Window,
      Height => 1,
      Left => 0,
      Location => GWindows.Base.At_Right,
      Text => "Project",
      Top => 0,
      Width => 20);
   GWindows.GControls.GSize_Bars.Maximum_Size (Window.Right_Size_Bar, 300);
   GWindows.GControls.GSize_Bars.Border (Window.Right_Size_Bar, False);
   GWindows.Panels.Create (Window.Project_Panel,
      Parent => Window,
      Height => 1,
      Left => 0,
      Top => 0,
      Width => 1);
   GWindows.Panels.Dock (Window.Project_Panel, GWindows.Base.Fill);
   GWindows.Common_Controls.Create (Window.Project_Tools,
      Parent => Window.Project_Panel,
      Height => 1,
      Left => 0,
      Top => 0,
      Width => 1);
   GWindows.Common_Controls.Dock (Window.Project_Tools, GWindows.Base.At_Top);
   GWindows.Common_Controls.On_Button_Select_Handler (Window.Project_Tools, Do_Toolbar_Select'Access);
   GWindows.List_Boxes.Create (Window.Window_Type_List,
      Parent => Window.Project_Panel,
      Height => 1,
      Left => 0,
      Sort => False,
      Top => 0,
      Width => 1);
   GWindows.List_Boxes.Dock (Window.Window_Type_List, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.Window_Type_List, Do_Edit_Window'Access);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.Window_Type_List, Do_Window_Selection'Access);
   GWindows.List_Boxes.On_Selection_Cancel_Handler (Window.Window_Type_List, Do_Window_Selection'Access);
   GWindows.Panels.Create (Window.File_Panel,
      Parent => Window.Project_Panel,
      Height => 100,
      Left => 0,
      Top => 0,
      Width => 100);
   GWindows.Panels.Dock (Window.File_Panel, GWindows.Base.At_Bottom);
   GWindows.GControls.GSize_Bars.Create (Window.File_Size_Bar,
      Parent => Window.File_Panel,
      Height => 3,
      Left => 0,
      Location => GWindows.Base.At_Top,
      Top => 0,
      Width => 100);
   GWindows.GControls.GSize_Bars.Minimum_Size (Window.File_Size_Bar, 50);
   GWindows.GControls.GSize_Bars.Live_Resize (Window.File_Size_Bar, True);
   GWindows.List_Boxes.Create (Window.File_List,
      Parent => Window.File_Panel,
      Height => 30,
      ID => 0,
      Left => 0,
      Sort => True,
      Top => 0,
      Width => 60);
   GWindows.List_Boxes.Dock (Window.File_List, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.File_List, Do_File_Edit'Access);
   GWindows.List_Boxes.On_Selection_Cancel_Handler (Window.File_List, Do_File_Selection'Access);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.File_List, Do_File_Selection'Access);
   GWindows.Windows.On_Create (GWindows.Windows.Window_Type (Window));
end On_Create;
