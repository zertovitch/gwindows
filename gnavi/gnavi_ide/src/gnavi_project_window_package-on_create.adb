--  This file contains only automatically generated code.
--  Do not edit!
--  Change the GNAVI item instead.

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
      height => 1,
      left => 0,
      location => GWindows.Base.At_Right,
      text => "Project",
      top => 0,
      width => 20);
   GWindows.GControls.GSize_Bars.Maximum_Size (Window.Right_Size_Bar, 300);
   GWindows.GControls.GSize_Bars.Border (Window.Right_Size_Bar, False);
   GWindows.Panels.Create (Window.Project_Panel,
      Parent => Window,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Dock (Window.Project_Panel, GWindows.Base.Fill);
   GWindows.Common_Controls.Create (Window.Project_Tools,
      Parent => Window.Project_Panel,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Common_Controls.Dock (Window.Project_Tools, GWindows.Base.At_Top);
   GWindows.Common_Controls.On_Button_Select_Handler (Window.Project_Tools, Do_Toolbar_Select'Access);
   GWindows.List_Boxes.Create (Window.Window_Type_List,
      Parent => Window.Project_Panel,
      height => 1,
      left => 0,
      sort => False,
      top => 0,
      width => 1);
   GWindows.List_Boxes.Dock (Window.Window_Type_List, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.Window_Type_List, Do_Edit_Window'Access);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.Window_Type_List, Do_Window_Selection'Access);
   GWindows.List_Boxes.On_Selection_Cancel_Handler (Window.Window_Type_List, Do_Window_Selection'Access);
   GWindows.Panels.Create (Window.File_Panel,
      Parent => Window.Project_Panel,
      height => 100,
      left => 0,
      top => 0,
      width => 100);
   GWindows.Panels.Dock (Window.File_Panel, GWindows.Base.At_Bottom);
   GWindows.GControls.GSize_Bars.Create (Window.File_Size_Bar,
      Parent => Window.File_Panel,
      height => 3,
      left => 0,
      location => GWindows.Base.At_Top,
      top => 0,
      width => 100);
   GWindows.GControls.GSize_Bars.Minimum_Size (Window.File_Size_Bar, 50);
   GWindows.GControls.GSize_Bars.Live_Resize (Window.File_Size_Bar, True);
   GWindows.List_Boxes.Create (Window.File_List,
      Parent => Window.File_Panel,
      height => 30,
      id => 0,
      left => 0,
      sort => True,
      top => 0,
      width => 60);
   GWindows.List_Boxes.Dock (Window.File_List, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.File_List, Do_File_Edit'Access);
   GWindows.List_Boxes.On_Selection_Cancel_Handler (Window.File_List, Do_File_Selection'Access);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.File_List, Do_File_Selection'Access);
   GWindows.Windows.On_Create (GWindows.Windows.Window_Type (Window));
end On_Create;
