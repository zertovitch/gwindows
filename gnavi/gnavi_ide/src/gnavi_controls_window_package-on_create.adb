--  This file contains only automatically generated code.
--  Do not edit!
--  Change the GNAVI item instead.

with GWindows.Application;
with GWindows.Drawing_Objects;

separate (GNAVI_Controls_Window_Package)
procedure On_Create (Window : in out GNAVI_Controls_Window_Type)
is
begin
   Width (Window, 200);
   Set_Standard_Font (Window, GWindows.Drawing_Objects.Default_GUI);
   Dock (Window, GWindows.Base.At_Right);
On_Create_Handler (Window, Do_Create'Access);
   GWindows.GControls.GSize_Bars.Create (Window.Left_Size_Bar,
      Parent => Window,
      height => 1,
      left => 0,
      location => GWindows.Base.At_Left,
      text => "Controls",
      top => 0,
      width => 20);
   GWindows.GControls.GSize_Bars.Maximum_Size (Window.Left_Size_Bar, 300);
   GWindows.GControls.GSize_Bars.Border (Window.Left_Size_Bar, False);
   GWindows.Panels.Create (Window.Controls_Panel,
      Parent => Window,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Dock (Window.Controls_Panel, GWindows.Base.Fill);
   GWindows.List_Boxes.Create (Window.Controls_List,
      Parent => Window.Controls_Panel,
      height => 10,
      left => 0,
      sort => False,
      top => 0,
      width => 10);
   GWindows.List_Boxes.Dock (Window.Controls_List, GWindows.Base.Fill);
   GWindows.Windows.On_Create (GWindows.Windows.Window_Type (Window));
end On_Create;
