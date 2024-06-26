--  This file contains only automatically generated code.
--  Do not edit!
--  Change the GNAVI item instead.

with GWindows.Application;
with GWindows.Drawing_Objects;

separate (GNAVI_Main_Package)
procedure On_Create (Window : in out GNAVI_Main_Type)
is
begin
On_Create_Handler (Window, Do_Create'Access);
On_Menu_Select_Handler (Window, Handle_Menu'Access);
On_Menu_Hover_Handler (Window, Check_Menu'Access);
   Text (Window, "GNAVI - Open Source Visual RAD");
   Visible (Window, True);
   Set_Standard_Font (Window, GWindows.Drawing_Objects.Default_GUI);
   GWindows.Common_Controls.Create (Window.Top_Tools,
      Parent => Window,
      top => 0,
      left => 0,
      width => 1,
      height => 1);
   GWindows.Common_Controls.Dock (Window.Top_Tools, GWindows.Base.At_Top);
   GWindows.Common_Controls.On_Button_Select_Handler (Window.Top_Tools, Do_Toolbar_Select'Access);
   GWindows.Common_Controls.Create (Window.Bottom_Status,
      Parent => Window,
      text => "Ready");
   GWindows.Common_Controls.Dock (Window.Bottom_Status, GWindows.Base.At_Bottom);
   GWindows.Windows.MDI.On_Create (GWindows.Windows.MDI.MDI_Main_Window_Type (Window));
end On_Create;
