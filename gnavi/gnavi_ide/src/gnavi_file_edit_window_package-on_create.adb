--  This file contains only automatically generated code.
--  Do not edit!
--  Change the GNAVI item instead.

with GWindows.Application;
with GWindows.Drawing_Objects;

separate (GNAVI_File_Edit_Window_Package)
procedure On_Create (Window : in out GNAVI_File_Edit_Window_Type)
is
begin
   Text (Window, "GNAVI_File_Edit_Window");
   Visible (Window, True);
   Width (Window, 500);
   Height (Window, 400);
   GWindows.Scintilla.Create (Window.Edit_Box,
      Parent => Window,
      height => 30,
      id => 0,
      left => 0,
      top => 0,
      width => 60);
   GWindows.Scintilla.Dock (Window.Edit_Box, GWindows.Base.Fill);
On_Menu_Hover_Handler (Window, Check_Menu'Access);
On_Menu_Select_Handler (Window, Handle_Menu'Access);
On_Create_Handler (Window, Do_Create'Access);
   GWindows.Windows.MDI.On_Create (GWindows.Windows.MDI.MDI_Child_Window_Type (Window));
end On_Create;
