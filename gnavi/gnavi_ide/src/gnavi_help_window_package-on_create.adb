with GWindows.Application;
with GWindows.Drawing_Objects;

--  GNAVI: No user changes may be made here

separate (GNAVI_Help_Window_Package)
procedure On_Create (Window : in out GNAVI_Help_Window_Type)
is
begin
   Width (Window, 500);
   Height (Window, 400);
   Set_Standard_Font (Window, GWindows.Drawing_Objects.Default_GUI);
   Text (Window, "GNAVI Help");
On_Create_Handler (Window, Do_Create'Access);
On_Close_Handler (Window, Do_Close'Access);
   GWindows.ActiveX.Create (Window.html_browser,
      Parent => Window,
      progid => "Shell.Explorer",
      left => 0,
      top => 0,
      width => 1,
      height => 1);
   GWindows.ActiveX.Dock (Window.html_browser, GWindows.Base.Fill);
   GWindows.Windows.On_Create (GWindows.Windows.Window_Type (Window));
end On_Create;
