with GWindows.Application;
with GWen_Windows;

procedure GWenerator is
  --  pragma Linker_Options ("-mwindows");
  main_window: GWen_Windows.GWen_Window_Type;
begin
  main_window.Create("GWenerator - starting...");
  GWindows.Application.Message_Loop;
end GWenerator;
