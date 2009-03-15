with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Application;

procedure Tutorial2 is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;
begin
   Create (Main_Window, "My First Window");
   Visible (Main_Window, True);

   GWindows.Application.Message_Loop;
end Tutorial2;
