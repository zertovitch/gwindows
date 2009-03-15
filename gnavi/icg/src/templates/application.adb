with GWindows.Application;

procedure @_Application_Name_@ is
   pragma Linker_Options ("-mwindows");

begin
   GWindows.Application.Message_Loop;
end @_Application_Name_@;
