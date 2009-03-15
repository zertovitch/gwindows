with Hello_Window_Package;
with GWindows.Application;

procedure Hello_World is
   pragma Linker_Options ("-mwindows");

begin
   GWindows.Application.Message_Loop;
end  Hello_World;

