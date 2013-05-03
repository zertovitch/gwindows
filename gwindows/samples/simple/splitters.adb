with AnotherWindow_Package;
with GWindows.Application;

procedure Splitters is
--   pragma Linker_Options ("-mwindows");

begin
   AnotherWindow_Package.Show (AnotherWindow_Package.AnotherWindow);
   GWindows.Application.Message_Loop;
end Splitters;
