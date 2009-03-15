with Interfaces.C;

with GTest;

with GWindows.Application;

procedure Test_Application is
   pragma Linker_Options ("test_application.coff");

   use type Interfaces.C.long;
begin
   GTest.Start_Test ("Application");

   if GWindows.Application.hInstance = 0 then
      GTest.Put_Failed ("hInstance");
   end if;

   if GWindows.Application.Load_String (101) /= "Test String" then
      GTest.Put_Failed ("Load_String");
   end if;

   GTest.End_Test;
end Test_Application;
