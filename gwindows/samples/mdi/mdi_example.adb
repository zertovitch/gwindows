with Ada.Exceptions;
with GNAT.OS_Lib;

with GWindows.Application;
with GWindows.Message_Boxes;
with GWindows.GStrings; use GWindows.GStrings;

with MDI_Main; use MDI_Main;

procedure MDI_Example is
   pragma Linker_Options ("mdi_example.coff");

   Top : MDI_Main_Type;

begin
   Create_MDI_Top (Top, "MDI TOP");

   GWindows.Application.Message_Loop;
exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("MDI_Example",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end MDI_Example;
