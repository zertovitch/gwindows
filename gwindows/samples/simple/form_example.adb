with Ada.Exceptions;
with GNAT.OS_Lib;

with GWindows.Application;
with GWindows.Message_Boxes;
with GWindows.GStrings; use GWindows.GStrings;

with Form_Main; use Form_Main;

procedure Form_Example is
   pragma Linker_Options ("form_example.coff");

   Top : Form_Main_Type;
begin
   Create (Top, "Form Example");

   GWindows.Application.Message_Loop;
exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("Form Example",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end Form_Example;
