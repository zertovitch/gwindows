with Ada.Exceptions;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Utility;

with GNAT.OS_Lib;

with Browser;

procedure NT_gnatfind is
   pragma Linker_Options ("nt_gnatfind.coff");
begin

   Initialize_COM;

   Browser.Go;

exception
   when E : others =>
      GNATCOM.Utility.Message_Box ("GNATFIND",
                                   Ada.Exceptions.Exception_Name (E) & " : " &
                                   Ada.Exceptions.Exception_Message (E));
      GNAT.OS_Lib.OS_Exit (1);
end NT_gnatfind;
