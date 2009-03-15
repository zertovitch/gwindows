with Ada.Exceptions;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Utility;

with GNAT.OS_Lib;

with Tcl_Main;

procedure Tcl_Example is
begin
   Initialize_COM;

   Tcl_Main.Go;

exception
   when E : others =>
      GNATCOM.Utility.Message_Box ("TCL Control Example",
                                   Ada.Exceptions.Exception_Name (E) & " : " &
                                   Ada.Exceptions.Exception_Message (E));
      GNAT.OS_Lib.OS_Exit (1);
end Tcl_Example;
