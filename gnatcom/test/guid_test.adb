with GNATCOM.Types;
with GNATCOM.Initialize;
with GNATCOM.GUID;
use GNATCOM;

with GNAT.IO; use GNAT.IO;

procedure GUID_Test is
   pragma Linker_Options ("-lole32");

   use type Types.GUID;

   Test_GUID : Types.GUID;
begin
   Put_Line ("-- Start GUID_Test");
   New_Line;

   Initialize.Initialize_COM;

   Put_Line ("Test   : Create a GUID");
   Test_GUID := GUID.Create_GUID;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Convert GUID to String");
   declare
      Test_String : String := GUID.To_String (Test_GUID);
   begin
      Put_Line ("++ PASS");
      New_Line;

      Put_Line ("Test   : GUID -> String -> GUID again");
      Put_Line ("Expect : Conversion of Test String to GUID = Orig. GUID");
      if GUID.To_GUID (Test_String) = Test_GUID then
         Put_Line ("++ PASS");
         New_Line;
      else
         Put_Line ("** FAIL : GUIDs do not match");
      end if;
   end;

   Put_Line ("-- Completed GUID_Test");
   New_Line;

end GUID_Test;


