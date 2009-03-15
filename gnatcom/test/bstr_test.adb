with Interfaces.C;
use Interfaces;

with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Types;
with GNATCOM.BSTR;
use GNATCOM;

procedure BSTR_Test is

   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   A_Null_BSTR : Types.BSTR := null;
   A_Zero_Len  : Types.BSTR;
   A_BSTR      : Types.BSTR;

   Test_String      : constant String := "Hello World";
   Test_String_Wide : constant Wide_String := "Hello World";

begin
   Initialize_COM;

   Put_Line ("-- Start BSTR_Test");
   New_Line;

   Put_Line ("Test   : Create a zero length BSTR From String");
   A_Zero_Len := BSTR.To_BSTR ("");
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Free a zero length BSTR");
   BSTR.Free (A_Zero_Len);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Create a zero length BSTR From Wide_String");
   A_Zero_Len := BSTR.To_BSTR_From_Wide ("");
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Is_Empty of a zero length BSTR");
   Put_Line ("Expect : Is_Empty = True");
   if BSTR.Is_Empty (A_Zero_Len) then
      Put_Line ("++ PASS");
   else
      Put_Line ("** FAIL : Is_Empty = False");
   end if;
   New_Line;

   Put_Line ("Test   : Is_Empty of Blank BSTR");
   Put_Line ("Expect : Is_Empty = True");
   if BSTR.Is_Empty (A_Null_BSTR) then
      Put_Line ("++ PASS");
   else
      Put_Line ("** FAIL : Is_Empty = False");
   end if;
   New_Line;

   Put_Line ("Test   : Create a BSTR From a String");
   A_BSTR := BSTR.To_BSTR (Test_String);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Test Length function");
   declare
      Size : Natural := BSTR.Length (A_BSTR);
   begin
      if Size /= Test_String'Length then
         Put_Line ("** FAIL : Length return incorrect size of BSTR");
      else
         Put_Line ("++ PASS");
      end if;
   end;
   New_Line;

   Put_Line ("Test   : Create an Ada String from a BSTR");
   declare
      A_String : String := BSTR.To_Ada (From => A_BSTR,
                                        Free => True);
   begin
      if A_String = Test_String then
         Put_Line ("++ PASS");
         New_Line;
      else
         Put_Line ("++ FAIL");
         New_Line;
      end if;
   end;

   Put_Line ("Test   : Create a BSTR From a Wide_String");
   A_BSTR := BSTR.To_BSTR_From_Wide (Test_String_Wide);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Create an Ada Wide_String from a BSTR");
   declare
      A_String : Wide_String := BSTR.To_Ada_Wide (From => A_BSTR,
                                                  Free => True);
   begin
      if A_String = Test_String_Wide then
         Put_Line ("++ PASS");
         New_Line;
      else
         Put_Line ("++ FAIL");
         New_Line;
      end if;
   end;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Create a BSTR From a char_array");
   A_BSTR := BSTR.To_BSTR_From_C (C.To_C (Test_String));
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Create a char_array from a BSTR");
   declare
      use type Interfaces.C.char_array;

      A_String : C.char_array := BSTR.To_C (From => A_BSTR,
                                            Free => True);
   begin
      if A_String = C.To_C (Test_String) then
         Put_Line ("++ PASS");
         New_Line;
      else
         Put_Line ("++ FAIL");
         New_Line;
      end if;
   end;

   Put_Line ("Test   : Create a BSTR From a wchar_array");
   A_BSTR := BSTR.To_BSTR_From_Wide_C (C.To_C (Test_String_Wide));
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Create a wchar_array from a BSTR");
   declare
      use type Interfaces.C.wchar_array;

      A_String : C.wchar_array := BSTR.To_C_Wide (From => A_BSTR,
                                                  Free => True);
   begin
      if A_String = C.To_C (Test_String_Wide) then
         Put_Line ("++ PASS");
         New_Line;
      else
         Put_Line ("++ FAIL");
         New_Line;
      end if;
   end;

   Put_Line ("Test   : Create an Ada String from a Zero Length BSTR");
   declare
      A_String : String := BSTR.To_Ada (From => A_Zero_Len,
                                        Free => False);
   begin
      if A_String = "" then
         Put_Line ("++ PASS");
      else
         Put_Line ("** FAIL");
      end if;
   end;
   New_Line;

   Put_Line ("Test   : Create an Ada Wide String from a Zero Length BSTR");
   declare
      A_String : Wide_String := BSTR.To_Ada_Wide (From => A_Zero_Len,
                                                  Free => False);
   begin
      if A_String = "" then
         Put_Line ("++ PASS");
      else
         Put_Line ("** FAIL");
      end if;
   end;
   New_Line;

   Put_Line ("Test   : Create an Ada String from a null BSTR");
   declare
      A_String : String := BSTR.To_Ada (From => A_Null_BSTR,
                                        Free => False);
   begin
      if A_String = "" then
         Put_Line ("++ PASS");
      else
         Put_Line ("** FAIL");
      end if;
   end;
   New_Line;

   Put_Line ("Test   : Create an Ada Wide String from a Zero Length BSTR");
   declare
      A_String : Wide_String := BSTR.To_Ada_Wide (From => A_Null_BSTR,
                                                  Free => False);
   begin
      if A_String = "" then
         Put_Line ("++ PASS");
      else
         Put_Line ("** FAIL");
      end if;
   end;
   New_Line;

   Put_Line ("Test   : Free a null BSTR");
   BSTR.Free (A_Null_BSTR);
   Put_Line ("++ PASS");
   New_Line;

   BSTR.Free (A_Zero_Len);

   Put_Line ("-- Completed BSTR_Test");
   New_Line;

end BSTR_Test;
