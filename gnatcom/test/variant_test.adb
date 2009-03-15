with Ada.Calendar;

with Interfaces.C;
use Interfaces;

with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Types;
with GNATCOM.VARIANT;
use GNATCOM;

procedure VARIANT_Test is
   use type Interfaces.C.unsigned_short;
   use type Interfaces.C.char_array;
   use type Types.VARIANT;
   use type Ada.Calendar.Time;

   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   Test_Var  : Types.VARIANT;
   Test_Time : Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Year    => 1999,
      Month   => 12,
      Day     => 23,
      Seconds => Ada.Calendar.Day_Duration ((14 * 60*60) + (15 * 60) + 45));
begin
   Initialize_COM;
   Put_Line ("-- Start VARIANT_Test");
   New_Line;

   Put_Line ("Test    : Initialize VARIANT");
   Put_Line ("Expect  : Test_Var.vt = VT_EMPTY");
   Test_Var.vt := Types.VT_DATE;
   VARIANT.Initialize (Test_Var);
   if Test_Var.vt = Types.VT_EMPTY then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : Test_Var.vt = " & Test_Var.vt'Img);
   end if;
   New_Line;

   Put_Line ("Test    : Create variant from Integer");
   Test_Var := VARIANT.To_VARIANT (12345);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Create string from VARIANT containing an Integer");
   if VARIANT.To_Ada (Test_Var) = "12345" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create Float from VARIANT created by an Integer");
   declare
      New_Val    : Float := VARIANT.To_Ada (Test_Var);
   begin
      if New_Val = 12345.0 then
         Put_Line ("++ PASS");
      else
         Put_Line ("++ FAIL : " & New_Val'Img);
      end if;
      New_Line;
   end;

   Put_Line ("Test    : Create variant from Float");
   Test_Var := VARIANT.To_VARIANT (12345.52);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Create string from VARIANT containing a Float");
   if VARIANT.To_Ada (Test_Var) = "12345.52" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create Integer from VARIANT created by a Float");
   Put_Line ("Expect  : Rounding of float to integer");
   declare
      New_Val    : Integer := VARIANT.To_Ada (Test_Var);
   begin
      if New_Val = 12346 then
         Put_Line ("++ PASS");
      else
         Put_Line ("++ FAIL : " & New_Val'Img);
      end if;
      New_Line;
   end;

   Put_Line ("Test    : Create copy of VARIANT");
   declare
      New_Var    : Types.VARIANT := VARIANT.Copy (Test_Var);
   begin
      if VARIANT.To_Ada (New_Var) = "12345.52" then
         Put_Line ("++ PASS");
      else
         Put_Line ("++ FAIL : " & VARIANT.To_Ada (New_Var));
      end if;
      New_Line;
   end;

   Put_Line ("Test    : Create variant from Boolean");
   Test_Var := VARIANT.To_VARIANT (True);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Test  VARIANT as Boolean created from Boolean");
   if VARIANT.To_Ada (Test_Var) = True then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL :" & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create string from VARIANT containing a Boolean");
   if VARIANT.To_Ada (Test_Var) = "-1" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create variant from String");
   Test_Var := VARIANT.To_VARIANT ("False");
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Test VARIANT created from String as Boolean");
   if VARIANT.To_Ada (Test_Var) = False then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Clear Variant");
   Put_Line ("Expect  : Test_Var.vt = VT_EMPTY");
   VARIANT.Clear (Test_Var);
   if Test_Var.vt = Types.VT_EMPTY then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : Test_Var.vt = " & Test_Var.vt'Img);
   end if;
   New_Line;

   Put_Line ("Test    : Create VARIANT from Ada.Calendar.Time");
   Test_Var := VARIANT.To_VARIANT (Test_Time);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Create Ada.Calendar.Time from VARIANT");
   Put_Line ("Expect  : Value of Current_Time used to create VARIANT");
   if VARIANT.To_Ada (Test_Var) = Test_Time then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create VARIANT from Ada.Calendar.Time");
   Test_Var := VARIANT.To_VARIANT (Test_Time);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Create variant from empty String");
   Test_Var := VARIANT.To_VARIANT ("");
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Test empty string from VARIANT with empty string");
   if VARIANT.To_Ada (Test_Var, Clear => False) = "" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Test empty C string from VARIANT with empty string");
   if VARIANT.To_C (Test_Var) = C.To_C ("") then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Create variant wtih null BSTR");
   Test_Var := VARIANT.To_VARIANT (Types.BSTR'(null));
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test    : Test empty string from VARIANT with null BSTR");
   if VARIANT.To_Ada (Test_Var, Clear => False) = "" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Test empty C string from VARIANT with null BSTR");
   if VARIANT.To_C (Test_Var) = C.To_C ("") then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   Put_Line ("Test    : Test empty string from VARIANT_NULL");
   if VARIANT.To_Ada (Types.VARIANT_NULL) = "" then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL : " & VARIANT.To_Ada (Test_Var));
   end if;
   New_Line;

   --  Add tests for COM interfaces functions

   Put_Line ("-- Completed VARIANT_Test");
   New_Line;

end VARIANT_Test;
