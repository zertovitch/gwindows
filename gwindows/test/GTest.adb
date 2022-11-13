with Ada.Strings.Unbounded;

with GNAT.IO;
with GWindows.GStrings;

package body GTest is

   use Ada.Strings.Unbounded;

   Module_Name : Unbounded_String;

   --------------
   -- End_Test --
   --------------

   procedure End_Test is
   begin
       GNAT.IO.Put_Line
          ("++ Test completed for module : " & To_String (Module_Name));
   end End_Test;

   ----------------
   -- Put_Failed --
   ----------------

   procedure Put_Failed is
   begin
      GNAT.IO.Put_Line
         ("-- Test FAILED for module : " & To_String (Module_Name));
   end Put_Failed;

   ----------------
   -- Put_Failed --
   ----------------

   procedure Put_Failed (Test_Name : String)
   is
   begin
      GNAT.IO.Put_Line
        ("-- Test " & Test_Name &
         " FAILED in module : " & To_String (Module_Name));
   end Put_Failed;

   ----------------
   -- Put_Passed --
   ----------------

   procedure Put_Passed is
   begin
      GNAT.IO.Put_Line
         ("++ Test PASSED for module : " & To_String (Module_Name));
   end Put_Passed;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (Module_Name : String) is
   begin
      GTest.Module_Name := To_Unbounded_String (Module_Name);
      GNAT.IO.Put_Line ("++ Test started for module : " &
                        To_String (GTest.Module_Name));
   end Start_Test;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Value : GWindows.GString)
   is
   begin
      GNAT.IO.Put_Line (GWindows.GStrings.To_String (Value));
   end Put_Line;

end GTest;
