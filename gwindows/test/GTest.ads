with GWindows;

package GTest is

   --  Test functions

   procedure Start_Test (Module_Name : String);

   procedure Put_Passed;

   procedure Put_Failed;

   procedure Put_Failed (Test_Name : String);

   procedure End_Test;

   --  String output functions

   procedure Put_Line (Value : GWindows.GString);

end GTest;
