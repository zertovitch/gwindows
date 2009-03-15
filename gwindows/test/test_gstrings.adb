with GTest;

with GWindows.GStrings;

procedure Test_GStrings is
begin
   GTest.Start_Test ("GStrings");

   declare

      Test_String : constant GWindows.GString := "This is a test string";

      C_String : constant GWindows.GString_C :=
        GWindows.GStrings.To_GString_C (Test_String);
   begin
      if GWindows.GStrings.To_GString_From_C (C_String) /= Test_String then
         GTest.Put_Failed ("To_GString & To_GString_C");
      end if;

      if GWindows.GStrings.To_String (GWindows.GString'("A String")) /=
        String'("A String")
      then
         GTest.Put_Failed ("To_String");
      end if;

      if GWindows.GStrings.To_Wide_String (GWindows.GString'("A String")) /=
        Wide_String'("A String")
      then
         GTest.Put_Failed ("To_Wide_String");
      end if;

      if
        GWindows.GStrings.To_GString_From_String
          (String'("This is a test string")) /=
        Test_String
      then
         GTest.Put_Failed ("To_GString_From_String");
      end if;

      if
        GWindows.GStrings.To_GString_From_Wide_String
          (Wide_String'("This is a test string")) /=
        Test_String
      then
         GTest.Put_Failed ("To_GString_From_String");
      end if;
   end;

   GTest.End_Test;
end Test_GStrings;
