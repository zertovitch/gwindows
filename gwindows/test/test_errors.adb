with GTest;

with GWindows.Errors,
     GWindows.GStrings.IO;

procedure Test_Errors is
   use GWindows.GStrings, GWindows.GStrings.IO;
begin
   GTest.Start_Test ("Errors");

   for I in 0 .. 9 loop
      Put (
         "   Windows error code" &
         GWindows.GStrings.To_GString_From_String (Integer'Image (I)) &
         " is:     " & GWindows.Errors.Error_To_String (I)
      );
   end loop;

   begin
      GWindows.Errors.Error_Check (-1);  --  Failure
      GTest.Put_Failed ("Error_Check");
   exception
      when GWindows.Errors.Win32_Error =>
         Put_Line ("?? Last Error String : " & GWindows.Errors.Get_Last_Error);
   end;

   GTest.End_Test;
end Test_Errors;
