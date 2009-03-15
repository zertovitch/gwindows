with GTest;

with GWindows.Errors;
with GWindows.GStrings.IO; use GWindows.GStrings.IO;

procedure Test_Errors is
begin
   GTest.Start_Test ("Errors");

   begin
      GWindows.Errors.Error_Check (-1); -- Failure
      GTest.Put_Failed ("Error_Check");
   exception
      when GWindows.Errors.Win32_Error =>
         Put_Line ("?? Last Error String : " & GWindows.Errors.Get_Last_Error);
   end;

   GTest.End_Test;
end Test_Errors;
