with GWindows.Common_Dialogs; use GWindows.Common_Dialogs;
with GWindows.GStrings; use GWindows.Gstrings;
with GWindows.GStrings.IO; use GWindows.GStrings.IO;
with GNATCOM.Initialize;

procedure GetDir is
   Dir_Name : GWindows.GString_Unbounded;
   Dir_Path : GWindows.GString_Unbounded;
begin
   GNATCOM.Initialize.Initialize_COM;

   Get_Directory ("Please choose a directory", Dir_Name, Dir_Path);
   Put_Line ("Dir_Name : " & To_GString_From_Unbounded (Dir_Name));
   Put_Line ("Dir_Path : " & To_GString_From_Unbounded (Dir_Path));
end GetDir;
