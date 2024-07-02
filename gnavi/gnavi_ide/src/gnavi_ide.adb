pragma Warnings (Off);

with GNAVI_File_Edit_Window_Package;
with GNAVI_Help_Window_Package;
with GNAVI_Controls_Window_Package;
with GNAVI_Project_Window_Package;
with GNAVI_Edit_Window_Package;
with GNAVI_New_Project_Package;
with GNAVI_New_Window_Package;
with GNAVI_Main_Package;

with GNAVI_Project;
with GNAVI_Datastore;

with GWindows.Application;
with GWindows.Message_Boxes;

with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ada.Exceptions;

procedure GNAVI_IDE is

   use GNAVI_Project;

   --  pragma Linker_Options ("-mwindows");
   --  pragma Linker_Options ("gnavi_ide.coff");
begin

   ICG_Path_Exists := GNAT.OS_Lib.Locate_Exec_On_Path ("icg");
   GNATMAKE_Path_Exists := GNAT.OS_Lib.Locate_Exec_On_Path ("gnatmake");

   if ICG_Path_Exists = null then
      GWindows.Message_Boxes.Message_Box
        ("ALERT",
         "icg.exe must be on your path. Please correct and restart.",
         Icon => GWindows.Message_Boxes.Stop_Icon);
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if GNATMAKE_Path_Exists = null then
      GWindows.Message_Boxes.Message_Box
        ("ALERT",
         "gnatmake must be on your path. Please correct and restart.",
         Icon => GWindows.Message_Boxes.Stop_Icon);
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   GNAVI_Main_Package.Show (GNAVI_Main_Package.GNAVI_Main);
   GNAVI_Main_Package.Focus (GNAVI_Main_Package.GNAVI_Main);
   GWindows.Application.Message_Loop;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end GNAVI_IDE;
