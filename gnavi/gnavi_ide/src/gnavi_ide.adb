--  with GNAVI_File_Edit_Window_Package;
--  with GNAVI_Help_Window_Package;
--  with GNAVI_Controls_Window_Package;
--  with GNAVI_Edit_Window_Package;
--  with GNAVI_New_Project_Package;
--  with GNAVI_New_Window_Package;

with GNAVI_Main_Package;
with GNAVI_Project_Window_Package;

with GNAVI_Project;
--  with GNAVI_Datastore;

with GWindows.Application;
with GWindows.Message_Boxes;
with GWindows.GStrings;

with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ada.Command_Line;
with Ada.Exceptions;

procedure GNAVI_IDE is

   use Ada.Command_Line, GNAVI_Project,
       GWindows.GStrings, GWindows.Message_Boxes;

   --  pragma Linker_Options ("-mwindows");
   --  pragma Linker_Options ("gnavi_ide.coff");
begin

   GNATMAKE_Path_Exists := GNAT.OS_Lib.Locate_Exec_On_Path ("gnatmake");

   if GNATMAKE_Path_Exists = null then
      Message_Box
        ("ALERT",
         "gnatmake must be on your path. Please correct and restart.",
         Icon => Stop_Icon);
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   GNAVI_Main_Package.GNAVI_Main.Show;
   GNAVI_Main_Package.GNAVI_Main.Focus;

   case Argument_Count is
      when 0 =>
         null;
      when 1 =>
         GNAVI_Project_Window_Package.Load_Project
            (To_GString_From_String (Argument (1)));
      when others =>
         Message_Box
            (GNAVI_Main_Package.GNAVI_Main,
             "Startup file",
             "Cannot start application with more than one project.",
             OK_Box,
             Error_Icon);
   end case;
   GWindows.Application.Message_Loop;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end GNAVI_IDE;
