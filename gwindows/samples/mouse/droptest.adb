with Ada.Strings.Unbounded;

with GWindows.Windows; use GWindows.Windows;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Base;
with GWindows.Events;
with GWindows.Application;
with GWindows.Message_Boxes;

procedure DropTest is
   Window : Window_Type;

   procedure Do_File_Drop
     (Window     : in out GWindows.Base.Base_Window_Type'Class;
      File_Names : in     GWindows.Windows.Array_Of_File_Names)
   is
   begin
      for N in File_Names'Range loop
         GWindows.Message_Boxes.Message_Box
           ("Files",
            To_GString_From_Unbounded (File_Names (N)));
      end loop;
   end Do_File_Drop;

begin
   Create (Window, "Drop Files on Me!");

   On_Destroy_Handler (Window, GWindows.Events.Do_End_Application'Access);

   On_File_Drop_Handler (Window, Do_File_Drop'Unrestricted_Access);
   Accept_File_Drag_And_Drop (Window);

   Show (Window);

   GWindows.Application.Message_Loop;
end DropTest;
