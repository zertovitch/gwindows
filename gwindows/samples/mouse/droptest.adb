--  Demo for dragging files from Explorer onto our window.

with GWindows.Application;
with GWindows.Base;
with GWindows.Events;
with GWindows.GStrings;
with GWindows.Message_Boxes;
with GWindows.Windows;

procedure DropTest is
   Window : GWindows.Windows.Window_Type;

   procedure Do_File_Drop
     (Window     : in out GWindows.Base.Base_Window_Type'Class;
      File_Names : in     GWindows.Windows.Array_Of_File_Names)
   is
   begin
      --  Attempts to bring the window on the foreground.
      Window.Set_Active_Window;
      Window.Focus;
      --
      for N in File_Names'Range loop
         GWindows.Message_Boxes.Message_Box
           (Window,
            "Files dropped",
            "File name: " &
            GWindows.GStrings.To_GString_From_Unbounded (File_Names (N)),
            Icon => GWindows.Message_Boxes.Information_Icon);
      end loop;
   end Do_File_Drop;

begin
   Window.Create ("Drop Files (from Explorer) on Me !");

   Window.On_Destroy_Handler (GWindows.Events.Do_End_Application'Access);

   Window.On_File_Drop_Handler (Do_File_Drop'Unrestricted_Access);
   Window.Accept_File_Drag_And_Drop;

   Window.Show;

   GWindows.Application.Message_Loop;
end DropTest;
