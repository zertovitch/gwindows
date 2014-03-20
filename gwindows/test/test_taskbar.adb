with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Application; use GWindows.Application;
with GWindows.Taskbar; use GWindows.Taskbar;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;

procedure Test_Taskbar is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;
   TL : Taskbar_List;

begin
   Create (Main_Window, "Some Window...");
   Visible (Main_Window, True);
   TL.Set_Progress_State (Main_Window, Normal);
   for i in 1 .. 100 loop
     TL.Set_Progress_Value (Main_Window, i, 100);
     Message_Check;
     delay 0.08;
   end loop;
   TL.Set_Progress_State (Main_Window, Paused);
   delay 1.0;
   TL.Set_Progress_State (Main_Window, Error);
   delay 1.0;
   TL.Set_Progress_State (Main_Window, Indeterminate);
   Message_Loop; -- Just wait till the window is closed
exception
   when Taskbar_Interface_Not_Supported =>
      Message_Box (
         "Taskbar interface",
         "Cannot reach the taskbar." &
         "Probably the Windows version is prior to Windows 7",
         Icon => Error_Icon
      );
end Test_Taskbar;
