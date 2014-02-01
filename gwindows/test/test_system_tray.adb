with GWindows.System_Tray;        use GWindows.System_Tray;

with GWindows;                    use GWindows;
with GWindows.Drawing_Objects;    use GWindows.Drawing_Objects;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
--  with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Windows;            use GWindows.Windows;

procedure Test_System_Tray is
   some_window : Window_Type;
   d : Notify_Icon_Data;
   i : Icon_Type;
begin
   Create (some_window, "Just a window...");
   Set_Window (d, some_window);
   Load_Stock_Icon (i, IDI_INFORMATION);
   Set_Icon (d, i);
   Notify_Icon (d, Add);
   Message_Box (
      some_window,
      "Hello",
      "Look at the system tray (notification area, bottom right) !"
   );
   Notify_Icon (d, Delete);
end Test_System_Tray;
