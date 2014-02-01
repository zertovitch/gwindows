with GWindows.System_Tray;        use GWindows.System_Tray;

with GWindows;                    use GWindows;
with GWindows.Drawing_Objects;    use GWindows.Drawing_Objects;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.Windows;            use GWindows.Windows;

procedure Test_System_Tray is
   some_window : Window_Type;
   d : Notify_Icon_Data;
   i, ib : Icon_Type;
   NL : constant GCharacter := GCharacter'Val (10); -- New Line
begin
   --  Create a dummy window. We don't even show it.
   Create (some_window, "Just a window...");
   --  Preparing the system tray icon:
   Set_Window (d, some_window);
   Load_Stock_Icon (i, IDI_QUESTION); -- Icon for the system tray
   Set_Icon (d, i, 1);
   Load_Stock_Icon (ib, IDI_HAND); -- Icon for the balloons
   Set_Balloon_Icon (d, ib); -- Set user icon for the balloons
   Set_Tool_Tip (d, "Test_System_Tray" & NL & "The coolest app ever ;-)");
   Set_Balloon (d,
     "Balloon's text" & NL & "Funny, isn't it ?",
     "Balloon's title",
     Warning_Icon   --   User_Icon
   );
   --  Now the fun part:
   Notify_Icon (d, Add);
   Message_Box (some_window, "Hello", "Look at the system tray !");
   Notify_Icon (d, Delete);
end Test_System_Tray;
