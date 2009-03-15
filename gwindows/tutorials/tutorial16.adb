with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Cursors;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial16 is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;

   procedure Do_Change_Cursor
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Cursors;
   begin
      Set_Cursor (Load_System_Cursor (IDC_CROSS));
   end Do_Change_Cursor;

begin
   Create (Main_Window, "Cursor Window", Width => 200, Height => 200);
   Visible (Main_Window, True);
   On_Change_Cursor_Handler (Main_Window,
                             Do_Change_Cursor'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Tutorial16;
