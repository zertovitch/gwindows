with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial15 is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;
   X_Label     : Label_Type;
   Y_Label     : Label_Type;

   procedure Do_Mouse_Move
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Text (X_Label, GWindows.GStrings.Image (X));
      Text (Y_Label, GWindows.GStrings.Image (Y));
   end Do_Mouse_Move;

   procedure Do_Left_Mouse_Button_Down
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Capture_Mouse (Main_Window);
   end Do_Left_Mouse_Button_Down;

   procedure Do_Left_Mouse_Button_Up
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      GWindows.Base.Release_Mouse;
   end Do_Left_Mouse_Button_Up;

begin
   Create (Main_Window, "Mouse Demo Window", Width => 200, Height => 200);
   Visible (Main_Window, True);

   Create (X_Label, Main_Window, "0", 10, 10, 40, 25, Center);
   Create (Y_Label, Main_Window, "0", 60, 10, 40, 25, Center);

   On_Mouse_Move_Handler (Main_Window, Do_Mouse_Move'Unrestricted_Access);
   On_Left_Mouse_Button_Down_Handler (Main_Window,
                               Do_Left_Mouse_Button_Down'Unrestricted_Access);
   On_Left_Mouse_Button_Up_Handler (Main_Window,
                             Do_Left_Mouse_Button_Up'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Tutorial15;
