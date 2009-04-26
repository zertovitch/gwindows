with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Scroll_Panels; use GWindows.Scroll_Panels;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Message_Boxes;
with GWindows.Events;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial6 is
   pragma Linker_Options ("-mwindows");

begin
   --  Using a Scroll Panel as a Window

   declare
      Main_Window : GWindows.Scroll_Panels.Scroll_Panel_Type;
      User_Name   : GWindows.Edit_Boxes.Edit_Box_Type;
      Disp_Button : GWindows.Buttons.Button_Type;

      procedure Do_Display
        (Window    : in out GWindows.Base.Base_Window_Type'Class)
      is
         pragma Unreferenced (Window);
      begin
         GWindows.Message_Boxes.Message_Box
           ("Scroll Window", Text (User_Name));
      end Do_Display;
   begin
      Create (Main_Window, "Scroll Window", Width => 200, Height => 150);
      Visible (Main_Window, True);
      On_Destroy_Handler (Main_Window,
                          GWindows.Events.Do_End_Application'Access);
      On_Destroy_Handler (Main_Window,
                          GWindows.Events.Do_End_Application'Access);
      --  Since Scroll_Panel_Type is not derived from Main_Window_Type
      --  it will not automaticly close the application when the window
      --  is destroyed. This handler will do that for us.

      Panel_Size (Main_Window, 500, 500);

      Keyboard_Support (Main_Window.Panel, True);

      Create_Label (Main_Window.Panel, "Name :", 150, 10, 50, 25);

      Create (User_Name, Main_Window.Panel, "", 230, 10, 100, 25);

      Create (Disp_Button, Main_Window.Panel, "&Display", 150, 50, 75, 30);
      On_Click_Handler (Disp_Button, Do_Display'Unrestricted_Access);

      Focus (User_Name);

      GWindows.Application.Message_Loop;
   end;

   --  Using a Scroll Panel as a control

   declare
      Main_Window  : GWindows.Windows.Main.Main_Window_Type;
      Scroll_Panel : GWindows.Scroll_Panels.Scroll_Panel_Type;
      User_Name   : GWindows.Edit_Boxes.Edit_Box_Type;
      Disp_Button : GWindows.Buttons.Button_Type;

      procedure Do_Display
        (Window    : in out GWindows.Base.Base_Window_Type'Class)
      is
         pragma Unreferenced (Window);
      begin
         GWindows.Message_Boxes.Message_Box
           ("Scroll Window", Text (User_Name));
      end Do_Display;
   begin
      Create (Main_Window, "Scrolling Window 2", Width => 400, Height => 400);
      Visible (Main_Window, True);

      Create_As_Control (Scroll_Panel, Main_Window,
                         Top    => 20,
                         Left   => 20,
                         Width  => 300,
                         Height => 300);

      Panel_Size (Scroll_Panel, 500, 500);

      Keyboard_Support (Scroll_Panel.Panel, True);

      Create_Label (Scroll_Panel.Panel, "Name :", 150, 10, 50, 25);

      Create (User_Name, Scroll_Panel.Panel, "", 230, 10, 100, 25);

      Create (Disp_Button, Scroll_Panel.Panel, "&Display", 150, 50, 75, 30);
      On_Click_Handler (Disp_Button, Do_Display'Unrestricted_Access);

      Focus (User_Name);

      GWindows.Application.Message_Loop;
   end;

end Tutorial6;
