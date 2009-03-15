with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Message_Boxes;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial5 is
   pragma Linker_Options ("-mwindows");
   
   Main_Window : GWindows.Windows.Main.Main_Window_Type;
   User_Name   : GWindows.Edit_Boxes.Edit_Box_Type;
   Disp_Button : GWindows.Buttons.Button_Type;
   
   procedure Do_Display
     (Window    : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Message_Boxes.Message_Box ("Controls Window", Text (User_Name));
   end Do_Display;
   
begin
   Create (Main_Window, "Controls Window", Width => 200, Height => 125);
   Visible (Main_Window, True);
   Keyboard_Support (Main_Window, True);
   
   Create_Label (Main_Window, "Name :", 10, 10, 50, 25);
   
   Create (User_Name, Main_Window, "", 70, 10, 100, 25);
   
   Create (Disp_Button, Main_Window, "&Display", 10, 50, 75, 30);
   On_Click_Handler (Disp_Button, Do_Display'Unrestricted_Access);
   
   GWindows.Application.Message_Loop;
end Tutorial5;
