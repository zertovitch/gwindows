with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Common_Controls; use GWindows.Common_Controls;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Base; use GWindows.Base;

procedure Test_up_down is
   main : GWindows.Windows.Main.Main_Window_Type;
   updown : Up_Down_Control_Type;
   button : Button_Type;

   procedure do_on_button_click (button : in out Base_Window_Type'class) is
   pragma Unreferenced (button);
   begin
      Message_Box ("pos() #1", To_GString_From_String (updown.Position'Img));
      Message_Box ("pos() #2", To_GString_From_String (updown.Position'Img));
   end do_on_button_click;

begin
   Create (main, "Test_up_down", 0, 0, 300, 200);
   Center (main);

   Create (updown, main, 50, 50, 150, 50, Send_Int => False);
   Set_Range (updown, 0, 10);
   Position (updown, 5);

   Create (button, main, "Query", 120, 50, 80, 30);
   On_Click_Handler (button, do_on_button_click'Unrestricted_Access);

   Visible (main);
   GWindows.Application.Message_Loop;

end Test_up_down;
