with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial14 is
   pragma Linker_Options ("-mwindows");

   Window1     : Main_Window_Type;
   Window2     : Main_Window_Type;
   Jump_Button : Button_Type;

   procedure Do_Click (Window : in out GWindows.Base.Base_Window_Type'Class) is
      use GWindows.Base;
   begin
      if Text (Parent (Window).all) = "Window 1" then
         Parent (Window, Window2);
      else
         Parent (Window, Window1);
      end if;
   end Do_Click;

begin
   Create (Window1, "Window 1", Width => 100, Height => 100);
   Visible (Window1, True);

   Create (Window2, "Window 2",
           Top   => Top (Window1),
           Left  => Left (Window1) + 200,
           Width => 100, Height => 100);
   Visible (Window2, True);

   Create (Jump_Button, Window1, "Jump!", 10, 10,
           Client_Area_Width (Window1) - 20,
           Client_Area_Height (Window1) - 20);
   On_Click_Handler (Jump_Button, Do_Click'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Tutorial14;
