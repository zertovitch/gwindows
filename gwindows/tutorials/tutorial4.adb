with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Base;
with GWindows.Application;
with GWindows.Message_Boxes;

with Tutorial4_Window; use Tutorial4_Window;

procedure Tutorial4 is
   pragma Linker_Options ("-mwindows");
begin

   declare
      My_Window   : Tutorial4_Window.My_Window_Type;
   begin
      Create (My_Window, "Event Handling Window - Version 1");
      Visible (My_Window, True);

      GWindows.Application.Message_Loop;
   end;

   declare
      Main_Window : GWindows.Windows.Main.Main_Window_Type;

      procedure Do_On_Close
        (Window    : in out GWindows.Base.Base_Window_Type'Class;
         Can_Close :    out Boolean)
      is
         use GWindows.Message_Boxes;
      begin
         Can_Close := Message_Box (Window, "Tutorial4", "Ok to close?",
                                   Yes_No_Box, Question_Icon) =  Yes;
      end Do_On_Close;

   begin
      Create (Main_Window, "Event Handling Window - Version 2");
      Visible (Main_Window, True);
      On_Close_Handler (Main_Window, Do_On_Close'Unrestricted_Access);

      GWindows.Application.Message_Loop;
   end;

end Tutorial4;
