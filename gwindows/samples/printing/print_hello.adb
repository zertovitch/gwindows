with Interfaces.C;

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Common_Dialogs; use GWindows.Common_Dialogs;
with GWindows.Base;
with GWindows.Message_Boxes;
with GWindows.Application;

procedure Print_Hello is

   procedure Do_Print (Window : in out GWindows.Base.Base_Window_Type'Class) is
      Canvas    : GWindows.Drawing.Printer_Canvas_Type;
      Settings  : GWindows.Common_Dialogs.DEVMODE;
      Flags     : Interfaces.C.unsigned := 0;
      From_Page : Natural := 1;
      To_Page   : Natural := 1;
      Copies    : Natural := 1;
      Success   : Boolean;
   begin
      Choose_Printer (Window, Canvas, Settings, Flags,
                      From_Page, To_Page, 1, 1, Copies, Success);

      --  Choose_Default_Printer (Canvas, Settings, Success);

      if Success then
         Start_Document (Canvas, "Print Hello Document");
         Start_Page (Canvas);

         Put (Canvas, 100, 100, "Hello World!");

         End_Page (Canvas);
         End_Document (Canvas);
         GWindows.Message_Boxes.Message_Box ("Print Hello", "Done");
      else
         GWindows.Message_Boxes.Message_Box ("Print Hello",
                                             "Error - Unable to print");
      end if;
   end Do_Print;

   Top    : GWindows.Windows.Main.Main_Window_Type;
   Button : GWindows.Buttons.Button_Type;
begin

   Create (Top, "Print Hello");
   Size (Top, 300, 100);
   Visible (Top);

   Create (Button, Top, "Print", 10, 10, 75, 40);
   On_Click_Handler (Button, Do_Print'Unrestricted_Access);

   GWindows.Application.Message_Loop;

end Print_Hello;
