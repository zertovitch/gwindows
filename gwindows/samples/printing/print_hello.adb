with Interfaces.C;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons;
with GWindows.Drawing;
with GWindows.Common_Dialogs;
with GWindows.Message_Boxes;
with GWindows.Windows.Main;

procedure Print_Hello is

   procedure Do_Print (Window : in out GWindows.Base.Base_Window_Type'Class) is
      First_Page : constant := 1;  --  In this demo.
      Last_Page  : constant := 2;  --  In this demo.
      --
      Canvas     : GWindows.Drawing.Printer_Canvas_Type;
      Settings   : GWindows.Common_Dialogs.DEVMODE;
      Flags      : Interfaces.C.unsigned := 0;
      From_Page  : Natural := First_Page;
      To_Page    : Natural := Last_Page;
      Copies     : Natural := 1;
      Success    : Boolean;
      use GWindows.Common_Dialogs, GWindows.Drawing;
   begin
      Choose_Printer (Window, Canvas, Settings, Flags,
                      From_Page, To_Page,
                      First_Page, Last_Page,
                      Copies, Success);

      --  Choose_Default_Printer (Canvas, Settings, Success);

      if Success then
         Start_Document (Canvas, "Print Hello Document");
         --
         if From_Page = First_Page then
            Start_Page (Canvas);
            Put (Canvas, 100, 100, "Hello World!");
            End_Page (Canvas);
         end if;
         --
         if To_Page = Last_Page then
            Start_Page (Canvas);
            Put (Canvas, 100, 100, "Now, the second page...");
            End_Page (Canvas);
         end if;
         --
         End_Document (Canvas);
         GWindows.Message_Boxes.Message_Box ("Print Hello", "Done");
      else
         GWindows.Message_Boxes.Message_Box ("Print Hello",
                                             "Error - Unable to print");
      end if;
   end Do_Print;

   use GWindows.Windows.Main;
   Top : Main_Window_Type;

   use GWindows.Buttons;
   Button : Button_Type;
begin

   Create (Top, "Print Hello");
   Size (Top, 500, 200);
   Visible (Top);

   Create (Button, Top, "Print", 10, 10, 75, 40);
   On_Click_Handler (Button, Do_Print'Unrestricted_Access);

   GWindows.Application.Message_Loop;

end Print_Hello;
