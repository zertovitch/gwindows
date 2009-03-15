with Interfaces.C;

with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Common_Dialogs; use GWindows.Common_Dialogs;
with GWindows.Message_Boxes;
with GWindows.Windows;

procedure Tutorial10 is
   Canvas    : GWindows.Drawing.Printer_Canvas_Type;
   Settings  : GWindows.Common_Dialogs.DEVMODE;
   Flags     : Interfaces.C.unsigned := 0;
   From_Page : Natural := 1;
   To_Page   : Natural := 1;
   Copies    : Natural := 1;
   Success   : Boolean;
   Null_Win  : GWindows.Windows.Window_Type;
begin
   Choose_Printer (Null_Win, Canvas, Settings, Flags,
                   From_Page, To_Page, 1, 1, Copies, Success);

   if Success then
      Start_Document (Canvas, "Tutorial10 Document");
      Start_Page (Canvas);

      Put (Canvas, 100, 100, "Hello World!");

      Ellipse (Canvas, 200, 200, 500, 500);

      End_Page (Canvas);
      End_Document (Canvas);
      GWindows.Message_Boxes.Message_Box ("Tutorial 10", "Done");
   else
      GWindows.Message_Boxes.Message_Box ("Tutorial 10",
                                          "Printing Canceled");
   end if;
end Tutorial10;
