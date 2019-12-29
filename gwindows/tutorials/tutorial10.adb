with Interfaces.C;

with GWindows.Common_Dialogs; use GWindows.Common_Dialogs;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.GStrings; use GWindows.GStrings;
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
   Fake_Win  : GWindows.Windows.Window_Type;
   use GWindows;
begin
   GWindows.Windows.Create (Fake_Win, "Fake", 100, 100, 20, 20);

   Choose_Printer (Fake_Win, Canvas, Settings, Flags,
                   From_Page, To_Page, 1, 1, Copies, Success);

   if Success then
      Start_Document (Canvas, "Tutorial 10 Document");
      Start_Page (Canvas);

      Put (Canvas, 100, 100, "Hello World!");

      Ellipse (Canvas, 200, 200, 500, 500);

      End_Page (Canvas);
      End_Document (Canvas);

      GWindows.Message_Boxes.Message_Box (
        "Tutorial 10",
        "Done. " & GCharacter'Val (10) &
        --  TBD: fix Device_Name and Form_Name in that setting,
        --       they return garbage.
        --  "Output Device: " & Device_Name (Settings) & GCharacter'Val (10) &
        --  "Form Name: " & Form_Name (Settings) & GCharacter'Val (10) &
        "Copies: " & To_GString_From_String (Integer'Image (Copies))
        --  TBD: make Copies effective.
      );
   else
      GWindows.Message_Boxes.Message_Box (
        "Tutorial 10",
        "Printing Canceled"
      );
   end if;
end Tutorial10;
