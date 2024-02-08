with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Common_Dialogs,
     GWindows.Drawing,
     GWindows.Message_Boxes,
     GWindows.GStrings,
     GWindows.Windows.Main;

with Interfaces.C;

procedure Print_Hello is

   procedure Do_Print (Window : in out GWindows.Base.Base_Window_Type'Class) is
      First_Page : constant := 1;  --  In this demo.
      Last_Page  : constant := 2;  --  In this demo.
      --
      use GWindows, GWindows.Common_Dialogs,
          GWindows.Drawing, GWindows.GStrings, GWindows.Message_Boxes;
      Canvas     : Printer_Canvas_Type;
      Settings   : GWindows.Common_Dialogs.DEVMODE;
      Flags      : Interfaces.C.unsigned := 0;
      From_Page  : Natural := First_Page;
      To_Page    : Natural := Last_Page;
      Copies     : Natural := 1;
      Success    : Boolean;
      use type Interfaces.C.unsigned;
      procedure Info (Message : GString) is
        NL : constant GCharacter := GCharacter'Val (13);
      begin
         Message_Box
            ("Print Hello - Choice",
             Message & NL &
             NL &
             "From page (if relevant):" &
             To_GString_From_String (From_Page'Image) & NL &
             "To page (if relevant):" &
             To_GString_From_String (To_Page'Image) & NL  &
             "Copies:" &
             To_GString_From_String (Copies'Image) & NL &
             --  "Device:"     & Device_Name (Settings) & NL &
             --  "Paper form:" & Form_Name (Settings) & NL &
             NL &
             "Flags code:" &
             To_GString_From_String (Flags'Image),
             OK_Box,
             Information_Icon);
      end Info;
      Radio_Flags : constant := PD_SELECTION + PD_PAGENUMS;
      --  + PD_CURRENTPAGE
   begin
      Choose_Printer
         (Window,
          Canvas,
          Settings,
          Flags,
          From_Page,
          To_Page,
          First_Page,
          Last_Page,
          Copies,
          Success);

      --  Choose_Default_Printer (Canvas, Settings, Success);

      if Success then

         if (Flags and Radio_Flags) = PD_ALLPAGES then
            --  The fall-back value is 0, when other
            --  radio buttons are not chosen.
            Info ("You have chosen ""All Pages""");
         elsif (Flags and PD_SELECTION) > 0 then
            Info ("You have chosen ""Print Selection""");
         elsif (Flags and PD_PAGENUMS) > 0 then
            Info ("You have chosen a page range");
         else
            Info ("Unknown print range choice");
         end if;

         Start_Document (Canvas, "Print Hello Document");
         --
         if From_Page = First_Page then
            Start_Page (Canvas);
            Put (Canvas, 100, 100, "Hello World (page 1)!");
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
         --
         Message_Box ("Print Hello", "Done", OK_Box, Information_Icon);
      else
         Message_Box ("Print Hello", "Unable to print", OK_Box, Error_Icon);
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

   Create (Button, Top, "Print", 20, 20, 300, 100);
   On_Click_Handler (Button, Do_Print'Unrestricted_Access);

   GWindows.Application.Message_Loop;

end Print_Hello;
