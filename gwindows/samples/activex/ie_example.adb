with Ada.Exceptions;
with Ada.Command_Line;

with GNAT.OS_Lib;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;

with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Base;
with GWindows.ActiveX; use GWindows.ActiveX;
with GWindows.Events;
with GWindows.Menus; use GWindows.Menus;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Application; use GWindows.Application;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;

procedure IE_Example is
   pragma Linker_Options ("-mwindows");

   ID_FILE_OPEN : constant := 100;
   ID_FILE_EXIT : constant := 101;

   Main    : Main_Window_Type;
   Contain : ActiveX_Type;
   IE_Control : Dispinterface_Type;

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);
   --  Handle menu selection

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
   begin
      case Item is
         when ID_FILE_OPEN =>
            declare
               Location_Window : Window_Type;
               URL             : Edit_Box_Type;
               OK_Button       : Default_Button_Type;
            begin
               Create_As_Dialog (Location_Window, Main,
                                 "Open Location...",
                                 Width => 200,
                                 Height => 100);

               Center (Location_Window, Main);
               Keyboard_Support (Location_Window);

               Create (URL, Location_Window, "", 5, 5, 185, 25);
               Focus (URL);

               Create (OK_Button, Location_Window, "O&k", 60, 35, 70, 25);
               On_Click_Handler (OK_Button,
                                 GWindows.Events.Do_Dialog_OK'Access);

               Show_Dialog (Location_Window, Main);

               if Text (URL) /= "" then
                  Text (Main, Text (URL));
                  Invoke (IE_Control, "navigate",
                          (1 => To_VARIANT (To_String (Text (URL)))));
               end if;
            end;
         when ID_FILE_EXIT =>
            End_Application;

         when others =>
            null;
      end case;
   end Do_Menu_Select;

   Menu      : Menu_Type;
   File_Menu : Menu_Type;
begin
   Initialize_COM;

   Create (Main, "IE Web Viewer",
           Width => 400, HEIGHT => 300);
   Center (Main);
   On_Menu_Select_Handler (Main, Do_Menu_Select'Unrestricted_Access);

   Create (Contain, Main, "Shell.Explorer", 0, 0,
           Client_Area_Width (Main),
           Client_Area_Height (Main));
   Dock (Contain, GWindows.Base.Fill);
   Query (IE_Control, Interface (Contain));

   Menu := Create_Menu;
   File_Menu := Create_Popup;

   Append_Item (File_Menu, "&Open...", ID_FILE_OPEN);
   Append_Item (File_Menu, "E&xit", ID_FILE_EXIT);

   Append_Menu (Menu, "&File", File_Menu);

   GWindows.Windows.Main.Menu (Main, Menu);

   Show (Main);

   if Ada.Command_Line.Argument_Count >= 1 then
      Text (Main, To_GString_From_String (Ada.Command_Line.Argument (1)));
      Invoke (IE_Control, "navigate",
              (1 => To_VARIANT (Ada.Command_Line.Argument (1))));
   end if;

   Message_Loop;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("IE Viewer",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end IE_Example;
