with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

with GNAT.OS_Lib;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;

with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Base;
with GWindows.ActiveX; use GWindows.ActiveX;
with GWindows.Common_Dialogs;
with GWindows.Menus; use GWindows.Menus;
with GWindows.Application; use GWindows.Application;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;

procedure PDF_Example is
   pragma Linker_Options ("-mwindows");

   ID_FILE_OPEN : constant := 100;
   ID_FILE_EXIT : constant := 101;

   Main    : Main_Window_Type;
   Contain : ActiveX_Type;
   Control : Dispinterface_Type;

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);
   --  Handle menu selection

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item  : in     Integer)
   is
      use GWindows.Common_Dialogs;
      use Ada.Strings.Unbounded;

   begin
      case Item is
         when ID_FILE_OPEN =>
            declare
               File_Name  : GWindows.GString_Unbounded;
               File_Title : GWindows.GString_Unbounded;
               Success    : Boolean;
            begin
               Open_File (Main, "Open PDF file...",
                          File_Name,
                          ((To_GString_Unbounded ("PDF Files"),
                            To_GString_Unbounded ("*.pdf")),
                           (To_GString_Unbounded ("All Files"),
                            To_GString_Unbounded ("*.*"))),
                          "pdf",
                          File_Title,
                          Success);

               if Success then
                  Invoke (Control, "LoadFile",
                          (1 => To_VARIANT
                             (To_String
                                (To_GString_From_Unbounded (File_Name)))));
                  Text (Main, To_GString_From_Unbounded (File_Title));
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

   Create (Main, "PDF Viewer",
           Width => 400, HEIGHT => 300);
   On_Menu_Select_Handler (Main, Do_Menu_Select'Unrestricted_Access);

   Create (Contain, Main, "PDF.PdfCtrl.1", 0, 0,
           Client_Area_Width (Main),
           Client_Area_Height (Main));
   Dock (Contain, GWindows.Base.Fill);

   Menu := Create_Menu;
   File_Menu := Create_Popup;

   Append_Item (File_Menu, "&Open...", ID_FILE_OPEN);
   Append_Item (File_Menu, "E&xit", ID_FILE_EXIT);

   Append_Menu (Menu, "&File", File_Menu);

   GWindows.Windows.Main.Menu (Main, Menu);

   Show (Main);

   Query (Control, Interface (Contain));

   if Ada.Command_Line.Argument_Count >= 1 then
      Invoke (Control, "LoadFile",
              (1 => To_VARIANT (Ada.Command_Line.Argument (1))));
      Text (Main, To_GString_From_String (Ada.Command_Line.Argument (1)));
   end if;

   Message_Loop;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("PDF Viewer",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end PDF_Example;
