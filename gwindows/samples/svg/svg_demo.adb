with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GNATCOM.GUID;
with GNATCOM.Initialize; use GNATCOM.Initialize;

with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Base;
with GWindows.ActiveX; use GWindows.ActiveX;
with GWindows.Common_Dialogs;
with GWindows.Menus; use GWindows.Menus;
with GWindows.Application; use GWindows.Application;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;

with Adobe_SVG.ISVGCtl_Interface; use Adobe_SVG.ISVGCtl_Interface;

procedure SVG_demo is
   pragma Linker_Options ("-mwindows");

   ID_FILE_OPEN : constant := 100;
   ID_FILE_EXIT : constant := 101;

   Main    : Main_Window_Type;
   Contain : ActiveX_Type;
   SVG_Ctl : ISVGCtl_Type;

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
               Open_File (Main, "Open SVG file...",
                          File_Name,
                          ((To_GString_Unbounded ("SVG Files"),
                            To_GString_Unbounded ("*.svg")),
                           (To_GString_Unbounded ("All Files"),
                            To_GString_Unbounded ("*.*"))),
                          "svg",
                          File_Title,
                          Success);

               if Success then
                  SetSrc (SVG_Ctl,
                          To_BSTR_From_GString (To_GString (File_Name)));
                  Text (Main, To_GString (File_Title));
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

   Create (Main, "SVG Viewer",
           Width => 400, HEIGHT => 300);
   On_Menu_Select_Handler (Main, Do_Menu_Select'Unrestricted_Access);

   Create (Contain, Main,
           To_GString_From_String
           (GNATCOM.GUID.To_String (Adobe_SVG.CLSID_SVGCtl)), 0, 0,
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

   Query (SVG_Ctl, Interfac (Contain));

   Message_Loop;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("SVG_Demo",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end SVG_Demo;
