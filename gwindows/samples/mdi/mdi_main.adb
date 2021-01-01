with Ada.Text_IO;

with Interfaces.C;

with Standard_IDs;

with GWindows.Base;
with GWindows.Application;
with GWindows.Menus;
with GWindows.Message_Boxes;
with GWindows.Common_Dialogs;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.GStrings.Unbounded;
with GWindows.Edit_Boxes;

with MDI_Edit_Child;

package body MDI_Main is

   Current_MDI_Window : Natural := 0;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create (Window : in out MDI_Main_Type) is
      use GWindows.Menus;
      use GWindows.Image_Lists;
      use Standard_IDs;
   begin
      Small_Icon (Window, "Main_Icon");
      Large_Icon (Window, "Main_Icon");

      MDI_Menu (Window, Load_Menu ("Main_Menu"), 2);
      Accelerator_Table (Window, "Main_Menu");

      Create (Window.Status_Bar, Window, "Ready");
      Dock (Window.Status_Bar, GWindows.Base.At_Bottom);

      Create (Window.Tool_Bar, Window, 0, 0, 0, 40);
      Dock (Window.Tool_Bar, GWindows.Base.At_Top);

      Create (Window.Images, "Toolbar_Bmp", 16);
      Set_Image_List (Window.Tool_Bar, Window.Images);

      Add_Button (Window.Tool_Bar, 0, ID_FILE_NEW);
      Add_Button (Window.Tool_Bar, 1, ID_FILE_OPEN);
      Add_Button (Window.Tool_Bar, 2, ID_FILE_SAVE);
      Add_Button (Window.Tool_Bar, 3, ID_EDIT_CUT);
      Add_Button (Window.Tool_Bar, 4, ID_EDIT_COPY);
      Add_Button (Window.Tool_Bar, 5, ID_EDIT_PASTE);
      Add_Button (Window.Tool_Bar, 7, ID_APP_ABOUT);

      Show (Window);
   end On_Create;

   -----------------
   -- On_File_New --
   -----------------

   procedure On_File_New (Window : in out MDI_Main_Type) is
      use MDI_Edit_Child;

      New_Window : constant MDI_Edit_Child_Access := new MDI_Edit_Child_Type;

      function Suffix return GWindows.GString;
      --  Suffix

      function Suffix return GWindows.GString is
      begin
         if Current_MDI_Window = 0 then
            return "";
         else
            return To_GString_From_String (Current_MDI_Window'Img);
         end if;
      end Suffix;

   begin
      Create_MDI_Child (New_Window.all,
                        Window, "New Window" & Suffix, Is_Dynamic => True);
      MDI_Active_Window (Window, New_Window.all);

      Current_MDI_Window := Current_MDI_Window + 1;
   end On_File_New;

   ------------------
   -- On_File_Open --
   ------------------

   procedure On_File_Open (Window : in out MDI_Main_Type) is
      use MDI_Edit_Child;
      use GWindows.Common_Dialogs;

      New_Window : constant MDI_Edit_Child_Access := new MDI_Edit_Child_Type;
      File_Title : GWindows.GString_Unbounded;
      Success    : Boolean;
   begin
      Open_File (Window, "Open...",
                 New_Window.File_Name,
                 ((To_GString_Unbounded ("Text Files (*.txt)"),
                   To_GString_Unbounded ("*.txt")),
                  (To_GString_Unbounded ("All Files (*.*)"),
                   To_GString_Unbounded ("*.*"))),
                 ".txt",
                 File_Title,
                 Success);

      if Success then
         Create_MDI_Child (New_Window.all,
                           Window,
                           To_GString_From_Unbounded (File_Title),
                           Is_Dynamic => True);
         MDI_Active_Window (Window, New_Window.all);

         declare
            use Ada.Text_IO;

            OFile : File_Type;
            OText : GWindows.GString_Unbounded;
            NL    : constant String := Character'Val (13) & Character'Val (10);
         begin
            Open (File => OFile,
                  Mode => In_File,
                  Name => To_String
                    (To_GString_From_Unbounded (New_Window.File_Name)));

            while not End_Of_File (OFile) loop
               declare
                  use GWindows.GStrings.Unbounded;

                  Line_Length       : Natural;
                  Max_String_Length : constant := 255;
                  Output_String     : String (1 .. Max_String_Length);
               begin
                  Get_Line (File => OFile,
                            Item => Output_String,
                            Last => Line_Length);

                  OText := OText &
                    To_GString_From_String
                      (Output_String (1 .. Line_Length) & NL);
               end;
            end loop;

            Close (OFile);

            GWindows.Edit_Boxes.Text (New_Window.Edit_Box,
                                      To_GString_From_Unbounded (OText));
         exception
            when others =>
               declare
                  use GWindows.Message_Boxes;
               begin
                  Message_Box (Window,
                               "Open...",
                               "Unable to open file",
                               OK_Box,
                               Exclamation_Icon);
               end;
         end;
      end if;
   end On_File_Open;

   --------------
   -- On_About --
   --------------

   procedure On_About (Window : in out MDI_Main_Type) is
      use GWindows;
      NL : constant GString := GCharacter'Val (13) & GCharacter'Val (10);
   begin
      GWindows.Message_Boxes.Message_Box
        (Window,
         "MDI_Example",
         "Sample MDI Application" & NL &
         "MDI child windows open:" &
         To_GString_From_String (
           Integer'Image (Count_MDI_Children (Window)))
        );
   end On_About;

   -------------------
   -- On_Menu_Hover --
   -------------------

   overriding procedure On_Menu_Hover (Window  : in out MDI_Main_Type;
                                       Item    : in     Integer;
                                       Kind    : in     GWindows.Windows.Hover_Item_Type)
   is
      use GWindows.Windows;
   begin
      if Kind = GWindows.Windows.Menu_Item and Item > 0 then
         Text (Window.Status_Bar,
               GWindows.Application.Load_String
                 (Interfaces.C.unsigned (Item)));
      else
         Text (Window.Status_Bar, "Ready");
      end if;
   end On_Menu_Hover;

   --------------------
   -- On_Menu_Select --
   --------------------

   overriding procedure On_Menu_Select
     (Window : in out MDI_Main_Type;
      Item   : in     Integer)
   is
      use Standard_IDs;
      use GWindows.Windows;
   begin
      case Item is
         when ID_FILE_NEW =>
            On_File_New (Window);
         when ID_FILE_OPEN =>
            On_File_Open (Window);
         when ID_APP_ABOUT =>
            On_About (Window);
         when ID_APP_EXIT =>
            Close (Window);
         when ID_WINDOW_CASCADE =>
            MDI_Cascade (Window);
         when ID_WINDOW_TILE_HORZ =>
            MDI_Tile_Horizontal (Window);
         when ID_WINDOW_TILE_VERT =>
            MDI_Tile_Vertical (Window);
         when ID_WINDOW_CLOSE_ALL =>
            MDI_Close_All (Window);
         when others =>
            On_Menu_Select (Window_Type (Window), Item);
      end case;
   end On_Menu_Select;

   --------------------
   -- On_Right_Click --
   --------------------

   overriding procedure On_Right_Click (Control : in out MDI_Status_Bar_Type) is
      Parent : constant MDI_Main_Access :=
        MDI_Main_Access (Controlling_Parent (Control));
   begin
      On_About (Parent.all);
   end On_Right_Click;

   ----------------------
   -- On_Button_Select --
   ----------------------

   overriding procedure On_Button_Select (Control : in out MDI_Toolbar_Type;
                                          Item    : in     Integer)
   is
      Parent : constant MDI_Main_Access :=
        MDI_Main_Access (Controlling_Parent (Control));
   begin
      On_Menu_Select (Parent.all, Item);
   end On_Button_Select;

end MDI_Main;
