with Ada.Text_IO;
with GNAT.OS_Lib;

with Standard_IDs;

with GWindows.Base;
with GWindows.Common_Dialogs;
with GWindows.Drawing_Objects;
with GWindows.GStrings;
with GWindows.Menus;
with GWindows.Message_Boxes;

package body MDI_Edit_Child is

   use GWindows.GStrings;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create (Window : in out MDI_Edit_Child_Type) is
      use GWindows.Edit_Boxes;
      use GWindows.Menus;

      Window_Font : GWindows.Drawing_Objects.Font_Type;
   begin
      Small_Icon (Window, "Main_Icon");

      GWindows.Drawing_Objects.Create_Stock_Font
        (Window_Font, GWindows.Drawing_Objects.ANSI_Fixed_Width);
      Set_Font (Window, Window_Font);

      Create_Multi_Line (Window.Edit_Box,
                         Window,
                         "",
                         0, 0,
                         Client_Area_Width (Window),
                         Client_Area_Height (Window),
                         True);
      Dock (Window.Edit_Box, GWindows.Base.Fill);

      Focus (Window.Edit_Box);

      Dock_Children (Window);

      MDI_Menu (Window, Load_Menu ("Edit_Child_Menu"), 3);

      Zoom (Window);
   end On_Create;

   --------------------
   -- On_Menu_Select --
   --------------------

   overriding procedure On_Menu_Select
     (Window : in out MDI_Edit_Child_Type;
      Item   : in     Integer)
   is
      use Standard_IDs;
      use GWindows.Edit_Boxes;
   begin
      case Item is
         when ID_FILE_SAVE =>
            On_Save (Window);
         when ID_FILE_SAVE_AS =>
            On_Save_As (Window);
         when ID_EDIT_COPY =>
            Copy (Window.Edit_Box);
         when ID_EDIT_PASTE =>
            Paste (Window.Edit_Box);
         when ID_EDIT_CUT =>
            Cut (Window.Edit_Box);
         when ID_EDIT_UNDO =>
            Undo (Window.Edit_Box);
         when others =>
            null;
      end case;
   end On_Menu_Select;

   -------------
   -- On_Save --
   -------------

   procedure On_Save (Window : in out MDI_Edit_Child_Type)
   is
      File_Name : constant GWindows.GString :=
        To_GString_From_Unbounded (Window.File_Name);
   begin
      if File_Name = "" then
         On_Save_As (Window);
      else
         Save (Window, File_Name);
      end if;
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As (Window : in out MDI_Edit_Child_Type)
   is
      use GWindows.Common_Dialogs;

      New_File_Name : GWindows.GString_Unbounded := Window.File_Name;
      File_Title    : GWindows.GString_Unbounded;
      Success       : Boolean;
   begin
      Save_File (Window, "Save As...",
                 New_File_Name,
                 ((To_GString_Unbounded ("Text Files (*.txt)"),
                   To_GString_Unbounded ("*.txt")),
                  (To_GString_Unbounded ("All Files (*.*)"),
                   To_GString_Unbounded ("*.*"))),
                 ".txt",
                 File_Title,
                 Success);
      if Success then
         if
           GNAT.OS_Lib.Is_Regular_File
              (To_String ((To_GString_From_Unbounded (New_File_Name))))
         then
            declare
               use GWindows, GWindows.Message_Boxes;
               NL : constant GString := GCharacter'Val (13) & GCharacter'Val (10);
            begin
               if Message_Box (Window,
                               "Save As",
                               To_GString_From_Unbounded (New_File_Name) &
                               " already exists." & NL &
                               "Do you want to replace it?",
                               Yes_No_Box,
                               Exclamation_Icon) = No
               then
                  return;
               end if;
            end;
         end if;

         Window.File_Name := New_File_Name;
         Text (Window, To_GString_From_Unbounded (File_Title));
         Save (Window, To_GString_From_Unbounded (New_File_Name));
      end if;
   end On_Save_As;

   ----------
   -- Save --
   ----------

   procedure Save (Window    : in out MDI_Edit_Child_Type;
                   File_Name : in     GWindows.GString)
   is
      use Ada.Text_IO;
      use GWindows.Edit_Boxes;

      OFile : File_Type;
   begin
      Create (File => OFile,
            Mode => Out_File,
            Name => To_String (File_Name));

      for N in 1 .. Line_Count (Window.Edit_Box) loop
         Put_Line (OFile, To_String (Line_Text (Window.Edit_Box, N)));
      end loop;

      Close (OFile);
   exception
      when others =>
         declare
            use GWindows.Message_Boxes;
         begin
            Message_Box (Window,
                         "Open...",
                         "Unable to save file",
                         OK_Box,
                         Exclamation_Icon);
         end;
   end Save;

end MDI_Edit_Child;
