with Ada.Directories;          use Ada.Directories;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with GWindows.Application;     use GWindows.Application;
with GWindows.GStrings;        use GWindows.GStrings;
with GWindows.List_Boxes;      use GWindows.List_Boxes;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Windows;         use GWindows.Windows;

package body GWindows.Open_File is

   type Open_File_Window is new Window_Type with
      record
         Files    : List_Box_Type;
         Selected : Boolean := False;
      end record;
   type Open_File_Window_Access is access all Open_File_Window;

   procedure Do_Size (Window : in out GWindows.Base.Base_Window_Type'Class;
                      Width  : in     Integer;
                      Height : in     Integer) is
      pragma Unreferenced (Width, Height);
      Win : Open_File_Window renames Open_File_Window (Window);
   begin
      Size (Win.Files,
            (Client_Area_Width (Win) - 20, Client_Area_Height (Win) - 20));
   end Do_Size;

   procedure Open_File
     (Window            : in out GWindows.Base.Base_Window_Type'Class;
      Dialog_Title      : in     GString;
      File_Name         : in out GString_Unbounded;
      Success           :    out Boolean) is

      procedure Do_Double_Click
                   (Window : in out GWindows.Base.Base_Window_Type'Class) is
         Win : Open_File_Window_Access :=
               Open_File_Window_Access (Parent (List_Box_Type (Window)));
      begin
         File_Name := To_GString_Unbounded (Text (List_Box_Type (Window)));
         Win.Selected := True;
         Close (Win.all);
      end Do_Double_Click;

      Win           : Open_File_Window;
      Search        : Search_Type;
      Dir_Entry     : Directory_Entry_Type;
      Hlp_File_Name : String :=
                      To_String (To_GString_From_Unbounded (File_Name));
      Pos           : Integer;
      Font          : Font_Type;
   begin
      Create_Child (Win, Window, Dialog_Title, Top => 0, Left => 0,
                    Width => 200, Height => 400);
      Center (Win (Window));
      Create (Win.Files, Win, Top => 10, Left => 10, Width => 0, Height => 0);
      On_Size_Handler (Win, Do_Size'Unrestricted_Access);
      Create_Stock_Font (Font, Default_GUI);
      Set_Font (Win.Files, Font);
      Pos := Index (Hlp_File_Name, "\", Backward);
      if Pos = 0 then
         Pos := Hlp_File_Name'Last + 1;
      end if;
      Start_Search (Search,
                    Hlp_File_Name (1 .. Pos - 1),
                    Hlp_File_Name (Pos + 1 .. Hlp_File_Name'Last),
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Entry);
         Add (Win.Files, To_GString_From_String (Simple_Name (Dir_Entry)));
      end loop;
      End_Search (Search);
      On_Double_Click_Handler (Win.Files, Do_Double_Click'Unrestricted_Access);
      Show_Modal (Win, Window);
      Success := Win.Selected;
   end Open_File;

end GWindows.Open_File;
