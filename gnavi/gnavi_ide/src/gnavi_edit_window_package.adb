with Ada.Unchecked_Deallocation;

with GNAVI_Common;
with GNAVI_Controls_Window_Package;
with GNAVI_Main_Menus;
with GNAVI_Main_Package;
with GNAVI_Project;
with GNAVI_Project_Window_Package;

with Standard_IDs;
with GNAVI_IDs;

with GWindows.Colors;
with GWindows.GStrings.Handling;
with GWindows.GStrings.Constants;
with GWindows.Menus;
with GWindows.Message_Boxes;

package body GNAVI_Edit_Window_Package is

   procedure On_Create (Window : in out GNAVI_Edit_Window_Type) is separate;

   -- On_Menu_Select added by GdM, July 2012
   -- Probably should also be separate, like On_Create,
   -- and generated GNAVI
   procedure On_Menu_Select (Window : in out GNAVI_Edit_Window_Type;
                             Item   : in     Integer) is
   begin
      Handle_Menu (Window, Item);
   end On_Menu_Select;

   -------------------------------------------------------------------------
   --  Private Methods
   -------------------------------------------------------------------------

   procedure Save_All_Views (Window : in out GNAVI_Edit_Window_Type);
   --  Save all window views

   procedure Load_All_Views (Window : in out GNAVI_Edit_Window_Type);
   --  Load all window views

   procedure Hide_All_Views (Window : in out GNAVI_Edit_Window_Type);
   --  Hide all window views

   procedure Show_View
     (View_Name : in     GWindows.GString;
      Window    : in out GWindows.Base.Base_Window_Type'Class);
   --  Show a particular view

   procedure Save (Window    : in out GWindows.Scintilla.Scintilla_Type;
                   File_Name : in     GWindows.GString);

   procedure Load (Window    : in out GWindows.Scintilla.Scintilla_Type;
                   File_Name : in     GWindows.GString);


   procedure Reload_Details (This : in out GNAVI_Edit_Window_Type);


   procedure Save (Window    : in out GWindows.Scintilla.Scintilla_Type;
                   File_Name : in     GWindows.GString)
   is
      use GWindows.Scintilla;
      use GNAT.OS_Lib;

      OFile       : File_Descriptor;
      Doc_Length  : constant Position := Get_Length (Window);
      Length      : Position := 0;
      F_Exception : exception;
      Success     : Boolean;
      FName       : GWindows.GString := File_Name;
   begin
      if Get_Modify (Window) = False then
         return;
      end if;

      GWindows.GStrings.To_Lower (FName);
      Rename_File (GWindows.GStrings.To_String (FName),
                   GWindows.GStrings.To_String (FName) & "~",
                   Success);

      OFile := Create_File (GWindows.GStrings.To_String (FName), Binary);

      if OFile = Invalid_FD then
         raise F_Exception;
      end if;

      while Length < Doc_Length loop
         declare
            Grab_Size  : Position := Doc_Length - Length;
            Block_Size : constant := 255;
         begin
            if Grab_Size > Block_Size then
               Grab_Size := Block_Size;
            end if;

            declare
               Buffer : String := GWindows.GStrings.To_String
                 (Get_Text_Range (Window,
                                  Length,
                                  Length + Grab_Size));

               Dummy : Integer :=
                 Write (OFile, Buffer (Buffer'First)'Address, Integer (Grab_Size));
            begin
               Length := Length + Grab_Size;
            end;
         end;
      end loop;

      Close (OFile);
      Set_Save_Point (Window);
   exception
      when others =>
         declare
            use GWindows.Message_Boxes;
         begin
            Message_Box (Controlling_Parent (Window).all,
                         "Save...",
                         "Unable to save file",
                         OK_Box,
                         Exclamation_Icon);
         end;
   end Save;

   procedure Load (Window    : in out GWindows.Scintilla.Scintilla_Type;
                   File_Name : in     GWindows.GString)
   is
      use GWindows.Scintilla;
      use GNAT.OS_Lib;

      type String_Buffer is new String (1 .. 255);

      OFile         : File_Descriptor;
      Output_String : String_Buffer;
      Length        : Integer := 1;

      F_Exception   : exception;
   begin
      --  Check disk for modification and if none do nothing.

      Clear_All (Window);
      Set_Undo_Collection (Window, False);

      OFile := Open_Read (GWindows.GStrings.To_String (File_Name),
                          Binary);

      if OFile = Invalid_FD then
         raise F_Exception;
      end if;

      while Length > 0 loop
         Length := Read (OFile,
                         Output_String (Output_String'First)'Address,
                         Output_String'Length);

         GWindows.Scintilla.Add_Text
           (Window,
            GWindows.GStrings.To_GString_From_String
            (String (Output_String (1 .. Length))));
      end loop;

      Close (OFile);

      Convert_EOLs (Window,
                   SC_EOL_CRLF);
      Set_Undo_Collection (Window, True);
      Empty_Undo_Buffer (Window);
      Set_Save_Point (Window);
      Go_To_Pos (Window, 0);
   exception
      when others =>
         declare
            use GWindows.Message_Boxes;
         begin
            Message_Box (Controlling_Parent (Window).all,
                         "Open...",
                         "Unable to open file",
                         OK_Box,
                         Exclamation_Icon);
         end;
   end Load;

   procedure Save_All_Views (Window : in out GNAVI_Edit_Window_Type)
   is
      use GNAVI_Window;
   begin
      Save (Window.Body_Edit_Box,
            Window_Name (Window.Win_XML) & "_package.adb");


      Save (Window.Spec_Edit_Box,
            Window_Name (Window.Win_XML) & "_package.ads");

      Save (Window.XML_Edit_Box,
            Window_Name (Window.Win_XML) & ".gnw");
   end Save_All_Views;

   procedure Load_Outline_View (Window : in out GNAVI_Edit_Window_Type)
   is
      use GWindows.List_Boxes;
      use GNAVI_Window;

      IL : Positive := 1;

      function Expand (S : GWindows.GString; L : Natural)
                      return GWindows.GString
      is
         N : constant Natural := L - 1;
      begin
         if N = 0 then
            return S & S;
         else
            return Expand (S & S, N);
         end if;
      end Expand;

      procedure Load_More (Controls : GNAVI_Window.Control_Element) is
         use type GNAVI_Window.Control_Element;
         N : GNAVI_Window.Control_Element := First_Control (Controls);
      begin
         while N /= null loop
            declare
               A : constant GWindows.GString :=
                 Expand (" ", IL) & "   " & Control_Name (N);
            begin
               Add (Window.Outline_View, A);
               if Has_Child_Controls (N) then
                  IL := IL + 1;
                  Load_More (Child_Controls (N));
                  IL := IL - 1;
               end if;
            end;
            N := Next_Control (N);
         end loop;
      end Load_More;

   begin
      Clear (Window.Outline_View);
      Clear (Window.Handlers_View);
      Clear (Window.Properties_View);
      Add (Window.Outline_View, Window_Name (Window.Win_XML));
      Load_More (Controls (Window.Win_XML));
   end Load_Outline_View;

   procedure Load_All_Views (Window : in out GNAVI_Edit_Window_Type)
   is
      use GWindows.GStrings;
      use GNAVI_Window;
      use GNAT.OS_Lib;

      Win_Name  : constant GWindows.GString := Window_Name (Window.Win_XML);
      Body_Name : constant GWindows.GString := Win_Name & "_package.adb";
      Spec_Name : constant GWindows.GString := Win_Name & "_package.ads";
      XML_Name  : constant GWindows.GString := Win_Name & ".gnw";

      B_TS : constant Time_Stamp := File_Time_Stamp (To_String (Body_Name));
      S_TS : constant Time_Stamp := File_Time_Stamp (To_String (Spec_Name));
      X_TS : constant Time_Stamp := File_Time_Stamp (To_String (XML_Name));
   begin
      if Window.Body_TS < B_TS then
         Load (Window.Body_Edit_Box, Body_Name);
         Window.Body_TS := B_TS;
      end if;

      if Window.Spec_TS < S_TS then
         Load (Window.Spec_Edit_Box, Spec_Name);
         Window.Spec_TS := S_TS;
      end if;

      if Window.XML_TS < X_TS then
         Load (Window.XML_Edit_Box, XML_Name);
         Refresh (Window.Win_XML);
         Load_Outline_View (Window);
         Window.XML_TS := X_TS;
      end if;
   end Load_All_Views;

   procedure Hide_All_Views (Window : in out GNAVI_Edit_Window_Type)
   is
      use GWindows.Scintilla;
      use GWindows.Packing_Boxes;
      use GWindows.Base;
      use GWindows.GStrings;
      use GWindows.Scroll_Panels;
      use GNAT.OS_Lib;

      Win_Name  : constant GWindows.GString :=
        GNAVI_Window.Window_Name (Window.Win_XML);

      XML_Name  : constant GWindows.GString := Win_Name & ".gnw";

      X_TS : constant Time_Stamp :=
        GNAT.OS_Lib.File_Time_Stamp (To_String (XML_Name));
   begin
      Save_All_Views (Window);

      if Window.XML_TS < X_TS then
         GNAVI_Project.Run_ICG
           (GNAVI_Project_Window_Package.GNAVI_Project_Window.Project);
         Load_All_Views (Window);
      end if;

      Visible (Window.XML_Edit_Box, False);
      Visible (Window.Spec_Edit_Box, False);
      Visible (Window.Body_Edit_Box, False);
      Visible (Window.Outline_Box, False);
      GNAVI_Layout_View.Close (Window.Layout_Editor);
      Visible (Window.Layout_Box, False);

      Dock (Window.XML_Edit_Box, None);
      Dock (Window.Spec_Edit_Box, None);
      Dock (Window.Body_Edit_Box, None);
      Dock (Window.Outline_Box, None);
      Dock (Window.Layout_Box, None);
   end Hide_All_Views;

   procedure Show_View
     (View_Name : in     GWindows.GString;
      Window    : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Base;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type (Controlling_Parent (Window).all);
   begin
      Text (This, View_Name & " - " & Window_Name (This.Win_XML));
      Text (This.Property_Edit_Box, "");
      Text (This.Handler_Edit_Box, "");
      Hide_All_Views (This);
      Visible (Window);
      Dock (Window, Fill);
      Dock_Children (This);
   end Show_View;

   type Window_Rec;

   type Window_Rec_Access is access all Window_Rec;

   type Window_Rec is
      record
         Name   : GWindows.GString_Unbounded;
         Window : GNAVI_Edit_Window_Access := null;
         Next   : Window_Rec_Access := null;
      end record;

   First_Window_Rec : aliased Window_Rec;

   procedure Add_To_Window_List (Name   : in GWindows.GString;
                                 Window : in GNAVI_Edit_Window_Access);

   function In_Window_List (Name : in GWindows.GString) return Boolean;

   procedure Remove_From_Window_List (Window : in GNAVI_Edit_Window_Access);

   procedure Add_To_Window_List (Name   : GWindows.GString;
                                 Window : GNAVI_Edit_Window_Access)
   is
      use GWindows.GStrings;

      C_Rec   : Window_Rec_Access := First_Window_Rec'Access;
      N_Rec   : constant Window_Rec_Access := new Window_Rec;
   begin
      N_Rec.Name   := To_GString_Unbounded (Name);
      N_Rec.Window := Window;

      while C_Rec.Next /= null loop
         C_Rec := C_Rec.Next;
      end loop;

      C_Rec.Next := N_Rec;
   end Add_To_Window_List;

   function In_Window_List (Name : in GWindows.GString) return Boolean is
      use GWindows.GStrings;

      C_Rec   : Window_Rec_Access := First_Window_Rec'Access;
   begin
      while C_Rec.Next /= null loop
         C_Rec := C_Rec.Next;

         if To_GString_From_Unbounded (C_Rec.Name) = Name then
            Activate (C_Rec.Window.all);
            return True;
         end if;
      end loop;

      return False;
   end In_Window_List;

   procedure Remove_From_Window_List (Window : in GNAVI_Edit_Window_Access) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Window_Rec,
                                         Window_Rec_Access);

      C_Rec   : Window_Rec_Access := First_Window_Rec'Access;
      N_Rec   : Window_Rec_Access;
   begin
      while C_Rec.Next /= null loop

         if C_Rec.Next.Window = Window then
            N_Rec := C_Rec.Next.Next;
            Free (C_Rec.Next);
            C_Rec.Next := N_Rec;
            return;
         end if;

         C_Rec := C_Rec.Next;
      end loop;

   end Remove_From_Window_List;

   procedure Reload_Details (This : in out GNAVI_Edit_Window_Type) is
      use GWindows.List_Boxes;
      use GNAVI_Window;

      Current_Index   : constant Natural := Current (This.Outline_View) - 1;
      Current_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Clear (This.Handlers_View);

      for N in 1 .. All_Handler_Count (Current_Control) loop
         Add (This.Handlers_View,
              All_Handler_Event (Current_Control, N) & " := " &
              All_Handler_Name (Current_Control, N));
      end loop;

      Clear (This.Properties_View);

      for N in 1 .. All_Property_Count (Current_Control) loop
         Add (This.Properties_View,
              All_Property_Name (Current_Control, N) & " := " &
              All_Property_Value (Current_Control, N));
      end loop;
   end Reload_Details;

   -------------------------------------------------------------------------
   --  Public Methods
   -------------------------------------------------------------------------

   procedure Open_Window (Name : GWindows.GString)
   is
      use GWindows.Base;
   begin
      if not In_Window_List (Name) then
         declare
            New_Window : constant GNAVI_Edit_Window_Access :=
              new GNAVI_Edit_Window_Type;

            P : constant GWindows.Base.Pointer_To_Base_Window_Class :=
              GNAVI_Main_Package.MDI_Active_Window
              (GNAVI_Main_Package.GNAVI_Main);

            Z : Boolean := False;
         begin
            if P /= null then
               Z := GWindows.Windows.Zoom
                     (GWindows.Windows.Window_Type (P.all));
            end if;

            GNAVI_Window.Open (New_Window.Win_XML, Name & ".gnw");

            Create_MDI_Child (New_Window.all,
                              GNAVI_Main_Package.GNAVI_Main,
                              Is_Dynamic => True);

            Zoom (New_Window.all, Z);
            Visible (New_Window.all);

            Add_To_Window_List (Name, New_Window);
         end;
      end if;
   end Open_Window;


   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Scintilla;
      use GWindows.Colors;
      use GNAVI_Main_Menus;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type (Window);

      M : constant Base_Menus := Setup_Editor_Menus;

   begin
      MDI_Menu (This, M.Main_Menu, M.Windows_Menu);

      --  Set up editor
      Set_EOL_Mode (This.Body_Edit_Box, SC_EOL_CRLF);
      Set_Tab_Width (This.Body_Edit_Box, GNAVI_Common.TAB_WIDTH);
      Set_Use_Tabs (This.Body_Edit_Box, False);
      Set_Edge_Column (This.Body_Edit_Box, 80);
      Set_Edge_Mode (This.Body_Edit_Box, EDGE_LINE);
      --  SetIndentationGuides (This.Body_Edit_Box, True);

      Set_Lexer (This.Body_Edit_Box, SCLEX_ADA);
      Set_Key_Words (This.Body_Edit_Box, 0, GNAVI_Common.Key_Words);

      Style_Set_Fore (This.Body_Edit_Box, STYLE_DEFAULT, Black);
      Style_Set_Back (This.Body_Edit_Box, STYLE_DEFAULT, White);
      Style_Set_Size (This.Body_Edit_Box, STYLE_DEFAULT, 10);
      Style_Set_Font (This.Body_Edit_Box, STYLE_DEFAULT, "Courier");
      Style_Clear_All (This.Body_Edit_Box);

      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_DEFAULT, Black);
      Style_Set_Back (This.Body_Edit_Box, SCE_ADA_DEFAULT, White);
      Style_Set_Size (This.Body_Edit_Box, SCE_ADA_DEFAULT, 10);
      Style_Set_Font (This.Body_Edit_Box, SCE_ADA_DEFAULT, "Courier");


      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_COMMENTLINE, Red);
      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_NUMBER, Blue);
      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_WORD, Dark_Green);
      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_STRING, Dark_Red);
      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_CHARACTER, Blue);
      --  StyleSetFore (This.Body_Edit_Box, SCE_ADA_OPERATOR, Black);
      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_IDENTIFIER, Black);

      Style_Set_Fore (This.Body_Edit_Box, SCE_ADA_STRINGEOL, White);
      Style_Set_Back (This.Body_Edit_Box, SCE_ADA_STRINGEOL, Red);

      --  Set up editor
      Set_EOL_Mode (This.Spec_Edit_Box, SC_EOL_CRLF);
      Set_Tab_Width (This.Spec_Edit_Box, GNAVI_Common.TAB_WIDTH);
      Set_Use_Tabs (This.Spec_Edit_Box, False);
      Set_Edge_Column (This.Spec_Edit_Box, 80);
      Set_Edge_Mode (This.Spec_Edit_Box, EDGE_LINE);
      --  SetIndentationGuides (This.Spec_Edit_Box, True);

      Set_Lexer (This.Spec_Edit_Box, SCLEX_ADA);
      Set_Key_Words (This.Spec_Edit_Box, 0, GNAVI_Common.Key_Words);

      Style_Set_Fore (This.Spec_Edit_Box, STYLE_DEFAULT, Black);
      Style_Set_Back (This.Spec_Edit_Box, STYLE_DEFAULT, White);
      Style_Set_Size (This.Spec_Edit_Box, STYLE_DEFAULT, 10);
      Style_Set_Font (This.Spec_Edit_Box, STYLE_DEFAULT, "Courier");
      Style_Clear_All (This.Spec_Edit_Box);

      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_DEFAULT, Black);
      Style_Set_Back (This.Spec_Edit_Box, SCE_ADA_DEFAULT, White);
      Style_Set_Size (This.Spec_Edit_Box, SCE_ADA_DEFAULT, 10);
      Style_Set_Font (This.Spec_Edit_Box, SCE_ADA_DEFAULT, "Courier");


      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_COMMENTLINE, Red);
      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_NUMBER, Blue);
      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_WORD, Dark_Green);
      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_STRING, Dark_Red);
      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_CHARACTER, Blue);
      --  Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_OPERATOR, Black);
      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_IDENTIFIER, Black);

      Style_Set_Fore (This.Spec_Edit_Box, SCE_ADA_STRINGEOL, White);
      Style_Set_Back (This.Spec_Edit_Box, SCE_ADA_STRINGEOL, Red);

      --  Set up editor
      Set_EOL_Mode (This.XML_Edit_Box, SC_EOL_CRLF);
      Set_Tab_Width (This.XML_Edit_Box, GNAVI_Common.TAB_WIDTH);
      Set_Use_Tabs (This.XML_Edit_Box, False);
      Set_Edge_Column (This.XML_Edit_Box, 80);
      Set_Edge_Mode (This.XML_Edit_Box, EDGE_LINE);
      --  SetIndentationGuides (This.XML_Edit_Box, True);

      Set_Lexer (This.XML_Edit_Box, SCLEX_XML);

      Style_Set_Fore (This.XML_Edit_Box, STYLE_DEFAULT, Black);
      Style_Set_Back (This.XML_Edit_Box, STYLE_DEFAULT, White);
      Style_Set_Size (This.XML_Edit_Box, STYLE_DEFAULT, 10);
      Style_Set_Font (This.XML_Edit_Box, STYLE_DEFAULT, "Courier");
      Style_Clear_All (This.XML_Edit_Box);

      Style_Set_Fore (This.XML_Edit_Box, SCE_H_DEFAULT, Black);
      Style_Set_Back (This.XML_Edit_Box, SCE_H_DEFAULT, White);
      Style_Set_Size (This.XML_Edit_Box, SCE_H_DEFAULT, 10);
      Style_Set_Font (This.XML_Edit_Box, SCE_H_DEFAULT, "Courier");


      Style_Set_Fore (This.XML_Edit_Box, SCE_H_COMMENT, Red);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_NUMBER, Blue);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_TAG, Dark_Green);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_ATTRIBUTE, Dark_Red);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_ENTITY, Blue);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_SINGLESTRING, Dark_Blue);
      Style_Set_Fore (This.XML_Edit_Box, SCE_H_DOUBLESTRING, Dark_Blue);

      --  Setup Layout Box
      GWindows.Scroll_Panels.Panel_Size (This.Layout_Box, 2024, 2024);
      GWindows.Scroll_Panels.Background_Color (This.Layout_Box.Panel,
                                               GWindows.Colors.White);
      Load_All_Views (This);

      This.Edit_Box := null;
      Show_View ("Outline View", This.Outline_Box);
   end Do_Create;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
      This : constant GNAVI_Edit_Window_Access :=
        GNAVI_Edit_Window_Type (Window)'Unchecked_Access;
   begin
      Can_Close := True;
      Remove_From_Window_List (This);
   end Do_Close;

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
      use Standard_IDs;
      use GNAVI_IDs;
      use GWindows.Scintilla;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type (Window);
   begin
      case Item is
         when ID_FILE_SAVE =>
            Save_All_Views (This);
         when ID_EDIT_COPY =>
            Copy (This.Edit_Box.all);
         when ID_EDIT_PASTE =>
            Paste (This.Edit_Box.all);
         when ID_EDIT_CUT =>
            Cut (This.Edit_Box.all);
         when ID_EDIT_UNDO =>
            Undo (This.Edit_Box.all);
         when ID_EDIT_REDO =>
            Redo (This.Edit_Box.all);
         when ID_EDIT_SELECTALL =>
            Select_All (This.Edit_Box.all);
         when ID_EDIT_DELETE =>
            Clear (This.Edit_Box.all);
         when others =>
            null;
      end case;
   end Handle_Menu;

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type)
   is
   pragma Unreferenced (Item);
      use GWindows.Menus;
      use GWindows.Scintilla;
      use type GWindows.Windows.Hover_Item_Type;
      use Standard_IDs;
      use GNAVI_IDs;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type (Window);

      M : constant Menu_Type := GWindows.Windows.Menu
        (GWindows.Windows.Window_Type (Controlling_Parent (This).all));
   begin
      if This.Edit_Box /= null then
         if Kind = GWindows.Windows.Menu then
            State (M, Command, ID_EDIT_UNDO, Enabled);
            State (M, Command, ID_EDIT_REDO, Enabled);
            State (M, Command, ID_EDIT_COPY, Enabled);
            State (M, Command, ID_EDIT_CUT, Enabled);
            State (M, Command, ID_EDIT_PASTE, Enabled);
            State (M, Command, ID_EDIT_DELETE, Enabled);
            State (M, Command, ID_EDIT_SELECTALL, Enabled);

            if Can_Paste (This.Edit_Box.all) then
               State (M, Command, ID_EDIT_PASTE, Enabled);
            else
               State (M, Command, ID_EDIT_PASTE, Grayed);
            end if;

            if Can_Undo (This.Edit_Box.all) then
               State (M, Command, ID_EDIT_UNDO, Enabled);
            else
               State (M, Command, ID_EDIT_UNDO, Grayed);
            end if;

            if Can_Redo (This.Edit_Box.all) then
               State (M, Command, ID_EDIT_REDO, Enabled);
            else
               State (M, Command, ID_EDIT_REDO, Grayed);
            end if;
         end if;
      else
         State (M, Command, ID_EDIT_UNDO, Grayed);
         State (M, Command, ID_EDIT_REDO, Grayed);
         State (M, Command, ID_EDIT_COPY, Grayed);
         State (M, Command, ID_EDIT_CUT, Grayed);
         State (M, Command, ID_EDIT_PASTE, Grayed);
         State (M, Command, ID_EDIT_DELETE, Grayed);
         State (M, Command, ID_EDIT_SELECTALL, Grayed);
      end if;
   end Check_Menu;

   procedure Do_Save_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      Save_All_Views (This);
   end Do_Save_Window;

   procedure Do_XML_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      This.Edit_Box := This.XML_Edit_Box'Unchecked_Access;
      Show_View ("XML View", This.XML_Edit_Box);
   end Do_XML_Window;

   procedure Do_Outline_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      This.Edit_Box := null;
      Show_View ("Outline View", This.Outline_Box);
   end Do_Outline_Window;

   procedure Do_Layout_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      This.Edit_Box := null;
      Show_View ("Layout View", This.Layout_Box);
      GNAVI_Layout_View.Edit_Window (This.Layout_Editor,
                                     This.Layout_Box.Panel,
                                     This.Win_XML);
   end Do_Layout_Window;

   procedure Do_Spec_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      This.Edit_Box := This.Spec_Edit_Box'Unchecked_Access;
      Show_View ("Spec View", This.Spec_Edit_Box);
   end Do_Spec_Window;

   procedure Do_Body_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      This.Edit_Box := This.Body_Edit_Box'Unchecked_Access;
      Show_View ("Body View", This.Body_Edit_Box);
   end Do_Body_Window;

   procedure Do_Select_OV_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Edit_Boxes;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);
   begin
      Text (This.Property_Edit_Box, "");
      Text (This.Handler_Edit_Box, "");

      Reload_Details (This);
   end Do_Select_OV_Control;

   procedure Do_Jump_To_Handler
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Window;
      use GWindows.List_Boxes;
      use GWindows.GStrings;
      use GWindows.Scintilla;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index   : constant Natural := Current (This.Outline_View) - 1;
      Current_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      declare
         Search_Text : String :=
           To_String ("procedure " &
                      All_Handler_Name (Current_Control,
                                        Current (This.Handlers_View))) &
           Character'Val (0);

         Begin_Text : String := "begin" & Character'Val (0);

         Find_Info : aliased Find_Text_Type :=
           (0, Sci_PositionCR (Get_Length (This.Body_Edit_Box)),
            Search_Text (1)'Address, 0, 0);

         F_POS, F_POS_2 : Position;
         pragma Unreferenced (F_POS_2);
      begin
         This.Edit_Box := This.Body_Edit_Box'Unchecked_Access;
         Show_View ("Body View", This.Body_Edit_Box);
         F_POS := Find_Text (This.Body_Edit_Box,
                             flags => 0,
                             ft => Find_Info'Unchecked_Access);
         Find_Info.Min := Sci_PositionCR (F_POS);
         Find_Info.Text := Begin_Text (1)'Address;

         F_POS := Find_Text (This.Body_Edit_Box,
                             flags => 0,
                             ft => Find_Info'Unchecked_Access);
         F_POS_2 :=
            Position (Line_From_Position (This.Body_Edit_Box, Position (Find_Info.TMax)));

         Go_To_Pos (This.Body_Edit_Box,
                    Position_From_Line (This.Body_Edit_Box, Integer (F_POS + 1)));


         Line_Scroll (This.Body_Edit_Box, 0, 5);

         Focus (This.Body_Edit_Box);
      end;
   end Do_Jump_To_Handler;

   procedure Do_Jump_To_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Window;
      use GWindows.List_Boxes;
      use GWindows.GStrings;
      use GWindows.Scintilla;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index   : constant Natural := Current (This.Outline_View) - 1;
      Current_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      declare
         Search_Text : String :=
           To_String ("name=""" &
                      Control_Name (Current_Control) & """" &
                      GWindows.GCharacter'Val (0));

         Find_Info : aliased Find_Text_Type :=
           (0, Sci_PositionCR (Get_Length (This.XML_Edit_Box)),
            Search_Text (1)'Address, 0, 0);

         F_POS : Position;
      begin
         This.Edit_Box := This.XML_Edit_Box'Unchecked_Access;
         Show_View ("XML View", This.XML_Edit_Box);
         F_POS := Find_Text (This.XML_Edit_Box,
                             flags => 0,
                             ft => Find_Info'Unchecked_Access);

         Go_To_Pos (This.XML_Edit_Box, F_POS);

         Line_Scroll (This.XML_Edit_Box, 0, 5);

         Focus (This.XML_Edit_Box);
      end;
   end Do_Jump_To_Control;

   procedure Do_Add_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Window;
      use GWindows.List_Boxes;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index   : constant Integer := Current (This.Outline_View);
      Current_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index <= 1 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index - 1);
      end if;

      Add_Control (This.Win_XML,
                   Current_Control,
                   "untitled",
                   GNAVI_Controls_Window_Package.Current_Control_Index);

      Load_Outline_View (This);
   end Do_Add_Control;

   procedure Do_Delete_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Window;
      use GWindows.List_Boxes;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index   : constant Integer := Current (This.Outline_View);
      Current_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index <= 1 then
         return;
      else
         Current_Control := Control (This.Win_XML, Current_Index - 1);
      end if;

      Delete_Control (This.Win_XML,
                      Current_Control);

      Load_Outline_View (This);
   end Do_Delete_Control;

   procedure Do_Property_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;

      Current_Property : constant Natural := Current (This.Properties_View);
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Text (This.Property_Edit_Box,
            All_Property_Value (Current_Control,
                                Current_Property));
   end Do_Property_Change;

   procedure Do_Property_Value_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;

      Current_Property : constant Natural := Current (This.Properties_View);
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      declare
         Prop_Name : constant GWindows.GString :=
           All_Property_Name (Current_Control, Current_Property);
      begin
         Set_All_Property (This.Win_XML,
                           Current_Control,
                           Prop_Name,
                           Text (This.Property_Edit_Box));

         Write (This.Win_XML);

         if Prop_Name = "name" then
            Load_Outline_View (This);
            Current (This.Outline_View, Current_Index + 1);
         end if;

         Reload_Details (This);
         Current (This.Properties_View, Current_Property);
      end;
   end Do_Property_Value_Change;

   procedure Do_Handler_Value_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;

      Current_Handler : constant Natural := Current (This.Handlers_View);
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Set_All_Handler (This.Win_XML,
                       Current_Control,
                       Text (This.Handler_Edit_Box),
                       All_Handler_Event (Current_Control,
                                          Current_Handler),
                       All_Handler_Type (Current_Control,
                                         Current_Handler));

      Write (This.Win_XML);
      Reload_Details (This);
      Current (This.Handlers_View, Current_Handler);
   end Do_Handler_Value_Change;

   procedure Do_Handler_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;

      Current_Handler : constant Natural := Current (This.Handlers_View);
   begin
      if Current_Index = 0 then
         Current_Control := Window_Element (This.Win_XML);
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      if Current_Handler > 0 then
         Text (This.Handler_Edit_Box, All_Handler_Name (Current_Control,
                                                        Current_Handler));
      end if;

   end Do_Handler_Change;

   procedure Do_Control_Up
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;
      use type GNAVI_Window.Control_Element;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;
      Previous_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index <= 1 then
         return;
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Previous_Control := Control (This.Win_XML, Current_Index - 1);

      Insert_Control_Before (This.Win_XML,
                             Current_Control,
                             Parent_Control (Previous_Control),
                             Previous_Control);

      Load_Outline_View (This);
      Current (This.Outline_View, Current_Index);
      Text (This.Property_Edit_Box, "");
      Text (This.Handler_Edit_Box, "");
      Reload_Details (This);
   end Do_Control_Up;

   procedure Do_Control_Down
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.GStrings.Handling;
      use GWindows.GStrings.Constants;
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;
      use type GNAVI_Window.Control_Element;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;
      Next_Control     : GNAVI_Window.Control_Element;
   begin
      if Current_Index = 0 then
         return;
      elsif Current_Index = Count (This.Outline_View) + 1 then
         return;
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Next_Control := Control (This.Win_XML, Current_Index + 1);
      if Next_Control = GNAVI_Window.Next_Control (Current_Control) then

         Next_Control := GNAVI_Window.Next_Control (Next_Control);
         Insert_Control_Before (This.Win_XML,
                                Current_Control,
                                Parent_Control (Current_Control),
                                Next_Control);

      end if;

      Load_Outline_View (This);

      for I in 2 .. Count (This.Outline_View) loop
         if
           Trim (Value (This.Outline_View, I), Both)
           =
           Control_Name (Current_Control)
         then
            Current (This.Outline_View, I);
         end if;
      end loop;

      Text (This.Property_Edit_Box, "");
      Text (This.Handler_Edit_Box, "");
      Reload_Details (This);
   end Do_Control_Down;

   procedure Do_Control_Right
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.List_Boxes;
      use GWindows.Edit_Boxes;
      use GNAVI_Window;
      use type GNAVI_Window.Control_Element;

      This : GNAVI_Edit_Window_Type
        renames GNAVI_Edit_Window_Type
        (GWindows.Base.Controlling_Parent (Window).all);

      Current_Index    : constant Natural := Current (This.Outline_View) - 1;
      Current_Control  : GNAVI_Window.Control_Element;
      Previous_Control : GNAVI_Window.Control_Element;
   begin
      if Current_Index <= 1 then
         return;
      else
         Current_Control := Control (This.Win_XML, Current_Index);
      end if;

      Previous_Control := Control (This.Win_XML, Current_Index - 1);
      if Previous_Control /= null then
         Insert_Control_Before (This.Win_XML,
                                Current_Control, Previous_Control, null);
      end if;

      Load_Outline_View (This);
      Current (This.Outline_View, Current_Index + 1);

      Text (This.Property_Edit_Box, "");
      Text (This.Handler_Edit_Box, "");
      Reload_Details (This);
   end Do_Control_Right;

end GNAVI_Edit_Window_Package;
