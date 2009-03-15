with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GWindows.GStrings;
with GWindows.Cursors;

with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Core.Documents;

with GNAVI_ICG;
with GNAVI_Window;

with Templates;

with GNAT.Directory_Operations;
with GNAT.IO;

package body GNAVI_Project is

   procedure Load_Project (Project   : in out GNAVI_Project_Type;
                           File_Name : in     GWindows.GString)
   is
      use GNAT.IO;
      use GWindows.GStrings;
   begin
      Open (Project, File_Name);
      Project.File_Name := To_GString_Unbounded (File_Name);
      Project.Load_State := True;
      Project.Dirty_State := False;

   exception
      when E : others =>
         Put_Line ("XML Project Error:");
         Put_Line (Ada.Exceptions.Exception_Name (E));
         Put_Line (Ada.Exceptions.Exception_Message (E));
   end Load_Project;

   procedure Save_Project (Project : in out GNAVI_Project_Type)
   is
      use GWindows.GStrings;
   begin
      Save_As_Project
        (Project, To_GString_From_Unbounded (Project.File_Name));
   end Save_Project;

   procedure Save_As_Project (Project   : in out GNAVI_Project_Type;
                              File_Name : in     GWindows.GString)
   is
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      Project.Dirty_State := False;
      Write (Project, File_Name);
   end Save_As_Project;

   function Project_Name (Project : in GNAVI_Project_Type)
                         return GWindows.GString
   is
      use DOM.Core;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      return GWindows.GStrings.To_GString_From_String
        (Elements.Get_Attribute
           (GNAVI_XML.Get_Child_Node
              (Root (Project), "application"), "name"));
   end Project_Name;

   function Window_Count (Project : in GNAVI_Project_Type) return Natural
   is
      use DOM.Core;

      NL : Node_List := Elements.Get_Elements_By_Tag_Name
        (Root (Project), "window");
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

         return Nodes.Length (NL);
   end Window_Count;

   function Window_Name (Project : in GNAVI_Project_Type;
                         Index   : in Positive)
                        return GWindows.GString
   is
      use GNAVI_Window;

      Win_XML : GNAVI_Window_Type;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      Open (Win_XML, Window_File_Name (Project, Index));

      return Window_Name (Win_XML);
   exception
      when GNAVI_XML.GNAVI_XML_ERROR =>
         return "XML Error:" & Window_File_Name (Project, Index);
   end Window_Name;

   function Window_File_Name  (Project : in GNAVI_Project_Type;
                               Index   : in Positive)
                              return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;

      NL : Node_List := Elements.Get_Elements_By_Tag_Name
        (Root (Project), "window");
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      return To_GString_From_String
        (Elements.Get_Attribute (Nodes.Item (NL, Index - 1), "file"));
   end Window_File_Name;

   procedure Close_Project (Project : in out GNAVI_Project_Type)
   is
   begin
      Project.Load_State := False;
      Project.Dirty_State := False;
   end Close_Project;

   function Is_Loaded (Project : in GNAVI_Project_Type) return Boolean
   is
   begin
      return Project.Load_State;
   end Is_Loaded;

   function Has_Changed (Project : in GNAVI_Project_Type) return Boolean
   is
   begin
      return Project.Dirty_State;
   end Has_Changed;

   procedure Add_Window  (Project   : in out GNAVI_Project_Type;
                          File_Name : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Windows_Node : Node := GNAVI_XML.Get_Child_Node
        (GNAVI_XML.Get_Child_Node (Root (Project), "application"), "windows");

      New_Node     : Node :=
        Documents.Create_Element (Document (Project), "window");
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      New_Node := Nodes.Append_Child (Windows_Node, New_Node);
      Elements.Set_Attribute (New_Node, "file", To_String (File_Name));
      Project.Dirty_State := True;
      Save_Project (Project);
      Run_ICG (Project);
   end Add_Window;

   procedure Delete_Window (Project : in out GNAVI_Project_Type;
                            Index   : in     Positive)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Windows_Node : Node;
      NL           : Node_List;
      Old_Node     : Node;
      Success      : Boolean;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      Windows_Node := GNAVI_XML.Get_Child_Node
        (GNAVI_XML.Get_Child_Node (Root (Project), "application"),
         "windows");

      NL := Elements.Get_Elements_By_Tag_Name (Root (Project), "window");

      Old_Node :=
        Nodes.Remove_Child (Windows_Node, Nodes.Item (NL, Index - 1));

      Nodes.Free (Old_Node);

      Save_Project (Project);

      GNAT.OS_Lib.Delete_File
        (To_String(Project_Name (Project) & ".adb"), Success);
   end Delete_Window;

   procedure Run_ICG (Project : in out GNAVI_Project_Type)
   is
      use Ada.Exceptions;
      use GNAT.IO;
      use GNAVI_ICG;
      use Ada.Strings.Unbounded;
      use GWindows.GStrings;
      use GNAT.Directory_Operations;

      ICG_Project : GNAVI_ICG.GNAVI_Project_Type;
   begin
      GWindows.Cursors.Start_Wait_Cursor;

      Templates.Template_Dir (Dir_Name (ICG_Path_Exists.all) & "templates\");

      ICG_Project.Application_File := To_Unbounded_String
        (To_String (To_GString_From_Unbounded (Project.File_Name)));

      ICG_Project.Project_Document := Document (Project);
      ICG_Project.Project_Root     := Root (Project);

      Update_Project (ICG_Project);

      GWindows.Cursors.End_Wait_Cursor;
   exception
      when E : others =>
         Put_Line ("ICG Exception:");
         Put_Line (Ada.Exceptions.Exception_Name (E));
         Put_Line (Ada.Exceptions.Exception_Message (E));
   end Run_ICG;

   procedure Compile (Project : in out GNAVI_Project_Type)
   is
      use GNAT.OS_Lib;
      use GWindows.GStrings;

      N : Integer;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      N := Spawn (GNATMAKE_Path_Exists.all, Argument_String_To_List
                  (To_String("-gnatf " & Project_Name (Project))).all);
   exception
      when others=>
         null;
   end Compile;

   procedure Run (Project : in out GNAVI_Project_Type)
   is
      use GNAT.OS_Lib;
      use GWindows.GStrings;

      N : Process_Id;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      N := Non_Blocking_Spawn (To_String(Project_Name (Project)),
                               Argument_String_To_List ("").all);
   exception
      when others=>
         null;
   end Run;

   function File_Count (Project : in GNAVI_Project_Type) return Natural
   is
      use DOM.Core;

      NL : Node_List := Elements.Get_Elements_By_Tag_Name
        (Root (Project), "project_file");
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

         return Nodes.Length (NL);
   end File_Count;

   function File_Name (Project : in GNAVI_Project_Type;
                         Index   : in Positive)
                      return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;

      NL : Node_List := Elements.Get_Elements_By_Tag_Name
        (Root (Project), "project_file");
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      return To_GString_From_String
        (Elements.Get_Attribute (Nodes.Item (NL, Index - 1), "file"));
   end File_Name;

   procedure Add_File  (Project   : in out GNAVI_Project_Type;
                        File_Name : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Application_Node : Node := GNAVI_XML.Get_Child_Node
        (Root (Project), "application");

      Project_Files_Node : Node := GNAVI_XML.Get_Child_Node
        (Application_Node, "Project_Files");

      New_Node     : Node :=
        Documents.Create_Element (Document (Project), "project_file");
   begin
      if Project_Files_Node = null then
         Project_Files_Node := Documents.Create_Element
           (Document (Project), "project_files");
         Project_Files_Node := Nodes.Append_Child (Application_Node,
                                                   Project_Files_Node);
      end if;

      New_Node := Nodes.Append_Child (Project_Files_Node, New_Node);
      Elements.Set_Attribute (New_Node, "file", To_String (File_Name));
      Project.Dirty_State := True;
      Save_Project (Project);
   end Add_File;

   procedure Delete_File (Project : in out GNAVI_Project_Type;
                          Index   : in     Positive)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Project_Files_Node : Node;
      NL                 : Node_List;
      Old_Node           : Node;
   begin
      if not Project.Load_State then
         raise No_Project_Loaded;
      end if;

      Project_Files_Node := GNAVI_XML.Get_Child_Node
        (GNAVI_XML.Get_Child_Node (Root (Project), "application"),
         "project_files");

      NL := Elements.Get_Elements_By_Tag_Name (Root (Project),
                                               "project_file");

      Old_Node :=
        Nodes.Remove_Child (Project_Files_Node, Nodes.Item (NL, Index - 1));

      Nodes.Free (Old_Node);

      Save_Project (Project);
   end Delete_File;

end GNAVI_Project;
