with Ada.Text_IO;

with Templates_Parser;

with GNAVI_Datastore;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with GWindows.GStrings;

package body GNAVI_Project_Classes is
   use DOM.Core;
   use GWindows.GStrings;

   Project_Classes_XML : GNAVI_Datastore.GNAVI_Datastore_Type;
   Class_File          : constant GWindows.GString := "project_classes.xml";
   Project_List        : DOM.Core.Node_List;

   procedure Init
   is
   begin
      GNAVI_Datastore.Open (Project_Classes_XML, Class_File);

      Project_List :=
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_Datastore.Root (Project_Classes_XML), "project_class");
   end Init;

   function Count return Natural
   is
   begin
      return Nodes.Length (Project_List);
   end Count;

   function Display_Name (Index : in Positive)
                         return GWindows.GString
   is
      N : constant Node := Nodes.Item (Project_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "display_name"));
   end Display_Name;

   function Description (Index : in Positive)
                        return GWindows.GString
   is
      N : constant Node := Nodes.Item (Project_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "description"));
   end Description;

   function Template (Index : in Positive)
                     return GWindows.GString
   is
      N : constant Node := Nodes.Item (Project_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "template"));
   end Template;

   procedure Generate_Project (Index          : in Positive;
                               Project_Name   : in GWindows.GString;
                               Directory_Name : in GWindows.GString  := "")
   is
      use Templates_Parser;
      use Ada.Text_IO;
      use GWindows.GStrings;

      O_File : File_Type;

      Trans : constant Translate_Table :=
        ( 1 => Assoc ("Project_Name", To_String (Project_Name)));

      File_Name : GWindows.GString := Directory_Name & Project_Name & ".gnp";
   begin
      To_Lower (File_Name);

      Create (O_File, Out_File, To_String (File_Name));

      Put (O_File,
           Parse
             (Filename     => To_String (GNAVI_Datastore.Directory
                (Project_Classes_XML) & Template (Index)),
              Translations =>Trans));

      Close (O_File);
   end Generate_Project;

end GNAVI_Project_Classes;
