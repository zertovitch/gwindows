with Ada.Text_IO;

with Templates_Parser;

with GNAVI_Datastore;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with GWindows.GStrings;

package body GNAVI_Window_Classes is
   use DOM.Core;
   use GWindows.GStrings;

   Window_Classes_XML : GNAVI_Datastore.GNAVI_Datastore_Type;
   Class_File         : constant GWindows.GString := "window_classes.xml";
   Window_List        : DOM.Core.Node_List;

   procedure Init
   is
   begin
      GNAVI_Datastore.Open (Window_Classes_XML, Class_File);
      Window_List :=
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_Datastore.Root (Window_Classes_XML), "window_class");
   end Init;

   function Count return Natural
   is
   begin
      return Nodes.Length (Window_List);
   end Count;

   function Display_Name (Index : in Positive)
                         return GWindows.GString
   is
      N : constant Node := Nodes.Item (Window_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "display_name"));
   end Display_Name;

   function Description (Index : in Positive)
                        return GWindows.GString
   is
      N : constant Node := Nodes.Item (Window_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "description"));
   end Description;

   function Template (Index : in Positive)
                     return GWindows.GString
   is
      N : constant Node := Nodes.Item (Window_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "template"));
   end Template;

   function Window_Type (Index : in Positive)
                        return GWindows.GString
   is
      N : constant Node := Nodes.Item (Window_List, Index - 1);
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (N,
                                 "type"));
   end Window_Type;

   procedure Generate_Window (Index          : in Positive;
                              Window_Name    : in GWindows.GString;
                              Project_Name   : in GWindows.GString  := "";
                              Directory_Name : in GWindows.GString  := "")
   is
   pragma Unreferenced (Project_Name);
      use Templates_Parser;
      use Ada.Text_IO;

      O_File : File_Type;

      Trans : constant Translate_Table :=
        (Assoc ("Window_Name", To_String (Window_Name)),
         Assoc ("Window_Type", To_String (Window_Type (Index))));

      File_Name : GWindows.GString := Directory_Name & Window_Name & ".gnw";
   begin
      To_Lower (File_Name);

      Create (O_File, Out_File, To_String (File_Name));

      Put (O_File,
           Parse
             (Filename     => To_String (GNAVI_Datastore.Directory
                (Window_Classes_XML) & Template (Index)),
              Translations => Trans));

      Close (O_File);

   end Generate_Window;

end GNAVI_Window_Classes;
