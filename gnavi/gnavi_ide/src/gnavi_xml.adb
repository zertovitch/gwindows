with Ada.Text_IO;
with Ada.Exceptions;

with GWindows.GStrings;

with DOM.Core.Documents;
with DOM.Core.Nodes;

package body GNAVI_XML is

   procedure Open (Object    : in out GNAVI_XML_Type;
                   File_Name : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;
      use Ada.Text_IO;
   begin
      Object.File_Name := To_GString_Unbounded (File_Name);
      Input_Sources.File.Open (To_String (File_Name),
                               Object.XML_File);
      DOM.Readers.Parse (Object.XML_Tree, Object.XML_File);
      Object.XML_Document := DOM.Readers.Get_Tree (Object.XML_Tree);
      Object.XML_Root := Documents.Get_Element (Object.XML_Document);
   exception
      when E : others =>
         Put_Line ("XML Error in : " & To_String (File_Name));
         Put_Line (Ada.Exceptions.Exception_Name (E));
         Put_Line (Ada.Exceptions.Exception_Message (E));
         raise GNAVI_XML_ERROR;
   end Open;

   procedure Close (Object : in out GNAVI_XML_Type)
   is
   begin
      DOM.Readers.Free (Object.XML_Tree);
      Input_Sources.File.Close (Object.XML_File);
   end Close;

   procedure Finalize (Object : in out GNAVI_XML_Type)
   is
   begin
      Close (Object);
   end Finalize;

   function Document (Object : in GNAVI_XML_Type) return DOM.Core.Document
   is
   begin
      return Object.XML_Document;
   end Document;

   function Root (Object : in GNAVI_XML_Type) return DOM.Core.Element
   is
   begin
      return Object.XML_Root;
   end Root;

   procedure Write (Object    : in out GNAVI_XML_Type;
                    File_Name : in     GWindows.GString)
   is
      use Ada.Text_IO;

      O_File : File_Type;
   begin
      Create (O_File, Out_File, GWindows.GStrings.To_String (File_Name));
      DOM.Core.Nodes.Print (O_File, Root (Object));
      Close (O_File);
   end Write;

   function Get_Child_Node (Parent_Node : in DOM.Core.Node;
                            Child_Name  : in GWindows.GString)
                           return DOM.Core.Node
   is
      use DOM.Core;

      C_Name : constant String := GWindows.GStrings.To_String (Child_Name);

      NL : constant Node_List := Nodes.Child_Nodes (Parent_Node);
   begin
      for NI in 1 .. Nodes.Length (NL) loop
         declare
            N : constant Node := Nodes.Item (NL, NI - 1);
         begin
            if Nodes.Node_Name (N) = C_Name then
               return N;
            end if;
         end;
      end loop;

      return null;
   end Get_Child_Node;

   procedure Refresh (Object : in out GNAVI_XML_Type)
   is
      use GWindows.GStrings;
   begin
      Close (Object);
      Open (Object, To_GString_From_Unbounded (Object.File_Name));
   end Refresh;

   procedure Write (Object : in out GNAVI_XML_Type)
   is
      use GWindows.GStrings;
   begin
      Write (Object, To_GString_From_Unbounded (Object.File_Name));
   end Write;

end GNAVI_XML;
