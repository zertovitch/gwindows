with Ada.Finalization;

with Input_Sources.File;
with DOM.Readers;
with DOM.Core;

with GWindows;

package GNAVI_XML is

   type GNAVI_XML_Type is
     new Ada.Finalization.Limited_Controlled with private;

   procedure Open (Object    : in out GNAVI_XML_Type;
                   File_Name : in     GWindows.GString);
   --  Open XML file

   procedure Close (Object : in out GNAVI_XML_Type);
   --  Close XML file

   procedure Refresh (Object : in out GNAVI_XML_Type);
   --  Reload XML file

   procedure Finalize (Object : in out GNAVI_XML_Type);
   --  Auto Close XML file

   function Document (Object : in GNAVI_XML_Type) return DOM.Core.Document;
   --  Access XML document

   function Root (Object : in GNAVI_XML_Type) return DOM.Core.Element;
   --  Access XML root

   procedure Write (Object : in out GNAVI_XML_Type);
   --  Overwrite XML file

   procedure Write (Object    : in out GNAVI_XML_Type;
                    File_Name : in     GWindows.GString);
   --  Write XML file

   -------------------------------------------------------------------------
   --  XML Utilities
   -------------------------------------------------------------------------

   function Get_Child_Node (Parent_Node : in DOM.Core.Node;
                            Child_Name  : in GWindows.GString)
                           return DOM.Core.Node;

   GNAVI_XML_ERROR : exception;

private
   type GNAVI_XML_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         File_Name    : GWindows.GString_Unbounded;
         XML_File     : Input_Sources.File.File_Input;
         XML_Tree     : DOM.Readers.Tree_Reader;
         XML_Document : DOM.Core.Document;
         XML_Root     : DOM.Core.Element;
      end record;
end GNAVI_XML;
