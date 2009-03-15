with GWindows;
with GNAVI_XML;

package GNAVI_Datastore is

   type GNAVI_Datastore_Type is new GNAVI_XML.GNAVI_XML_Type with private;

   procedure Open (Object    : in out GNAVI_Datastore_Type;
                   File_Name : in     GWindows.GString);
   --  Open XML file in Datastore dir

   function Directory (Object : in GNAVI_Datastore_Type)
                     return GWindows.GString;
   --  Returns location of datastore directory

   function IDE_Dir return GWindows.GString;
   --  Returns directory of IDE

private

   type GNAVI_Datastore_Type is new GNAVI_XML.GNAVI_XML_Type with null record;

end GNAVI_Datastore;
