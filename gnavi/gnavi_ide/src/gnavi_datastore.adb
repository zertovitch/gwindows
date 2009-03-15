with Ada.Command_Line;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with GWindows.GStrings;

package body GNAVI_Datastore is

   IDE_Path : GNAT.OS_Lib.String_Access := null;

   function Datastore_Dir return GWindows.GString;
   --  Returns the datastore directoy

   function IDE_Dir return GWindows.GString is
      use GNAT.OS_Lib;
      use GNAT.Directory_Operations;
   begin
      if IDE_Path = null then
         IDE_Path :=
           GNAT.OS_Lib.Locate_Exec_On_Path (Ada.Command_Line.Command_Name);

         if IDE_Path = null then
            IDE_Path :=
              new String'(Get_Current_Dir & Ada.Command_Line.Command_Name);
         end if;
      end if;

      return GWindows.GStrings.To_GString_From_String
        (GNAT.Directory_Operations.Dir_Name (IDE_Path.all));
   end IDE_Dir;

   function Datastore_Dir return GWindows.GString
   is
   begin
      return IDE_Dir & "datastore\";
   end Datastore_Dir;

   procedure Open (Object    : in out GNAVI_Datastore_Type;
                   File_Name : in     GWindows.GString)
   is
   begin
      GNAVI_XML.Open (GNAVI_XML.GNAVI_XML_Type (Object),
            Datastore_Dir & File_Name);
   end Open;

   function Directory (Object : in GNAVI_Datastore_Type)
                      return GWindows.GString
   is
      pragma Warnings (Off, Object);
   begin
      return Datastore_Dir;
   end Directory;

end GNAVI_Datastore;
