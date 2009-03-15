with GNATExample.GNATCOMClass;

with Ada.Strings.Unbounded;
with GNATCOM.Create.Local_Server;

procedure GNATExample.Exe is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("gnatexamplerc.coff");

   Object_Map : aliased GNATCOM.Create.Local_Server.Factory_Record_Array :=
     (1 => (CLSID_GNATCOMClass,
            GNATCOMClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMLibrary.GNATCOMClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMClass"),
            null,
            0));

begin
   GNATCOM.Create.Local_Server.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Local_Server.Init_Object (LIBID_GNATCOMLibrary);
end GNATExample.Exe;

