with Ada.Strings.Unbounded;
with GNATCOM.Create.Remote_Register;

procedure GNATExample.Remote is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("gnatexamplerc.coff");

   Object_Map : aliased GNATCOM.Create.Remote_Register.Factory_Record_Array :=
     (1 => (CLSID_GNATCOMClass,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMLibrary.GNATCOMClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMClass")));

begin
   GNATCOM.Create.Remote_Register.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Remote_Register.Init_Object (LIBID_GNATCOMLibrary);
end GNATExample.Remote;

