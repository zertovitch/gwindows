with GNATExample.GNATCOMClass;

with Ada.Strings.Unbounded;
with GNATCOM.Create.Inproc;

package GNATExample.Dll is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("gnatexamplerc.coff");

   procedure Main;

   Object_Map : aliased GNATCOM.Create.Inproc.Factory_Record_Array :=
     (1 => (CLSID_GNATCOMClass,
            GNATCOMClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMLibrary.GNATCOMClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("GNATCOMClass")));

end GNATExample.Dll;

