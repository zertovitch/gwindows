with beep.BeepClass;
with Ada.Strings.Unbounded;
with GNATCOM.Create.Inproc;

package beep.Dll is
   pragma Linker_Options ("beeprc.coff");

   procedure Main;

   Object_Map : aliased GNATCOM.Create.Inproc.Factory_Record_Array :=
     (1 => (CLSID_BeepClass,
            BeepClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepLibrary.BeepClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepClass")));

end beep.Dll;

