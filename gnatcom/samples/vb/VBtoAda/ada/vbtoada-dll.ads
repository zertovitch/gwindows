with vbtoada.VBtoAdaClass;

with Ada.Strings.Unbounded;
with GNATCOM.Create.Inproc;

package vbtoada.Dll is
   pragma Linker_Options ("vbtoadarc.coff");

   procedure Main;

   Object_Map : aliased GNATCOM.Create.Inproc.Factory_Record_Array :=
     (1 => (CLSID_VBtoAdaClass,
            VBtoAdaClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaLibrary.VBtoAdaClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaClass")));

end vbtoada.Dll;

