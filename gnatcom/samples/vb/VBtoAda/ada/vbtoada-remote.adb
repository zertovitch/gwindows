with Ada.Strings.Unbounded;
with GNATCOM.Create.Remote_Register;

procedure vbtoada.Remote is
   pragma Linker_Options ("vbtoadarc.coff");

   Object_Map : aliased GNATCOM.Create.Remote_Register.Factory_Record_Array :=
     (1 => (CLSID_VBtoAdaClass,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaLibrary.VBtoAdaClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaClass")));

begin
   GNATCOM.Create.Remote_Register.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Remote_Register.Init_Object (LIBID_VBtoAdaLibrary);
end vbtoada.Remote;

