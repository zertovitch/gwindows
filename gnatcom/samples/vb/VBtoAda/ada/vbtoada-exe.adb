with vbtoada.VBtoAdaClass;

with Ada.Strings.Unbounded;
with GNATCOM.Create.Local_Server;

procedure vbtoada.Exe is
   pragma Linker_Options ("vbtoadarc.coff");

   Object_Map : aliased GNATCOM.Create.Local_Server.Factory_Record_Array :=
     (1 => (CLSID_VBtoAdaClass,
            VBtoAdaClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaLibrary.VBtoAdaClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("VBtoAdaClass"),
            null,
            0));

begin
   GNATCOM.Create.Local_Server.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Local_Server.Init_Object (LIBID_VBtoAdaLibrary);
end vbtoada.Exe;

