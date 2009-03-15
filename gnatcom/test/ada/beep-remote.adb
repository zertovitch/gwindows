with Ada.Strings.Unbounded;
with GNATCOM.Create.Remote_Register;

procedure beep.Remote is
   pragma Linker_Options ("beeprc.coff");

   Object_Map : aliased GNATCOM.Create.Remote_Register.Factory_Record_Array :=
     (1 => (CLSID_BeepClass,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepLibrary.BeepClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepClass")));

begin
   GNATCOM.Create.Remote_Register.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Remote_Register.Init_Object (LIBID_BeepLibrary);
end beep.Remote;

