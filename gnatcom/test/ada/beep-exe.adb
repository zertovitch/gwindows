with beep.BeepClass;
with Ada.Strings.Unbounded;
with GNATCOM.Create.Local_Server;

procedure beep.Exe is
   pragma Linker_Options ("beeprc.coff");

   Object_Map : aliased GNATCOM.Create.Local_Server.Factory_Record_Array :=
     (1 => (CLSID_BeepClass,
            BeepClass.Create'Access,
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepLibrary.BeepClass"),
            Ada.Strings.Unbounded.To_Unbounded_String ("1"),
            Ada.Strings.Unbounded.To_Unbounded_String
            ("BeepClass"),
            null,
            0));

begin
   GNATCOM.Create.Local_Server.Factory_Map := Object_Map'Unchecked_Access;
   GNATCOM.Create.Local_Server.Init_Object (LIBID_BeepLibrary);
end beep.Exe;

