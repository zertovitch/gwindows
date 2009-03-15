package body GNATExample.Dll is
   procedure Main is
   begin
      GNATCOM.Create.Inproc.Factory_Map := Object_Map'Access;
      GNATCOM.Create.Inproc.Init_Object (LIBID => LIBID_GNATCOMLibrary);
   end Main;

begin
   Main;
end GNATExample.Dll;
