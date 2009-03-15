with Interfaces.C;

with GNATCOM.Types;
with GNATCOM.GUID;
with GNATCOM.Errors;
with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.Interface; use GNATCOM.Interface;

with GNAT.IO; use GNAT.IO;

procedure RBeep_Test is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   IDispatch : Dispinterface_Type;
   Other     : Interface_Type;
   IID_None  : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{45393DF0-54B9-11CF-92A2-00AA00B8A733}");
begin
   Put_Line ("-- Start Interface_Test");
   New_Line;

   Initialize_COM;

   Put_Line ("Test   : Create Dispatch Object using PROGID");
   CreateRemote (IDispatch, "BeepLibrary.BeepClass", "10.183.24.96");
   Put_Line ("++ PASS");

   Put_Line ("Test   : Get ID of Name");
   declare
      use type Interfaces.C.long;
      DispID : Interfaces.C.long :=
        Get_DISPID (IDispatch, "beep");
   begin
      if DispID = 1 then
         Put_Line ("++ PASS");

         Put_Line ("Test   : Execute with ID");
         Invoke (IDispatch, DispID); -- ++ PASS not displayed on LocalServer
      else
         Put_Line ("++ FAIL");
      end if;
   end;

   Put_Line ("Test   : Test QueryInterface on non implemented IID");
   begin
      Set_IID (Other, IID_None);
      Query (Other, IDispatch);
      Put_Line ("++ FAIL");
   exception
      when GNATCOM.Errors.No_Interface_Error =>
         Put_Line ("++ PASS");
   end;

   Put_Line ("-- Completed Interface_Test");
   New_Line;

end RBeep_Test;

