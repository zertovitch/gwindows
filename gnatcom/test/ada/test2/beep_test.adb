with GNAT.IO; use GNAT.IO;
with GNATCOM.Errors;
with GNATCOM.Initialize; use GNATCOM.Initialize;

with Beep.IBeep_Interface; use Beep.IBeep_Interface;

procedure Beep_Test is
   IBeep : IBeep_Type;
begin
   Initialize_COM;

   Create (IBeep, Beep.CLSID_BeepClass);

   Beep.IBeep_Interface.Beep (IBeep);

   Put_Line ("HR := " & GNATCOM.Errors.Get_Last_HRESULT'Img);
end Beep_Test;
