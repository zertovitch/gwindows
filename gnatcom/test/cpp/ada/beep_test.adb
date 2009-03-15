with Interfaces.C;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.Types; use GNATCOM.Types;

with Beep.IBeep_Interface;
with Beep.IBeep_Object;
use Beep;

with GNAT.IO; use GNAT.IO;

procedure Beep_Test is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   IBeep : IBeep_Interface.IBeep_Type;
   Beep2 : IBeep_Interface.IBeep_Type;

begin
   Put_Line ("-- Start Interface_Test");
   New_Line;

   Initialize_COM;

   Put_Line ("Test   : Create COM Object using CLSID");
   IBeep_Interface.Create (IBeep, CLSID_BeepClass);
   Put_Line ("++ PASS");

   Put_Line ("Test   : Execute IBeep::Beep()");
   IBeep_Interface.Beep (IBeep);

   Put_Line ("Test   : Query new interface from created object");
   IBeep_Interface.Query (Beep2, IBeep);
   IBeep_Interface.Beep (Beep2);

   declare
      IBeep    : IBeep_Interface.IBeep_Type;
      OutBSTR  : aliased GNATCOM.Types.BSTR;
      OutVar   : aliased GNATCOM.Types.VARIANT;
   begin
      Put_Line ("Test   : Create COM Object using PROGID");
      IBeep_Interface.Create (IBeep, "BeepLibrary.BeepClass");
      IBeep_Interface.Beep (IBeep);

      Put_Line ("Test   : RetVals");
      Put_Line (GNATCOM.BSTR.To_Ada (IBeep_Interface.InDoubleOut
                                     (IBeep,
                                      To_BSTR ("++ PASS"),
                                      OutBSTR'Unchecked_Access)));

      Put_Line ("Test   : OutParams");
      if GNATCOM.BSTR.To_Ada (OutBSTR) = "++ PASS" then
         Put_Line ("++ PASS");
      else
         Put_Line ("++ FAIL");
      end if;

      Put_Line ("Test   : RetVals with Variants");

      GNATCOM.VARIANT.Initialize (OutVar);
      Put_Line (To_Ada (IBeep_Interface.InDoubleOutVar
                        (IBeep,
                         To_VARIANT ("++ PASS"),
                         OutVar'Unchecked_Access)));

      Put_Line ("Test   : OutParams");
      if GNATCOM.VARIANT.To_Ada (OutVar) = "++ PASS" then
         Put_Line ("++ PASS");
      else
         Put_Line ("++ FAIL");
      end if;
   end;

   declare
      IDispatch : IBeep_Object.IBeep_Type;
      OutBSTR   : aliased GNATCOM.Types.BSTR;
      OutVar    : aliased GNATCOM.Types.VARIANT;
   begin
      Put_Line ("Test   : Create Dispatch Object using PROGID");
      IBeep_Object.Create (IDispatch, "BeepLibrary.BeepClass");
      Put_Line ("++ PASS");

      Put_Line ("Test   : Get ID of Name");
      declare
         use type Interfaces.C.long;

         DispID : Interfaces.C.long := IBeep_Object.Get_DISPID
           (IDispatch, "beep");
      begin
         if DispID = 1 then
            Put_Line ("++ PASS");

            Put_Line ("Test   : Execute with ID");
            IBeep_Object.Invoke (IDispatch,
                                 DispID);
         else
            Put_Line ("++ FAIL");
         end if;
      end;

      Put_Line ("Test   : Execute using thick binding");
      IBeep_Object.Beep (IDispatch);

      Put_Line ("Test   : Get of data");
      Put_Line (To_Ada (IBeep_Object.GetText (IDispatch)));

      Put_Line ("Test   : SetGet of data");
      Put_Line (To_Ada (IBeep_Object.SetGetText (IDispatch,
                                                 To_VARIANT ("++ PASS"))));

      Put_Line ("Test   : Test Out BSTR result");
      Put_Line (To_Ada (IBeep_Object.InDoubleOut
                        (IDispatch,
                         To_VARIANT ("++ PASS"),
                         To_VARIANT_BYREF (OutBSTR'Access))));

      Put_Line ("Test   : Test Out BSTR param");
      Put_Line (GNATCOM.BSTR.To_Ada (OutBSTR));

      Put_Line ("Test   : Test Out VAR result");
      Put_Line (To_Ada (IBeep_Object.InDoubleOutVar
                        (IDispatch,
                         To_VARIANT ("++ PASS"),
                         To_VARIANT_BYREF (OutVar'Access))));

      Put_Line ("Test   : Test Out VAR param");
      Put_Line (GNATCOM.VARIANT.To_Ada (OutVar));
   end;

   Put_Line ("-- Completed Interface_Test");
   New_Line;

end Beep_Test;

