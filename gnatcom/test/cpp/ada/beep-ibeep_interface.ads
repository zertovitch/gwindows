with GNATCOM.Interface;

package Beep.IBeep_Interface is

   type IBeep_Type is
     new GNATCOM.Interface.Interface_Type with null record;

   procedure Initialize (This : in out IBeep_Type);

   function  Pointer (This : in IBeep_Type) return Pointer_To_IBeep;

   procedure Attach (This    : in out IBeep_Type;
                     Pointer : in     Pointer_To_IBeep);

   procedure Beep (This : IBeep_Type);

   procedure Display (This : IBeep_Type;
                      text : GNATCOM.Types.BSTR;
                      Free : Boolean            := True);

   function GetText (This : IBeep_Type) return GNATCOM.Types.BSTR;

   function SetGetText (This   : IBeep_Type;
                        inText : GNATCOM.Types.BSTR;
                        Free   : Boolean            := True)
     return GNATCOM.Types.BSTR;

   function InDoubleOut (This     : IBeep_Type;
                         inText   : GNATCOM.Types.BSTR;
                         out1Text : GNATCOM.Types.Pointer_To_BSTR;
                         Free     : Boolean                       := True)
     return GNATCOM.Types.BSTR;

   function InDoubleOutVar
     (This    : IBeep_Type;
      inVar   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Var : GNATCOM.Types.Pointer_To_VARIANT;
      Free    : Boolean                          := True)
     return GNATCOM.Types.VARIANT;

end Beep.IBeep_Interface;

