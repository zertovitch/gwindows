with GNATCOM.Dispinterface;

package Beep.IBeep_Object is

   type IBeep_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;


   procedure Beep (This : IBeep_Type);
   --  Make a beep

   procedure Display
     (This : IBeep_Type;
      text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean               := True);
   --  Display text

   function GetText
     (This : IBeep_Type)
     return GNATCOM.Types.VARIANT;
   --  Get text

   function SetGetText
     (This   : IBeep_Type;
      inText : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean               := True)
     return GNATCOM.Types.VARIANT;
   --  Set and Get Text

   function InDoubleOut
     (This     : IBeep_Type;
      inText   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean               := True)
     return GNATCOM.Types.VARIANT;
   --  In and Double Out

   function InDoubleOutVar
     (This     : IBeep_Type;
      inText   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean               := True)
     return GNATCOM.Types.VARIANT;
   --  In and Double Out VARIANT

end Beep.IBeep_Object;

