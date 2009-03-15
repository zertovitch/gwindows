with GNATCOM.VARIANT;

package body Beep.IBeep_Object is

   procedure Beep (This : IBeep_Type) is
   begin
      Invoke (This, ID_IBeep_Beep);
   end Beep;

   procedure Display
     (This : IBeep_Type;
      text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean               := True)
   is
   begin
      Invoke
        (This,
         ID_IBeep_Display,
         GNATCOM.Dispinterface.Parameter_Array'(1 => text));

      if Free then
         GNATCOM.VARIANT.Free (text);
      end if;
   end Display;

   function GetText
     (This : IBeep_Type)
      return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ID_IBeep_GetText);
   end GetText;

   function SetGetText
     (This   : IBeep_Type;
      inText : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean               := True)
      return GNATCOM.Types.VARIANT
   is
      Result : GNATCOM.Types.VARIANT;
   begin
      Result := Invoke
        (This, ID_IBeep_SetGetText,
         GNATCOM.Dispinterface.Parameter_Array'(1 => inText));

      if Free then
         GNATCOM.VARIANT.Free (inText);
      end if;

      return Result;
   end SetGetText;

   function InDoubleOut
     (This     : IBeep_Type;
      inText   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean               := True)
      return GNATCOM.Types.VARIANT
   is
      Result : GNATCOM.Types.VARIANT;
   begin
      Result := Invoke
        (This,
         ID_IBeep_InDoubleOut,
         GNATCOM.Dispinterface.Parameter_Array'(1 => out1Text,
                                                2 => inText));
      if Free then
         GNATCOM.VARIANT.Free (inText);
      end if;

      return Result;
   end InDoubleOut;

   function InDoubleOutVar
     (This     : IBeep_Type;
      inText   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Text : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean               := True)
      return GNATCOM.Types.VARIANT
   is
      use GNATCOM.VARIANT;

      Result : GNATCOM.Types.VARIANT;
   begin
      Result := Invoke
        (This,
         ID_IBeep_InDoubleOutVar,
         GNATCOM.Dispinterface.Parameter_Array'(1 => out1Text,
                                                2 => inText));
      if Free then
         GNATCOM.VARIANT.Free (inText);
      end if;

      return Result;
   end InDoubleOutVar;

end Beep.IBeep_Object;

