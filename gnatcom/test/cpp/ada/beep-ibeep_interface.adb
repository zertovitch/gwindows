with GNATCOM.BSTR;
with GNATCOM.VARIANT;
with GNATCOM.Errors;

package body Beep.IBeep_Interface is

   procedure Initialize (This : in out IBeep_Type) is
   begin
      Set_IID (This, IID_IBeep);
   end Initialize;

   function Pointer (This : in IBeep_Type)
     return Pointer_To_IBeep
   is
   begin
      return To_Pointer_To_IBeep (Address (This));
   end Pointer;

   procedure Attach (This : in out IBeep_Type;
                     Pointer : Pointer_To_IBeep)
   is
   begin
      Attach (This, GNATCOM.Interface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure Beep
     (This : IBeep_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Beep (Pointer (This)));
   end Beep;

   procedure Display
     (This : IBeep_Type;
      text : GNATCOM.Types.BSTR;
      Free : Boolean            := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Display (Pointer (This), text));

      if Free then
         GNATCOM.BSTR.Free (text);
      end if;

   end Display;

   function GetText
     (This : IBeep_Type)
     return GNATCOM.Types.BSTR
   is
      text : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetText (Pointer (This),
                                      text'Unchecked_Access));
      return text;
   end GetText;

   function SetGetText
     (This   : IBeep_Type;
      inText : GNATCOM.Types.BSTR;
      Free   : Boolean            := True)
     return GNATCOM.Types.BSTR
   is
      outText : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetGetText (Pointer (This),
                                         inText,
                                         outText'Unchecked_Access));

      if Free then
         GNATCOM.BSTR.Free (inText);
      end if;

      return outText;
   end SetGetText;

   function InDoubleOut
     (This     : IBeep_Type;
      inText   : GNATCOM.Types.BSTR;
      out1Text : GNATCOM.Types.Pointer_To_BSTR;
      Free     : Boolean                       := True)
     return GNATCOM.Types.BSTR
   is
      out2Text : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InDoubleOut (Pointer (This),
                                          inText,
                                          out1Text,
                                          out2Text'Unchecked_Access));
      if Free then
         GNATCOM.BSTR.Free (inText);
      end if;

      return out2Text;
   end InDoubleOut;

   function InDoubleOutVar
     (This    : IBeep_Type;
      inVar   : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      out1Var : GNATCOM.Types.Pointer_To_VARIANT;
      Free    : Boolean                          := True)
     return GNATCOM.Types.VARIANT
   is
      out2Var : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InDoubleOutVar (Pointer (This),
                                             inVar,
                                             out1Var,
                                             out2Var'Unchecked_Access));

      if Free then
         GNATCOM.VARIANT.Free (inVar);
      end if;

      return out2Var;
   end InDoubleOutVar;

end Beep.IBeep_Interface;

