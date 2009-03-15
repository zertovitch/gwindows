with GNATCOM.BSTR;
with GNATCOM.Utility;

package body vbtoada.VBtoAdaClass is

   function IVBtoAda_Display
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Message : GNATCOM.Types.BSTR)
     return GNATCOM.Types.HRESULT
   is
   begin
      GNATCOM.Utility.Message_Box ("Ada",
                                   GNATCOM.BSTR.To_Ada (Message, False));
      return GNATCOM.S_OK;
   end IVBtoAda_Display;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Create.COM_Interface.Create_Object
        (new VBtoAdaClass_Type);
   end Create;
end vbtoada.VBtoAdaClass;

