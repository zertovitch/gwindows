with GNATCOM.Errors;

package body ADO.ADOConnectionConstruction15_Interface is

   procedure Initialize (This : in out ADOConnectionConstruction15_Type) is
   begin
      Set_IID (This, IID_ADOConnectionConstruction15);
   end Initialize;

   function Pointer (This : ADOConnectionConstruction15_Type)
     return Pointer_To_ADOConnectionConstruction15
   is
   begin
      return To_Pointer_To_ADOConnectionConstruction15 (Address (This));
   end Pointer;

   procedure Attach (This    : in out ADOConnectionConstruction15_Type;
                     Pointer : in     Pointer_To_ADOConnectionConstruction15)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_DSO
     (This  : ADOConnectionConstruction15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DSO
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DSO;

   function Get_Session
     (This      : ADOConnectionConstruction15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Session
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Session;

   procedure WrapDSOandSession
     (This     : ADOConnectionConstruction15_Type;
      pDSO     : GNATCOM.Types.Pointer_To_IUnknown;
      pSession : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WrapDSOandSession
         (Pointer (This),
          pDSO,
          pSession));

   end WrapDSOandSession;

end ADO.ADOConnectionConstruction15_Interface;
