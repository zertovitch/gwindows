with GNATCOM.Iinterface;

package ADO.ADOConnectionConstruction15_Interface is

   type ADOConnectionConstruction15_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ADOConnectionConstruction15_Type);

   function Pointer (This : ADOConnectionConstruction15_Type)
     return Pointer_To_ADOConnectionConstruction15;

   procedure Attach (This    : in out ADOConnectionConstruction15_Type;
                     Pointer : in     Pointer_To_ADOConnectionConstruction15);

   function Get_DSO
     (This  : ADOConnectionConstruction15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function Get_Session
     (This      : ADOConnectionConstruction15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure WrapDSOandSession
     (This     : ADOConnectionConstruction15_Type;
      pDSO     : GNATCOM.Types.Pointer_To_IUnknown;
      pSession : GNATCOM.Types.Pointer_To_IUnknown);

end ADO.ADOConnectionConstruction15_Interface;
