with GNATCOM.Iinterface;

package ADO.ADOConnectionConstruction_Interface is

   type ADOConnectionConstruction_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ADOConnectionConstruction_Type);

   function Pointer (This : ADOConnectionConstruction_Type)
     return Pointer_To_ADOConnectionConstruction;

   procedure Attach (This    : in out ADOConnectionConstruction_Type;
                     Pointer : in     Pointer_To_ADOConnectionConstruction);

   function Get_DSO
     (This  : ADOConnectionConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function Get_Session
     (This      : ADOConnectionConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure WrapDSOandSession
     (This     : ADOConnectionConstruction_Type;
      pDSO     : GNATCOM.Types.Pointer_To_IUnknown;
      pSession : GNATCOM.Types.Pointer_To_IUnknown);

end ADO.ADOConnectionConstruction_Interface;
