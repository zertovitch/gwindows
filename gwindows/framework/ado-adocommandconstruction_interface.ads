with GNATCOM.Iinterface;

package ADO.ADOCommandConstruction_Interface is

   type ADOCommandConstruction_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ADOCommandConstruction_Type);

   function Pointer (This : ADOCommandConstruction_Type)
     return Pointer_To_ADOCommandConstruction;

   procedure Attach (This    : in out ADOCommandConstruction_Type;
                     Pointer : in     Pointer_To_ADOCommandConstruction);

   function Get_OLEDBCommand
     (This           : ADOCommandConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Put_OLEDBCommand
     (This           : ADOCommandConstruction_Type;
      ppOLEDBCommand : GNATCOM.Types.Pointer_To_IUnknown);

end ADO.ADOCommandConstruction_Interface;
