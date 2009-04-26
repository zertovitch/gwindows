with GNATCOM.Dispinterface;

package ADO.Errors_Object is

   type Errors_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : Errors_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : Errors_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : Errors_Type);

   function Get_Item
     (This  : Errors_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Clear
     (This : Errors_Type);

end ADO.Errors_Object;
