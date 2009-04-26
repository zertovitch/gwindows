with GNATCOM.Dispinterface;

package ADO.Properties_Object is

   type Properties_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : Properties_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : Properties_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : Properties_Type);

   function Get_Item
     (This  : Properties_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.Properties_Object;
