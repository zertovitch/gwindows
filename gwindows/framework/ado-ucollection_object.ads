with GNATCOM.Dispinterface;

package ADO.uCollection_Object is

   type uCollection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : uCollection_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : uCollection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : uCollection_Type);

end ADO.uCollection_Object;
