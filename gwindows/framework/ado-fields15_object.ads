with GNATCOM.Dispinterface;

package ADO.Fields15_Object is

   type Fields15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : Fields15_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : Fields15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : Fields15_Type);

   function Get_Item
     (This  : Fields15_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.Fields15_Object;
