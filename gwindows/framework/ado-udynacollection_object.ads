with GNATCOM.Dispinterface;

package ADO.uDynaCollection_Object is

   type uDynaCollection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : uDynaCollection_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : uDynaCollection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : uDynaCollection_Type);

   procedure Append
     (This   : uDynaCollection_Type;
      Object : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure Delete
     (This  : uDynaCollection_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

end ADO.uDynaCollection_Object;
