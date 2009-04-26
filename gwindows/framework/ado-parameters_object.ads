with GNATCOM.Dispinterface;

package ADO.Parameters_Object is

   type Parameters_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : Parameters_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : Parameters_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : Parameters_Type);

   procedure Append
     (This   : Parameters_Type;
      Object : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure Delete
     (This  : Parameters_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_Item
     (This  : Parameters_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.Parameters_Object;
