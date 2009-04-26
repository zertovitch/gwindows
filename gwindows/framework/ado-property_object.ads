with GNATCOM.Dispinterface;

package ADO.Property_Object is

   type Property_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Value
     (This : Property_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Property_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Name
     (This : Property_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Type
     (This : Property_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Attributes
     (This : Property_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Attributes
     (This : Property_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end ADO.Property_Object;
