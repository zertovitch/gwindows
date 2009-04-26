with GNATCOM.Dispinterface;

package ADO.Fields_Object is

   type Fields_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : Fields_Type)
     return GNATCOM.Types.VARIANT;

   function uNewEnum
     (This : Fields_Type)
     return GNATCOM.Types.VARIANT;

   procedure Refresh
     (This : Fields_Type);

   function Get_Item
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Append
     (This        : Fields_Type;
      Name        : GNATCOM.Types.VARIANT;
      uType       : GNATCOM.Types.VARIANT;
      DefinedSize : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Attrib      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free        : Boolean := True);

   procedure Delete
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

end ADO.Fields_Object;
