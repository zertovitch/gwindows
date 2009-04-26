package body ADO.Property_Object is

   function Get_Value
     (This : Property_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Property_Get_Value);
   end Get_Value;

   procedure Put_Value
     (This : Property_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Property_Put_Value,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Value;

   function Get_Name
     (This : Property_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Property_Get_Name);
   end Get_Name;

   function Get_Type
     (This : Property_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Property_Get_Type);
   end Get_Type;

   function Get_Attributes
     (This : Property_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Property_Get_Attributes);
   end Get_Attributes;

   procedure Put_Attributes
     (This : Property_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Property_Put_Attributes,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Attributes;

end ADO.Property_Object;
