package body ADO.Fields_Object is

   function Get_Count
     (This : Fields_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Fields_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : Fields_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Fields_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : Fields_Type)
   is
   begin
      Invoke (This, Fields_Refresh);
   end Refresh;

   function Get_Item
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         Fields_Get_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Item;

   procedure Append
     (This        : Fields_Type;
      Name        : GNATCOM.Types.VARIANT;
      uType       : GNATCOM.Types.VARIANT;
      DefinedSize : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Attrib      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         Fields_Append,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Attrib,
          2 => DefinedSize,
          3 => uType,
          4 => Name),
         Free);
   end Append;

   procedure Delete
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         Fields_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Delete;

end ADO.Fields_Object;
