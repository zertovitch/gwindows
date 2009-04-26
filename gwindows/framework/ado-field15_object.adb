package body ADO.Field15_Object is

   function Get_Properties
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Properties);
   end Get_Properties;

   function Get_ActualSize
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_ActualSize);
   end Get_ActualSize;

   function Get_Attributes
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Attributes);
   end Get_Attributes;

   function Get_DefinedSize
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_DefinedSize);
   end Get_DefinedSize;

   function Get_Name
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Name);
   end Get_Name;

   function Get_Type
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Type);
   end Get_Type;

   function Get_Value
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Value);
   end Get_Value;

   procedure Put_Value
     (This : Field15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field15_Put_Value,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Value;

   function Get_Precision
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_Precision);
   end Get_Precision;

   function Get_NumericScale
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_NumericScale);
   end Get_NumericScale;

   procedure AppendChunk
     (This : Field15_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         Field15_AppendChunk,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Data),
         Free);
   end AppendChunk;

   function GetChunk
     (This   : Field15_Type;
      Length : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         Field15_GetChunk,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Length),
         Free);
   end GetChunk;

   function Get_OriginalValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_OriginalValue);
   end Get_OriginalValue;

   function Get_UnderlyingValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field15_Get_UnderlyingValue);
   end Get_UnderlyingValue;

end ADO.Field15_Object;
