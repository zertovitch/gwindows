package body ADO.Field_Object is

   function Get_Properties
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Properties);
   end Get_Properties;

   function Get_ActualSize
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_ActualSize);
   end Get_ActualSize;

   function Get_Attributes
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Attributes);
   end Get_Attributes;

   function Get_DefinedSize
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_DefinedSize);
   end Get_DefinedSize;

   function Get_Name
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Name);
   end Get_Name;

   function Get_Type
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Type);
   end Get_Type;

   function Get_Value
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Value);
   end Get_Value;

   procedure Put_Value
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_Value,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Value;

   function Get_Precision
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_Precision);
   end Get_Precision;

   function Get_NumericScale
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_NumericScale);
   end Get_NumericScale;

   procedure AppendChunk
     (This : Field_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         Field_AppendChunk,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Data),
         Free);
   end AppendChunk;

   function GetChunk
     (This   : Field_Type;
      Length : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         Field_GetChunk,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Length),
         Free);
   end GetChunk;

   function Get_OriginalValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_OriginalValue);
   end Get_OriginalValue;

   function Get_UnderlyingValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_UnderlyingValue);
   end Get_UnderlyingValue;

   function Get_DataFormat
     (This : Field_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Field_Get_DataFormat);
   end Get_DataFormat;

   procedure PutRef_DataFormat
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         Field_PutRef_DataFormat,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_DataFormat;

   procedure Put_Precision
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_Precision,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Precision;

   procedure Put_NumericScale
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_NumericScale,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_NumericScale;

   procedure Put_Type
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_Type,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Type;

   procedure Put_DefinedSize
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_DefinedSize,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_DefinedSize;

   procedure Put_Attributes
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         Field_Put_Attributes,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Attributes;

end ADO.Field_Object;
