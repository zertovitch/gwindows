package body ADO.uParameter_Object is

   function Get_Properties
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Properties);
   end Get_Properties;

   function Get_Name
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Name);
   end Get_Name;

   procedure Put_Name
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Name,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Name;

   function Get_Value
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Value);
   end Get_Value;

   procedure Put_Value
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Value,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Value;

   function Get_Type
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Type);
   end Get_Type;

   procedure Put_Type
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Type,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Type;

   procedure Put_Direction
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Direction,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Direction;

   function Get_Direction
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Direction);
   end Get_Direction;

   procedure Put_Precision
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Precision,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Precision;

   function Get_Precision
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Precision);
   end Get_Precision;

   procedure Put_NumericScale
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_NumericScale,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_NumericScale;

   function Get_NumericScale
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_NumericScale);
   end Get_NumericScale;

   procedure Put_Size
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Size,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Size;

   function Get_Size
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Size);
   end Get_Size;

   procedure AppendChunk
     (This : uParameter_Type;
      Val  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         uParameter_AppendChunk,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Val),
         Free);
   end AppendChunk;

   function Get_Attributes
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uParameter_Get_Attributes);
   end Get_Attributes;

   procedure Put_Attributes
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uParameter_Put_Attributes,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Attributes;

end ADO.uParameter_Object;
