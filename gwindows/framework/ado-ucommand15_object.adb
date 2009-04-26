package body ADO.uCommand15_Object is

   function Get_Properties
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_Properties);
   end Get_Properties;

   function Get_ActiveConnection
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_ActiveConnection);
   end Get_ActiveConnection;

   procedure PutRef_ActiveConnection
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uCommand15_PutRef_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_ActiveConnection;

   procedure Put_ActiveConnection
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ActiveConnection;

   function Get_CommandText
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_CommandText);
   end Get_CommandText;

   procedure Put_CommandText
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_CommandText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandText;

   function Get_CommandTimeout
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_CommandTimeout);
   end Get_CommandTimeout;

   procedure Put_CommandTimeout
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_CommandTimeout,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandTimeout;

   function Get_Prepared
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_Prepared);
   end Get_Prepared;

   procedure Put_Prepared
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_Prepared,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Prepared;

   function Execute
     (This            : uCommand15_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Parameters      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uCommand15_Execute,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => Parameters,
          3 => RecordsAffected),
         Free);
   end Execute;

   function CreateParameter
     (This      : uCommand15_Type;
      Name      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      uType     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Direction : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Size      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Value     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uCommand15_CreateParameter,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value,
          2 => Size,
          3 => Direction,
          4 => uType,
          5 => Name),
         Free);
   end CreateParameter;

   function Get_Parameters
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_Parameters);
   end Get_Parameters;

   procedure Put_CommandType
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_CommandType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandType;

   function Get_CommandType
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_CommandType);
   end Get_CommandType;

   function Get_Name
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand15_Get_Name);
   end Get_Name;

   procedure Put_Name
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand15_Put_Name,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Name;

end ADO.uCommand15_Object;
