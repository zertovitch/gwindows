package body ADO.uCommand_Object is

   function Get_Properties
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_Properties);
   end Get_Properties;

   function Get_ActiveConnection
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_ActiveConnection);
   end Get_ActiveConnection;

   procedure PutRef_ActiveConnection
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uCommand_PutRef_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_ActiveConnection;

   procedure Put_ActiveConnection
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ActiveConnection;

   function Get_CommandText
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_CommandText);
   end Get_CommandText;

   procedure Put_CommandText
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_CommandText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandText;

   function Get_CommandTimeout
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_CommandTimeout);
   end Get_CommandTimeout;

   procedure Put_CommandTimeout
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_CommandTimeout,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandTimeout;

   function Get_Prepared
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_Prepared);
   end Get_Prepared;

   procedure Put_Prepared
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_Prepared,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Prepared;

   function Execute
     (This            : uCommand_Type;
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
         uCommand_Execute,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => Parameters,
          3 => RecordsAffected),
         Free);
   end Execute;

   function CreateParameter
     (This      : uCommand_Type;
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
         uCommand_CreateParameter,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value,
          2 => Size,
          3 => Direction,
          4 => uType,
          5 => Name),
         Free);
   end CreateParameter;

   function Get_Parameters
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_Parameters);
   end Get_Parameters;

   procedure Put_CommandType
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_CommandType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandType;

   function Get_CommandType
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_CommandType);
   end Get_CommandType;

   function Get_Name
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_Name);
   end Get_Name;

   procedure Put_Name
     (This : uCommand_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uCommand_Put_Name,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Name;

   function Get_State
     (This : uCommand_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCommand_Get_State);
   end Get_State;

   procedure Cancel
     (This : uCommand_Type)
   is
   begin
      Invoke (This, uCommand_Cancel);
   end Cancel;

end ADO.uCommand_Object;
