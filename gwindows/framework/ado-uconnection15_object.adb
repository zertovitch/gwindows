package body ADO.uConnection15_Object is

   function Get_Properties
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Properties);
   end Get_Properties;

   function Get_ConnectionString
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_ConnectionString);
   end Get_ConnectionString;

   procedure Put_ConnectionString
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_ConnectionString,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ConnectionString;

   function Get_CommandTimeout
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_CommandTimeout);
   end Get_CommandTimeout;

   procedure Put_CommandTimeout
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_CommandTimeout,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CommandTimeout;

   function Get_ConnectionTimeout
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_ConnectionTimeout);
   end Get_ConnectionTimeout;

   procedure Put_ConnectionTimeout
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_ConnectionTimeout,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ConnectionTimeout;

   function Get_Version
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Version);
   end Get_Version;

   procedure Close
     (This : uConnection15_Type)
   is
   begin
      Invoke (This, uConnection15_Close);
   end Close;

   function Execute
     (This            : uConnection15_Type;
      CommandText     : GNATCOM.Types.VARIANT;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uConnection15_Execute,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => RecordsAffected,
          3 => CommandText),
         Free);
   end Execute;

   function BeginTrans
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, uConnection15_BeginTrans);
   end BeginTrans;

   procedure CommitTrans
     (This : uConnection15_Type)
   is
   begin
      Invoke (This, uConnection15_CommitTrans);
   end CommitTrans;

   procedure RollbackTrans
     (This : uConnection15_Type)
   is
   begin
      Invoke (This, uConnection15_RollbackTrans);
   end RollbackTrans;

   procedure Open
     (This             : uConnection15_Type;
      ConnectionString : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      UserID           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Password         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options          : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free             : Boolean := True)
   is
   begin
      Invoke
        (This,
         uConnection15_Open,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => Password,
          3 => UserID,
          4 => ConnectionString),
         Free);
   end Open;

   function Get_Errors
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Errors);
   end Get_Errors;

   function Get_DefaultDatabase
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_DefaultDatabase);
   end Get_DefaultDatabase;

   procedure Put_DefaultDatabase
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_DefaultDatabase,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_DefaultDatabase;

   function Get_IsolationLevel
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_IsolationLevel);
   end Get_IsolationLevel;

   procedure Put_IsolationLevel
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_IsolationLevel,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_IsolationLevel;

   function Get_Attributes
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Attributes);
   end Get_Attributes;

   procedure Put_Attributes
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_Attributes,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Attributes;

   function Get_CursorLocation
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_CursorLocation);
   end Get_CursorLocation;

   procedure Put_CursorLocation
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_CursorLocation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CursorLocation;

   function Get_Mode
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Mode);
   end Get_Mode;

   procedure Put_Mode
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_Mode,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Mode;

   function Get_Provider
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_Provider);
   end Get_Provider;

   procedure Put_Provider
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uConnection15_Put_Provider,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Provider;

   function Get_State
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uConnection15_Get_State);
   end Get_State;

   function OpenSchema
     (This         : uConnection15_Type;
      Schema       : GNATCOM.Types.VARIANT;
      Restrictions : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      SchemaID     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free         : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uConnection15_OpenSchema,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => SchemaID,
          2 => Restrictions,
          3 => Schema),
         Free);
   end OpenSchema;

end ADO.uConnection15_Object;
