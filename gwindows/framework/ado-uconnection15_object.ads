with GNATCOM.Dispinterface;

package ADO.uConnection15_Object is

   type uConnection15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_ConnectionString
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ConnectionString
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CommandTimeout
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CommandTimeout
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ConnectionTimeout
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ConnectionTimeout
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Version
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Close
     (This : uConnection15_Type);

   function Execute
     (This            : uConnection15_Type;
      CommandText     : GNATCOM.Types.VARIANT;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function BeginTrans
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure CommitTrans
     (This : uConnection15_Type);

   procedure RollbackTrans
     (This : uConnection15_Type);

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
      Free             : Boolean := True);

   function Get_Errors
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_DefaultDatabase
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_DefaultDatabase
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_IsolationLevel
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_IsolationLevel
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Attributes
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Attributes
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CursorLocation
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CursorLocation
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Mode
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Mode
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Provider
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Provider
     (This : uConnection15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_State
     (This : uConnection15_Type)
     return GNATCOM.Types.VARIANT;

   function OpenSchema
     (This         : uConnection15_Type;
      Schema       : GNATCOM.Types.VARIANT;
      Restrictions : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      SchemaID     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free         : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.uConnection15_Object;
