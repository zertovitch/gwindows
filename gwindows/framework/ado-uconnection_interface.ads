with GNATCOM.Dispinterface;

package ADO.uConnection_Interface is

   type uConnection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uConnection_Type);

   function Pointer (This : uConnection_Type)
     return Pointer_To_uConnection;

   procedure Attach (This    : in out uConnection_Type;
                     Pointer : in     Pointer_To_uConnection);

   function Get_Properties
     (This      : uConnection_Type)
     return Pointer_To_Properties;

   function Get_ConnectionString
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_ConnectionString
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_CommandTimeout
     (This      : uConnection_Type)
     return Interfaces.C.long;

   procedure Put_CommandTimeout
     (This      : uConnection_Type;
      plTimeout : Interfaces.C.long);

   function Get_ConnectionTimeout
     (This      : uConnection_Type)
     return Interfaces.C.long;

   procedure Put_ConnectionTimeout
     (This      : uConnection_Type;
      plTimeout : Interfaces.C.long);

   function Get_Version
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR;

   procedure Close
     (This : uConnection_Type);

   function Execute
     (This            : uConnection_Type;
      CommandText     : GNATCOM.Types.BSTR;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Options         : Interfaces.C.long;
      Free            : Boolean := True)
     return Pointer_To_uRecordset;

   function BeginTrans
     (This             : uConnection_Type)
     return Interfaces.C.long;

   procedure CommitTrans
     (This : uConnection_Type);

   procedure RollbackTrans
     (This : uConnection_Type);

   procedure Open
     (This             : uConnection_Type;
      ConnectionString : GNATCOM.Types.BSTR;
      UserID           : GNATCOM.Types.BSTR;
      Password         : GNATCOM.Types.BSTR;
      Options          : Interfaces.C.long;
      Free             : Boolean := True);

   function Get_Errors
     (This      : uConnection_Type)
     return Pointer_To_Errors;

   function Get_DefaultDatabase
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_DefaultDatabase
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_IsolationLevel
     (This  : uConnection_Type)
     return IsolationLevelEnum;

   procedure Put_IsolationLevel
     (This  : uConnection_Type;
      Level : IsolationLevelEnum);

   function Get_Attributes
     (This   : uConnection_Type)
     return Interfaces.C.long;

   procedure Put_Attributes
     (This   : uConnection_Type;
      plAttr : Interfaces.C.long);

   function Get_CursorLocation
     (This        : uConnection_Type)
     return CursorLocationEnum;

   procedure Put_CursorLocation
     (This        : uConnection_Type;
      plCursorLoc : CursorLocationEnum);

   function Get_Mode
     (This   : uConnection_Type)
     return ConnectModeEnum;

   procedure Put_Mode
     (This   : uConnection_Type;
      plMode : ConnectModeEnum);

   function Get_Provider
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Provider
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_State
     (This       : uConnection_Type)
     return Interfaces.C.long;

   function OpenSchema
     (This         : uConnection_Type;
      Schema       : SchemaEnum;
      Restrictions : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      SchemaID     : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free         : Boolean := True)
     return Pointer_To_uRecordset;

   procedure Cancel
     (This : uConnection_Type);

end ADO.uConnection_Interface;
