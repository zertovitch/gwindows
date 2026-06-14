with GNATCOM.Dispinterface;
with Win32_Types;

package ADO.uConnection15_Interface is

   type uConnection15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uConnection15_Type);

   function Pointer (This : uConnection15_Type)
     return Pointer_To_uConnection15;

   procedure Attach (This    : in out uConnection15_Type;
                     Pointer : in     Pointer_To_uConnection15);

   function Get_Properties
     (This      : uConnection15_Type)
     return Pointer_To_Properties;

   function Get_ConnectionString
     (This  : uConnection15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_ConnectionString
     (This  : uConnection15_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_CommandTimeout
     (This      : uConnection15_Type)
     return Win32_Types.Long;

   procedure Put_CommandTimeout
     (This      : uConnection15_Type;
      plTimeout : Win32_Types.Long);

   function Get_ConnectionTimeout
     (This      : uConnection15_Type)
     return Win32_Types.Long;

   procedure Put_ConnectionTimeout
     (This      : uConnection15_Type;
      plTimeout : Win32_Types.Long);

   function Get_Version
     (This  : uConnection15_Type)
     return GNATCOM.Types.BSTR;

   procedure Close
     (This : uConnection15_Type);

   function Execute
     (This            : uConnection15_Type;
      CommandText     : GNATCOM.Types.BSTR;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Options         : Win32_Types.Long;
      Free            : Boolean := True)
     return Pointer_To_uRecordset;

   function BeginTrans
     (This             : uConnection15_Type)
     return Win32_Types.Long;

   procedure CommitTrans
     (This : uConnection15_Type);

   procedure RollbackTrans
     (This : uConnection15_Type);

   procedure Open
     (This             : uConnection15_Type;
      ConnectionString : GNATCOM.Types.BSTR;
      UserID           : GNATCOM.Types.BSTR;
      Password         : GNATCOM.Types.BSTR;
      Options          : Win32_Types.Long;
      Free             : Boolean := True);

   function Get_Errors
     (This      : uConnection15_Type)
     return Pointer_To_Errors;

   function Get_DefaultDatabase
     (This  : uConnection15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_DefaultDatabase
     (This  : uConnection15_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_IsolationLevel
     (This  : uConnection15_Type)
     return IsolationLevelEnum;

   procedure Put_IsolationLevel
     (This  : uConnection15_Type;
      Level : IsolationLevelEnum);

   function Get_Attributes
     (This   : uConnection15_Type)
     return Win32_Types.Long;

   procedure Put_Attributes
     (This   : uConnection15_Type;
      plAttr : Win32_Types.Long);

   function Get_CursorLocation
     (This        : uConnection15_Type)
     return CursorLocationEnum;

   procedure Put_CursorLocation
     (This        : uConnection15_Type;
      plCursorLoc : CursorLocationEnum);

   function Get_Mode
     (This   : uConnection15_Type)
     return ConnectModeEnum;

   procedure Put_Mode
     (This   : uConnection15_Type;
      plMode : ConnectModeEnum);

   function Get_Provider
     (This  : uConnection15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Provider
     (This  : uConnection15_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_State
     (This       : uConnection15_Type)
     return Win32_Types.Long;

   function OpenSchema
     (This         : uConnection15_Type;
      Schema       : SchemaEnum;
      Restrictions : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      SchemaID     : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free         : Boolean := True)
     return Pointer_To_uRecordset;

end ADO.uConnection15_Interface;
