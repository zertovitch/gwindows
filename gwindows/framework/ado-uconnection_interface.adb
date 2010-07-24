with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uConnection_Interface is

   procedure Initialize (This : in out uConnection_Type) is
   begin
      Set_IID (This, IID_uConnection);
   end Initialize;

   function Pointer (This : uConnection_Type)
     return Pointer_To_uConnection
   is
   begin
      return To_Pointer_To_uConnection (Address (This));
   end Pointer;

   procedure Attach (This    : in out uConnection_Type;
                     Pointer : in     Pointer_To_uConnection)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
      (This      : uConnection_Type)
      return Pointer_To_Properties
   is
       RetVal : aliased Pointer_To_Properties;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Properties
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Properties;

   function Get_ConnectionString
      (This  : uConnection_Type)
      return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ConnectionString
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ConnectionString;

   procedure Put_ConnectionString
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ConnectionString
         (Pointer (This),
          pbstr));

      if Free then
         GNATCOM.Iinterface.Free (pbstr);
      end if;

   end Put_ConnectionString;

   function Get_CommandTimeout
      (This      : uConnection_Type)
      return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CommandTimeout
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CommandTimeout;

   procedure Put_CommandTimeout
     (This      : uConnection_Type;
      plTimeout : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_CommandTimeout
         (Pointer (This),
          plTimeout));

   end Put_CommandTimeout;

   function Get_ConnectionTimeout
      (This      : uConnection_Type)
      return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ConnectionTimeout
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ConnectionTimeout;

   procedure Put_ConnectionTimeout
     (This      : uConnection_Type;
      plTimeout : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ConnectionTimeout
         (Pointer (This),
          plTimeout));

   end Put_ConnectionTimeout;

   function Get_Version
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Version
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Version;

   procedure Close
     (This : uConnection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Close
         (Pointer (This)));

   end Close;

   function Execute
     (This            : uConnection_Type;
      CommandText     : GNATCOM.Types.BSTR;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Options         : Interfaces.C.long;
      Free            : Boolean := True)
     return Pointer_To_uRecordset
   is
       RetVal : aliased Pointer_To_uRecordset;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Execute
         (Pointer (This),
          CommandText,
          RecordsAffected,
          Options,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (CommandText);
      end if;

      return RetVal;
   end Execute;

   function BeginTrans
     (This             : uConnection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.BeginTrans
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end BeginTrans;

   procedure CommitTrans
     (This : uConnection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CommitTrans
         (Pointer (This)));

   end CommitTrans;

   procedure RollbackTrans
     (This : uConnection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RollbackTrans
         (Pointer (This)));

   end RollbackTrans;

   procedure Open
     (This             : uConnection_Type;
      ConnectionString : GNATCOM.Types.BSTR;
      UserID           : GNATCOM.Types.BSTR;
      Password         : GNATCOM.Types.BSTR;
      Options          : Interfaces.C.long;
      Free             : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Open
         (Pointer (This),
          ConnectionString,
          UserID,
          Password,
          Options));

      if Free then
         GNATCOM.Iinterface.Free (ConnectionString);
         GNATCOM.Iinterface.Free (UserID);
         GNATCOM.Iinterface.Free (Password);
      end if;

   end Open;

   function Get_Errors
     (This      : uConnection_Type)
     return Pointer_To_Errors
   is
       RetVal : aliased Pointer_To_Errors;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Errors
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Errors;

   function Get_DefaultDatabase
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DefaultDatabase
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DefaultDatabase;

   procedure Put_DefaultDatabase
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_DefaultDatabase
         (Pointer (This),
          pbstr));

      if Free then
         GNATCOM.Iinterface.Free (pbstr);
      end if;

   end Put_DefaultDatabase;

   function Get_IsolationLevel
     (This  : uConnection_Type)
     return IsolationLevelEnum
   is
       RetVal : aliased IsolationLevelEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_IsolationLevel
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_IsolationLevel;

   procedure Put_IsolationLevel
     (This  : uConnection_Type;
      Level : IsolationLevelEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_IsolationLevel
         (Pointer (This),
          Level));

   end Put_IsolationLevel;

   function Get_Attributes
     (This   : uConnection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Attributes
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Attributes;

   procedure Put_Attributes
     (This   : uConnection_Type;
      plAttr : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Attributes
         (Pointer (This),
          plAttr));

   end Put_Attributes;

   function Get_CursorLocation
     (This        : uConnection_Type)
     return CursorLocationEnum
   is
       RetVal : aliased CursorLocationEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CursorLocation
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CursorLocation;

   procedure Put_CursorLocation
     (This        : uConnection_Type;
      plCursorLoc : CursorLocationEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_CursorLocation
         (Pointer (This),
          plCursorLoc));

   end Put_CursorLocation;

   function Get_Mode
     (This   : uConnection_Type)
     return ConnectModeEnum
   is
       RetVal : aliased ConnectModeEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Mode
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Mode;

   procedure Put_Mode
     (This   : uConnection_Type;
      plMode : ConnectModeEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Mode
         (Pointer (This),
          plMode));

   end Put_Mode;

   function Get_Provider
     (This  : uConnection_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Provider
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Provider;

   procedure Put_Provider
     (This  : uConnection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Provider
         (Pointer (This),
          pbstr));

      if Free then
         GNATCOM.Iinterface.Free (pbstr);
      end if;

   end Put_Provider;

   function Get_State
     (This       : uConnection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_State
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_State;

   function OpenSchema
     (This         : uConnection_Type;
      Schema       : SchemaEnum;
      Restrictions : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      SchemaID     : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free         : Boolean := True)
     return Pointer_To_uRecordset
   is
       RetVal : aliased Pointer_To_uRecordset;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.OpenSchema
         (Pointer (This),
          Schema,
          Restrictions,
          SchemaID,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (Restrictions);
         GNATCOM.Iinterface.Free (SchemaID);
      end if;

      return RetVal;
   end OpenSchema;

   procedure Cancel
     (This : uConnection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Cancel
         (Pointer (This)));

   end Cancel;

end ADO.uConnection_Interface;
