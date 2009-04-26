with GNATCOM.Dispinterface;

package ADO.uCommand_Interface is

   type uCommand_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uCommand_Type);

   function Pointer (This : uCommand_Type)
     return Pointer_To_uCommand;

   procedure Attach (This    : in out uCommand_Type;
                     Pointer : in     Pointer_To_uCommand);

   function Get_Properties
     (This      : uCommand_Type)
     return Pointer_To_Properties;

   function Get_ActiveConnection
     (This      : uCommand_Type)
     return Pointer_To_uConnection;

   procedure PutRef_ActiveConnection
     (This      : uCommand_Type;
      ppvObject : Pointer_To_uConnection);

   procedure Put_ActiveConnection
     (This      : uCommand_Type;
      ppvObject : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);

   function Get_CommandText
     (This  : uCommand_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_CommandText
     (This  : uCommand_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_CommandTimeout
     (This : uCommand_Type)
     return Interfaces.C.long;

   procedure Put_CommandTimeout
     (This : uCommand_Type;
      pl   : Interfaces.C.long);

   function Get_Prepared
     (This       : uCommand_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure Put_Prepared
     (This       : uCommand_Type;
      pfPrepared : GNATCOM.Types.VARIANT_BOOL);

   function Execute
     (This            : uCommand_Type;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Parameters      : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Options         : Interfaces.C.long)
     return Pointer_To_uRecordset;

   function CreateParameter
     (This      : uCommand_Type;
      Name      : GNATCOM.Types.BSTR;
      uType     : DataTypeEnum;
      Direction : ParameterDirectionEnum;
      Size      : Interfaces.C.long;
      Value     : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
     return Pointer_To_uParameter;

   function Get_Parameters
     (This      : uCommand_Type)
     return Pointer_To_Parameters;

   procedure Put_CommandType
     (This      : uCommand_Type;
      plCmdType : CommandTypeEnum);

   function Get_CommandType
     (This      : uCommand_Type)
     return CommandTypeEnum;

   function Get_Name
     (This      : uCommand_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Name
     (This      : uCommand_Type;
      pbstrName : GNATCOM.Types.BSTR;
      Free      : Boolean := True);

   function Get_State
     (This       : uCommand_Type)
     return Interfaces.C.long;

   procedure Cancel
     (This : uCommand_Type);

end ADO.uCommand_Interface;
