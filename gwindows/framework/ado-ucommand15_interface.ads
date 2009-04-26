with GNATCOM.Dispinterface;

package ADO.uCommand15_Interface is

   type uCommand15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uCommand15_Type);

   function Pointer (This : uCommand15_Type)
     return Pointer_To_uCommand15;

   procedure Attach (This    : in out uCommand15_Type;
                     Pointer : in     Pointer_To_uCommand15);

   function Get_Properties
     (This      : uCommand15_Type)
     return Pointer_To_Properties;

   function Get_ActiveConnection
     (This      : uCommand15_Type)
     return Pointer_To_uConnection;

   procedure PutRef_ActiveConnection
     (This      : uCommand15_Type;
      ppvObject : Pointer_To_uConnection);

   procedure Put_ActiveConnection
     (This      : uCommand15_Type;
      ppvObject : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);

   function Get_CommandText
     (This  : uCommand15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_CommandText
     (This  : uCommand15_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_CommandTimeout
     (This : uCommand15_Type)
     return Interfaces.C.long;

   procedure Put_CommandTimeout
     (This : uCommand15_Type;
      pl   : Interfaces.C.long);

   function Get_Prepared
     (This       : uCommand15_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure Put_Prepared
     (This       : uCommand15_Type;
      pfPrepared : GNATCOM.Types.VARIANT_BOOL);

   function Execute
     (This            : uCommand15_Type;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Parameters      : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Options         : Interfaces.C.long)
     return Pointer_To_uRecordset;

   function CreateParameter
     (This      : uCommand15_Type;
      Name      : GNATCOM.Types.BSTR;
      uType     : DataTypeEnum;
      Direction : ParameterDirectionEnum;
      Size      : Interfaces.C.long;
      Value     : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
     return Pointer_To_uParameter;

   function Get_Parameters
     (This      : uCommand15_Type)
     return Pointer_To_Parameters;

   procedure Put_CommandType
     (This      : uCommand15_Type;
      plCmdType : CommandTypeEnum);

   function Get_CommandType
     (This      : uCommand15_Type)
     return CommandTypeEnum;

   function Get_Name
     (This      : uCommand15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Name
     (This      : uCommand15_Type;
      pbstrName : GNATCOM.Types.BSTR;
      Free      : Boolean := True);

end ADO.uCommand15_Interface;
