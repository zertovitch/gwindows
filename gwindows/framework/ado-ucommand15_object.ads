with GNATCOM.Dispinterface;

package ADO.uCommand15_Object is

   type uCommand15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_ActiveConnection
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure PutRef_ActiveConnection
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_ActiveConnection
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CommandText
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CommandText
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CommandTimeout
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CommandTimeout
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Prepared
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Prepared
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Execute
     (This            : uCommand15_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Parameters      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

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
     return GNATCOM.Types.VARIANT;

   function Get_Parameters
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CommandType
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CommandType
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Name
     (This : uCommand15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Name
     (This : uCommand15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end ADO.uCommand15_Object;
