with GNATCOM.Dispinterface;

package ADO.Field_Interface is

   type Field_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Field_Type);

   function Pointer (This : Field_Type)
     return Pointer_To_Field;

   procedure Attach (This    : in out Field_Type;
                     Pointer : in     Pointer_To_Field);

   function Get_Properties
     (This      : Field_Type)
     return Pointer_To_Properties;

   function Get_ActualSize
     (This : Field_Type)
     return Interfaces.C.long;

   function Get_Attributes
     (This : Field_Type)
     return Interfaces.C.long;

   function Get_DefinedSize
     (This : Field_Type)
     return Interfaces.C.long;

   function Get_Name
     (This  : Field_Type)
     return GNATCOM.Types.BSTR;

   function Get_Type
     (This      : Field_Type)
     return DataTypeEnum;

   function Get_Value
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Field_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Precision
     (This        : Field_Type)
     return Interfaces.C.unsigned_char;

   function Get_NumericScale
     (This           : Field_Type)
     return Interfaces.C.unsigned_char;

   procedure AppendChunk
     (This : Field_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function GetChunk
     (This   : Field_Type;
      Length : Interfaces.C.long)
     return GNATCOM.Types.VARIANT;

   function Get_OriginalValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_UnderlyingValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_DataFormat
     (This  : Field_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure PutRef_DataFormat
     (This  : Field_Type;
      ppiDF : GNATCOM.Types.Pointer_To_IUnknown);

   procedure Put_Precision
     (This        : Field_Type;
      pbPrecision : Interfaces.C.unsigned_char);

   procedure Put_NumericScale
     (This           : Field_Type;
      pbNumericScale : Interfaces.C.unsigned_char);

   procedure Put_Type
     (This      : Field_Type;
      pDataType : DataTypeEnum);

   procedure Put_DefinedSize
     (This : Field_Type;
      pl   : Interfaces.C.long);

   procedure Put_Attributes
     (This : Field_Type;
      pl   : Interfaces.C.long);

end ADO.Field_Interface;
