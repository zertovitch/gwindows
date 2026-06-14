with GNATCOM.Dispinterface;
with Win32_Types;

package ADO.uParameter_Interface is

   type uParameter_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uParameter_Type);

   function Pointer (This : uParameter_Type)
     return Pointer_To_uParameter;

   procedure Attach (This    : in out uParameter_Type;
                     Pointer : in     Pointer_To_uParameter);

   function Get_Properties
     (This      : uParameter_Type)
     return Pointer_To_Properties;

   function Get_Name
     (This  : uParameter_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Name
     (This  : uParameter_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_Value
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : uParameter_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Type
     (This       : uParameter_Type)
     return DataTypeEnum;

   procedure Put_Type
     (This       : uParameter_Type;
      psDataType : DataTypeEnum);

   procedure Put_Direction
     (This            : uParameter_Type;
      plParmDirection : ParameterDirectionEnum);

   function Get_Direction
     (This            : uParameter_Type)
     return ParameterDirectionEnum;

   procedure Put_Precision
     (This        : uParameter_Type;
      pbPrecision : Interfaces.C.unsigned_char);

   function Get_Precision
     (This        : uParameter_Type)
     return Interfaces.C.unsigned_char;

   procedure Put_NumericScale
     (This    : uParameter_Type;
      pbScale : Interfaces.C.unsigned_char);

   function Get_NumericScale
     (This    : uParameter_Type)
     return Interfaces.C.unsigned_char;

   procedure Put_Size
     (This : uParameter_Type;
      pl   : Win32_Types.Long);

   function Get_Size
     (This : uParameter_Type)
     return Win32_Types.Long;

   procedure AppendChunk
     (This : uParameter_Type;
      Val  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Attributes
     (This          : uParameter_Type)
     return Win32_Types.Long;

   procedure Put_Attributes
     (This          : uParameter_Type;
      plParmAttribs : Win32_Types.Long);

end ADO.uParameter_Interface;
