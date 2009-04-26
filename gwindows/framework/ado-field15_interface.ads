with GNATCOM.Dispinterface;

package ADO.Field15_Interface is

   type Field15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Field15_Type);

   function Pointer (This : Field15_Type)
     return Pointer_To_Field15;

   procedure Attach (This    : in out Field15_Type;
                     Pointer : in     Pointer_To_Field15);

   function Get_Properties
     (This      : Field15_Type)
     return Pointer_To_Properties;

   function Get_ActualSize
     (This : Field15_Type)
     return Interfaces.C.long;

   function Get_Attributes
     (This : Field15_Type)
     return Interfaces.C.long;

   function Get_DefinedSize
     (This : Field15_Type)
     return Interfaces.C.long;

   function Get_Name
     (This  : Field15_Type)
     return GNATCOM.Types.BSTR;

   function Get_Type
     (This      : Field15_Type)
     return DataTypeEnum;

   function Get_Value
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Field15_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Precision
     (This        : Field15_Type)
     return Interfaces.C.unsigned_char;

   function Get_NumericScale
     (This           : Field15_Type)
     return Interfaces.C.unsigned_char;

   procedure AppendChunk
     (This : Field15_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function GetChunk
     (This   : Field15_Type;
      Length : Interfaces.C.long)
     return GNATCOM.Types.VARIANT;

   function Get_OriginalValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_UnderlyingValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

end ADO.Field15_Interface;
