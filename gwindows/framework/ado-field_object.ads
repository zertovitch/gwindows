with GNATCOM.Dispinterface;

package ADO.Field_Object is

   type Field_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_ActualSize
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Attributes
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_DefinedSize
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Name
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Type
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Value
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Precision
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_NumericScale
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   procedure AppendChunk
     (This : Field_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function GetChunk
     (This   : Field_Type;
      Length : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_OriginalValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_UnderlyingValue
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   function Get_DataFormat
     (This : Field_Type)
     return GNATCOM.Types.VARIANT;

   procedure PutRef_DataFormat
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_Precision
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_NumericScale
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_Type
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_DefinedSize
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_Attributes
     (This : Field_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end ADO.Field_Object;
