with GNATCOM.Dispinterface;

package ADO.Field15_Object is

   type Field15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_ActualSize
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Attributes
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_DefinedSize
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Name
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Type
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Value
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Field15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Precision
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_NumericScale
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   procedure AppendChunk
     (This : Field15_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function GetChunk
     (This   : Field15_Type;
      Length : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_OriginalValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_UnderlyingValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT;

end ADO.Field15_Object;
