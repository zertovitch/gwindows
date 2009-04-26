with GNATCOM.Dispinterface;

package ADO.uParameter_Object is

   type uParameter_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Name
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Name
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Value
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Type
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Type
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_Direction
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Direction
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Precision
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Precision
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_NumericScale
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_NumericScale
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Size
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Size
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure AppendChunk
     (This : uParameter_Type;
      Val  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Attributes
     (This : uParameter_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Attributes
     (This : uParameter_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end ADO.uParameter_Object;
