with GNATCOM.Dispinterface;

package ADO.Property_Interface is

   type Property_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Property_Type);

   function Pointer (This : Property_Type)
     return Pointer_To_Property;

   procedure Attach (This    : in out Property_Type;
                     Pointer : in     Pointer_To_Property);

   function Get_Value
     (This : Property_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Value
     (This : Property_Type;
      pval : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Name
     (This  : Property_Type)
     return GNATCOM.Types.BSTR;

   function Get_Type
     (This  : Property_Type)
     return DataTypeEnum;

   function Get_Attributes
     (This         : Property_Type)
     return Interfaces.C.long;

   procedure Put_Attributes
     (This         : Property_Type;
      plAttributes : Interfaces.C.long);

end ADO.Property_Interface;
