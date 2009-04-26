with GNATCOM.Dispinterface;

package ADO.Fields_Interface is

   type Fields_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Fields_Type);

   function Pointer (This : Fields_Type)
     return Pointer_To_Fields;

   procedure Attach (This    : in out Fields_Type;
                     Pointer : in     Pointer_To_Fields);

   function Get_Count
     (This : Fields_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : Fields_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : Fields_Type);

   function Get_Item
     (This      : Fields_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Field;

   procedure Append
     (This        : Fields_Type;
      Name        : GNATCOM.Types.BSTR;
      uType       : DataTypeEnum;
      DefinedSize : Interfaces.C.long;
      Attrib      : FieldAttributeEnum;
      Free        : Boolean := True);

   procedure Delete
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

end ADO.Fields_Interface;
