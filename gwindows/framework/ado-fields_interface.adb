with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Fields_Interface is

   procedure Initialize (This : in out Fields_Type) is
   begin
      Set_IID (This, IID_Fields);
   end Initialize;

   function Pointer (This : Fields_Type)
     return Pointer_To_Fields
   is
   begin
      return To_Pointer_To_Fields (Address (This));
   end Pointer;

   procedure Attach (This    : in out Fields_Type;
                     Pointer : in     Pointer_To_Fields)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
      (This : Fields_Type)
      return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Count
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Count;

   function uNewEnum
      (This      : Fields_Type)
      return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNewEnum
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uNewEnum;

   procedure Refresh
     (This : Fields_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   function Get_Item
      (This      : Fields_Type;
       Index     : GNATCOM.Types.VARIANT;
       Free      : Boolean := True)
      return Pointer_To_Field
   is
       RetVal : aliased Pointer_To_Field;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Item
         (Pointer (This),
          Index,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (Index);
      end if;

      return RetVal;
   end Get_Item;

   procedure Append
     (This        : Fields_Type;
      Name        : GNATCOM.Types.BSTR;
      uType       : DataTypeEnum;
      DefinedSize : Interfaces.C.long;
      Attrib      : FieldAttributeEnum;
      Free        : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Append
         (Pointer (This),
          Name,
          uType,
          DefinedSize,
          Attrib));

      if Free then
         GNATCOM.Iinterface.Free (Name);
      end if;

   end Append;

   procedure Delete
     (This  : Fields_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Delete
         (Pointer (This),
          Index));

      if Free then
         GNATCOM.Iinterface.Free (Index);
      end if;

   end Delete;

end ADO.Fields_Interface;
