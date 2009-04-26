with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Property_Interface is

   procedure Initialize (This : in out Property_Type) is
   begin
      Set_IID (This, IID_Property);
   end Initialize;

   function Pointer (This : Property_Type)
     return Pointer_To_Property
   is
   begin
      return To_Pointer_To_Property (Address (This));
   end Pointer;

   procedure Attach (This    : in out Property_Type;
                     Pointer : in     Pointer_To_Property)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Value
     (This : Property_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Value
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Value;

   procedure Put_Value
     (This : Property_Type;
      pval : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Value
         (Pointer (This),
          pval));

      if Free then
               GNATCOM.Iinterface.Free (pval);

      end if;

   end Put_Value;

   function Get_Name
     (This  : Property_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Name
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Name;

   function Get_Type
     (This  : Property_Type)
     return DataTypeEnum
   is
       RetVal : aliased DataTypeEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Type
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Type;

   function Get_Attributes
     (This         : Property_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Attributes
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Attributes;

   procedure Put_Attributes
     (This         : Property_Type;
      plAttributes : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Attributes
         (Pointer (This),
          plAttributes));

   end Put_Attributes;

end ADO.Property_Interface;
