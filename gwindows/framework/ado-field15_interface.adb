with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Field15_Interface is

   procedure Initialize (This : in out Field15_Type) is
   begin
      Set_IID (This, IID_Field15);
   end Initialize;

   function Pointer (This : Field15_Type)
     return Pointer_To_Field15
   is
   begin
      return To_Pointer_To_Field15 (Address (This));
   end Pointer;

   procedure Attach (This    : in out Field15_Type;
                     Pointer : in     Pointer_To_Field15)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
     (This      : Field15_Type)
     return Pointer_To_Properties
   is
       RetVal : aliased Pointer_To_Properties;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Properties
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Properties;

   function Get_ActualSize
     (This : Field15_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ActualSize
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ActualSize;

   function Get_Attributes
     (This : Field15_Type)
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

   function Get_DefinedSize
     (This : Field15_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DefinedSize
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DefinedSize;

   function Get_Name
     (This  : Field15_Type)
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
     (This      : Field15_Type)
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

   function Get_Value
     (This : Field15_Type)
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
     (This : Field15_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Value
         (Pointer (This),
          pvar));

      if Free then
               GNATCOM.Iinterface.Free (pvar);

      end if;

   end Put_Value;

   function Get_Precision
     (This        : Field15_Type)
     return Interfaces.C.unsigned_char
   is
       RetVal : aliased Interfaces.C.unsigned_char;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Precision
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Precision;

   function Get_NumericScale
     (This           : Field15_Type)
     return Interfaces.C.unsigned_char
   is
       RetVal : aliased Interfaces.C.unsigned_char;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_NumericScale
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_NumericScale;

   procedure AppendChunk
     (This : Field15_Type;
      Data : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AppendChunk
         (Pointer (This),
          Data));

      if Free then
               GNATCOM.Iinterface.Free (Data);

      end if;

   end AppendChunk;

   function GetChunk
     (This   : Field15_Type;
      Length : Interfaces.C.long)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetChunk
         (Pointer (This),
          Length,
          RetVal'Unchecked_Access));

      return RetVal;
   end GetChunk;

   function Get_OriginalValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_OriginalValue
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_OriginalValue;

   function Get_UnderlyingValue
     (This : Field15_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_UnderlyingValue
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_UnderlyingValue;

end ADO.Field15_Interface;
