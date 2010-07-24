with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Field_Interface is

   procedure Initialize (This : in out Field_Type) is
   begin
      Set_IID (This, IID_Field);
   end Initialize;

   function Pointer (This : Field_Type)
     return Pointer_To_Field
   is
   begin
      return To_Pointer_To_Field (Address (This));
   end Pointer;

   procedure Attach (This    : in out Field_Type;
                     Pointer : in     Pointer_To_Field)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
      (This      : Field_Type)
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
      (This : Field_Type)
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
      (This : Field_Type)
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
      (This : Field_Type)
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
      (This  : Field_Type)
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
      (This      : Field_Type)
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
      (This : Field_Type)
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
     (This : Field_Type;
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
      (This        : Field_Type)
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
      (This           : Field_Type)
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
     (This : Field_Type;
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
      (This   : Field_Type;
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
      (This : Field_Type)
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
      (This : Field_Type)
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

   function Get_DataFormat
      (This  : Field_Type)
      return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DataFormat
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DataFormat;

   procedure PutRef_DataFormat
     (This  : Field_Type;
      ppiDF : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PutRef_DataFormat
         (Pointer (This),
          ppiDF));

   end PutRef_DataFormat;

   procedure Put_Precision
     (This        : Field_Type;
      pbPrecision : Interfaces.C.unsigned_char)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Precision
         (Pointer (This),
          pbPrecision));

   end Put_Precision;

   procedure Put_NumericScale
     (This           : Field_Type;
      pbNumericScale : Interfaces.C.unsigned_char)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_NumericScale
         (Pointer (This),
          pbNumericScale));

   end Put_NumericScale;

   procedure Put_Type
     (This      : Field_Type;
      pDataType : DataTypeEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Type
         (Pointer (This),
          pDataType));

   end Put_Type;

   procedure Put_DefinedSize
     (This : Field_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_DefinedSize
         (Pointer (This),
          pl));

   end Put_DefinedSize;

   procedure Put_Attributes
     (This : Field_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Attributes
         (Pointer (This),
          pl));

   end Put_Attributes;

end ADO.Field_Interface;
