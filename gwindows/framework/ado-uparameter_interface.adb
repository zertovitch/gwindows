with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uParameter_Interface is

   procedure Initialize (This : in out uParameter_Type) is
   begin
      Set_IID (This, IID_uParameter);
   end Initialize;

   function Pointer (This : uParameter_Type)
     return Pointer_To_uParameter
   is
   begin
      return To_Pointer_To_uParameter (Address (This));
   end Pointer;

   procedure Attach (This    : in out uParameter_Type;
                     Pointer : in     Pointer_To_uParameter)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
     (This      : uParameter_Type)
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

   function Get_Name
     (This  : uParameter_Type)
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

   procedure Put_Name
     (This  : uParameter_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Name
         (Pointer (This),
          pbstr));

      if Free then
               GNATCOM.Iinterface.Free (pbstr);

      end if;

   end Put_Name;

   function Get_Value
     (This : uParameter_Type)
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
     (This : uParameter_Type;
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

   function Get_Type
     (This       : uParameter_Type)
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

   procedure Put_Type
     (This       : uParameter_Type;
      psDataType : DataTypeEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Type
         (Pointer (This),
          psDataType));

   end Put_Type;

   procedure Put_Direction
     (This            : uParameter_Type;
      plParmDirection : ParameterDirectionEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Direction
         (Pointer (This),
          plParmDirection));

   end Put_Direction;

   function Get_Direction
     (This            : uParameter_Type)
     return ParameterDirectionEnum
   is
       RetVal : aliased ParameterDirectionEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Direction
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Direction;

   procedure Put_Precision
     (This        : uParameter_Type;
      pbPrecision : Interfaces.C.unsigned_char)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Precision
         (Pointer (This),
          pbPrecision));

   end Put_Precision;

   function Get_Precision
     (This        : uParameter_Type)
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

   procedure Put_NumericScale
     (This    : uParameter_Type;
      pbScale : Interfaces.C.unsigned_char)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_NumericScale
         (Pointer (This),
          pbScale));

   end Put_NumericScale;

   function Get_NumericScale
     (This    : uParameter_Type)
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

   procedure Put_Size
     (This : uParameter_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Size
         (Pointer (This),
          pl));

   end Put_Size;

   function Get_Size
     (This : uParameter_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Size
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Size;

   procedure AppendChunk
     (This : uParameter_Type;
      Val  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AppendChunk
         (Pointer (This),
          Val));

      if Free then
               GNATCOM.Iinterface.Free (Val);

      end if;

   end AppendChunk;

   function Get_Attributes
     (This          : uParameter_Type)
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
     (This          : uParameter_Type;
      plParmAttribs : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Attributes
         (Pointer (This),
          plParmAttribs));

   end Put_Attributes;

end ADO.uParameter_Interface;
