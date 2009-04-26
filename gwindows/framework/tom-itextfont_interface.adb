with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextFont_Interface is

   procedure Initialize (This : in out ITextFont_Type) is
   begin
      Set_IID (This, IID_ITextFont);
   end Initialize;

   function Pointer (This : ITextFont_Type)
     return Pointer_To_ITextFont
   is
   begin
      return To_Pointer_To_ITextFont (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextFont_Type;
                     Pointer : in     Pointer_To_ITextFont)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Duplicate
     (This   : ITextFont_Type)
     return Pointer_To_ITextFont
   is
       RetVal : aliased Pointer_To_ITextFont;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Duplicate
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Duplicate;

   procedure Put_Duplicate
     (This   : ITextFont_Type;
      ppFont : Pointer_To_ITextFont)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Duplicate
         (Pointer (This),
          ppFont));

   end Put_Duplicate;

   function CanChange
     (This : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CanChange
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end CanChange;

   function IsEqual
     (This  : ITextFont_Type;
      pFont : Pointer_To_ITextFont)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsEqual
         (Pointer (This),
          pFont,
          RetVal'Unchecked_Access));

      return RetVal;
   end IsEqual;

   procedure Reset
     (This  : ITextFont_Type;
      Value : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This),
          Value));

   end Reset;

   function Get_Style
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Style
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Style;

   procedure Put_Style
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Style
         (Pointer (This),
          pValue));

   end Put_Style;

   function Get_AllCaps
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_AllCaps
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_AllCaps;

   procedure Put_AllCaps
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_AllCaps
         (Pointer (This),
          pValue));

   end Put_AllCaps;

   function Get_Animation
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Animation
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Animation;

   procedure Put_Animation
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Animation
         (Pointer (This),
          pValue));

   end Put_Animation;

   function Get_BackColor
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_BackColor
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_BackColor;

   procedure Put_BackColor
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_BackColor
         (Pointer (This),
          pValue));

   end Put_BackColor;

   function Get_Bold
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Bold
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Bold;

   procedure Put_Bold
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Bold
         (Pointer (This),
          pValue));

   end Put_Bold;

   function Get_Emboss
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Emboss
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Emboss;

   procedure Put_Emboss
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Emboss
         (Pointer (This),
          pValue));

   end Put_Emboss;

   function Get_ForeColor
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ForeColor
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ForeColor;

   procedure Put_ForeColor
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ForeColor
         (Pointer (This),
          pValue));

   end Put_ForeColor;

   function Get_Hidden
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Hidden
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Hidden;

   procedure Put_Hidden
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Hidden
         (Pointer (This),
          pValue));

   end Put_Hidden;

   function Get_Engrave
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Engrave
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Engrave;

   procedure Put_Engrave
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Engrave
         (Pointer (This),
          pValue));

   end Put_Engrave;

   function Get_Italic
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Italic
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Italic;

   procedure Put_Italic
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Italic
         (Pointer (This),
          pValue));

   end Put_Italic;

   function Get_Kerning
     (This   : ITextFont_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Kerning
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Kerning;

   procedure Put_Kerning
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Kerning
         (Pointer (This),
          pValue));

   end Put_Kerning;

   function Get_LanguageID
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LanguageID
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LanguageID;

   procedure Put_LanguageID
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_LanguageID
         (Pointer (This),
          pValue));

   end Put_LanguageID;

   function Get_Name
     (This  : ITextFont_Type)
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
     (This  : ITextFont_Type;
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

   function Get_Outline
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Outline
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Outline;

   procedure Put_Outline
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Outline
         (Pointer (This),
          pValue));

   end Put_Outline;

   function Get_Position
     (This   : ITextFont_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Position
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Position;

   procedure Put_Position
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Position
         (Pointer (This),
          pValue));

   end Put_Position;

   function Get_Protected
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Protected
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Protected;

   procedure Put_Protected
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Protected
         (Pointer (This),
          pValue));

   end Put_Protected;

   function Get_Shadow
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Shadow
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Shadow;

   procedure Put_Shadow
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Shadow
         (Pointer (This),
          pValue));

   end Put_Shadow;

   function Get_Size
     (This   : ITextFont_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Size
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Size;

   procedure Put_Size
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Size
         (Pointer (This),
          pValue));

   end Put_Size;

   function Get_SmallCaps
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SmallCaps
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SmallCaps;

   procedure Put_SmallCaps
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_SmallCaps
         (Pointer (This),
          pValue));

   end Put_SmallCaps;

   function Get_Spacing
     (This   : ITextFont_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Spacing
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Spacing;

   procedure Put_Spacing
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Spacing
         (Pointer (This),
          pValue));

   end Put_Spacing;

   function Get_StrikeThrough
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StrikeThrough
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StrikeThrough;

   procedure Put_StrikeThrough
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_StrikeThrough
         (Pointer (This),
          pValue));

   end Put_StrikeThrough;

   function Get_Subscript
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Subscript
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Subscript;

   procedure Put_Subscript
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Subscript
         (Pointer (This),
          pValue));

   end Put_Subscript;

   function Get_Superscript
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Superscript
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Superscript;

   procedure Put_Superscript
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Superscript
         (Pointer (This),
          pValue));

   end Put_Superscript;

   function Get_Underline
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Underline
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Underline;

   procedure Put_Underline
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Underline
         (Pointer (This),
          pValue));

   end Put_Underline;

   function Get_Weight
     (This   : ITextFont_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Weight
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Weight;

   procedure Put_Weight
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Weight
         (Pointer (This),
          pValue));

   end Put_Weight;

end TOM.ITextFont_Interface;
