with GNATCOM.Dispinterface;

package TOM.ITextFont_Interface is

   type ITextFont_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextFont_Type);

   function Pointer (This : ITextFont_Type)
     return Pointer_To_ITextFont;

   procedure Attach (This    : in out ITextFont_Type;
                     Pointer : in     Pointer_To_ITextFont);

   function Get_Duplicate
     (This   : ITextFont_Type)
     return Pointer_To_ITextFont;

   procedure Put_Duplicate
     (This   : ITextFont_Type;
      ppFont : Pointer_To_ITextFont);

   function CanChange
     (This : ITextFont_Type)
     return Interfaces.C.long;

   function IsEqual
     (This  : ITextFont_Type;
      pFont : Pointer_To_ITextFont)
     return Interfaces.C.long;

   procedure Reset
     (This  : ITextFont_Type;
      Value : Interfaces.C.long);

   function Get_Style
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Style
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_AllCaps
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_AllCaps
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Animation
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Animation
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_BackColor
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_BackColor
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Bold
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Bold
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Emboss
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Emboss
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_ForeColor
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_ForeColor
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Hidden
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Hidden
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Engrave
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Engrave
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Italic
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Italic
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Kerning
     (This   : ITextFont_Type)
     return Interfaces.C.C_float;

   procedure Put_Kerning
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float);

   function Get_LanguageID
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_LanguageID
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Name
     (This  : ITextFont_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Name
     (This  : ITextFont_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_Outline
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Outline
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Position
     (This   : ITextFont_Type)
     return Interfaces.C.C_float;

   procedure Put_Position
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float);

   function Get_Protected
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Protected
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Shadow
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Shadow
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Size
     (This   : ITextFont_Type)
     return Interfaces.C.C_float;

   procedure Put_Size
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float);

   function Get_SmallCaps
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_SmallCaps
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Spacing
     (This   : ITextFont_Type)
     return Interfaces.C.C_float;

   procedure Put_Spacing
     (This   : ITextFont_Type;
      pValue : Interfaces.C.C_float);

   function Get_StrikeThrough
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_StrikeThrough
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Subscript
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Subscript
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Superscript
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Superscript
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Underline
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Underline
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

   function Get_Weight
     (This   : ITextFont_Type)
     return Interfaces.C.long;

   procedure Put_Weight
     (This   : ITextFont_Type;
      pValue : Interfaces.C.long);

end TOM.ITextFont_Interface;
