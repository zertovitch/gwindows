with GNATCOM.Dispinterface;

package TOM.ITextFont_Object is

   type ITextFont_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Duplicate
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Duplicate
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function CanChange
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   function IsEqual
     (This  : ITextFont_Type;
      pFont : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Reset
     (This  : ITextFont_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_Style
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Style
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_AllCaps
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_AllCaps
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Animation
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Animation
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_BackColor
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_BackColor
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Bold
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Bold
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Emboss
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Emboss
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ForeColor
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ForeColor
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Hidden
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Hidden
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Engrave
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Engrave
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Italic
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Italic
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Kerning
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Kerning
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_LanguageID
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_LanguageID
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Name
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Name
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Outline
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Outline
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Position
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Position
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Protected
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Protected
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Shadow
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Shadow
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Size
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Size
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_SmallCaps
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_SmallCaps
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Spacing
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Spacing
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_StrikeThrough
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_StrikeThrough
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Subscript
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Subscript
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Superscript
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Superscript
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Underline
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Underline
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Weight
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Weight
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end TOM.ITextFont_Object;
