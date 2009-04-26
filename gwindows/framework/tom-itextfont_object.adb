package body TOM.ITextFont_Object is

   function Get_Duplicate
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Duplicate);
   end Get_Duplicate;

   procedure Put_Duplicate
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Duplicate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Duplicate;

   function CanChange
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextFont_CanChange);
   end CanChange;

   function IsEqual
     (This  : ITextFont_Type;
      pFont : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextFont_IsEqual,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pFont),
         Free);
   end IsEqual;

   procedure Reset
     (This  : ITextFont_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextFont_Reset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value),
         Free);
   end Reset;

   function Get_Style
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Style);
   end Get_Style;

   procedure Put_Style
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Style,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Style;

   function Get_AllCaps
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_AllCaps);
   end Get_AllCaps;

   procedure Put_AllCaps
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_AllCaps,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AllCaps;

   function Get_Animation
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Animation);
   end Get_Animation;

   procedure Put_Animation
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Animation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Animation;

   function Get_BackColor
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_BackColor);
   end Get_BackColor;

   procedure Put_BackColor
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_BackColor,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_BackColor;

   function Get_Bold
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Bold);
   end Get_Bold;

   procedure Put_Bold
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Bold,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Bold;

   function Get_Emboss
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Emboss);
   end Get_Emboss;

   procedure Put_Emboss
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Emboss,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Emboss;

   function Get_ForeColor
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_ForeColor);
   end Get_ForeColor;

   procedure Put_ForeColor
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_ForeColor,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ForeColor;

   function Get_Hidden
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Hidden);
   end Get_Hidden;

   procedure Put_Hidden
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Hidden,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Hidden;

   function Get_Engrave
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Engrave);
   end Get_Engrave;

   procedure Put_Engrave
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Engrave,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Engrave;

   function Get_Italic
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Italic);
   end Get_Italic;

   procedure Put_Italic
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Italic,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Italic;

   function Get_Kerning
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Kerning);
   end Get_Kerning;

   procedure Put_Kerning
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Kerning,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Kerning;

   function Get_LanguageID
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_LanguageID);
   end Get_LanguageID;

   procedure Put_LanguageID
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_LanguageID,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_LanguageID;

   function Get_Name
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Name);
   end Get_Name;

   procedure Put_Name
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Name,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Name;

   function Get_Outline
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Outline);
   end Get_Outline;

   procedure Put_Outline
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Outline,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Outline;

   function Get_Position
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Position);
   end Get_Position;

   procedure Put_Position
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Position,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Position;

   function Get_Protected
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Protected);
   end Get_Protected;

   procedure Put_Protected
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Protected,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Protected;

   function Get_Shadow
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Shadow);
   end Get_Shadow;

   procedure Put_Shadow
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Shadow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Shadow;

   function Get_Size
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Size);
   end Get_Size;

   procedure Put_Size
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Size,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Size;

   function Get_SmallCaps
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_SmallCaps);
   end Get_SmallCaps;

   procedure Put_SmallCaps
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_SmallCaps,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_SmallCaps;

   function Get_Spacing
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Spacing);
   end Get_Spacing;

   procedure Put_Spacing
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Spacing,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Spacing;

   function Get_StrikeThrough
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_StrikeThrough);
   end Get_StrikeThrough;

   procedure Put_StrikeThrough
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_StrikeThrough,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StrikeThrough;

   function Get_Subscript
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Subscript);
   end Get_Subscript;

   procedure Put_Subscript
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Subscript,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Subscript;

   function Get_Superscript
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Superscript);
   end Get_Superscript;

   procedure Put_Superscript
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Superscript,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Superscript;

   function Get_Underline
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Underline);
   end Get_Underline;

   procedure Put_Underline
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Underline,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Underline;

   function Get_Weight
     (This : ITextFont_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextFont_Get_Weight);
   end Get_Weight;

   procedure Put_Weight
     (This : ITextFont_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextFont_Put_Weight,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Weight;

end TOM.ITextFont_Object;
