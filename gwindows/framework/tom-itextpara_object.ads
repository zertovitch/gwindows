with GNATCOM.Dispinterface;

package TOM.ITextPara_Object is

   type ITextPara_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Duplicate
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Duplicate
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function CanChange
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   function IsEqual
     (This  : ITextPara_Type;
      pPara : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Reset
     (This  : ITextPara_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_Style
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Style
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Alignment
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Alignment
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Hyphenation
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Hyphenation
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_FirstLineIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   function Get_KeepTogether
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_KeepTogether
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_KeepWithNext
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_KeepWithNext
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_LeftIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   function Get_LineSpacing
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   function Get_LineSpacingRule
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   function Get_ListAlignment
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ListAlignment
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ListLevelIndex
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ListLevelIndex
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ListStart
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ListStart
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ListTab
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ListTab
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ListType
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_ListType
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_NoLineNumber
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_NoLineNumber
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_PageBreakBefore
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_PageBreakBefore
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_RightIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_RightIndent
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure SetIndents
     (This        : ITextPara_Type;
      StartIndent : GNATCOM.Types.VARIANT;
      LeftIndent  : GNATCOM.Types.VARIANT;
      RightIndent : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);

   procedure SetLineSpacing
     (This            : ITextPara_Type;
      LineSpacingRule : GNATCOM.Types.VARIANT;
      LineSpacing     : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);

   function Get_SpaceAfter
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_SpaceAfter
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_SpaceBefore
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_SpaceBefore
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_WidowControl
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_WidowControl
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_TabCount
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddTab
     (This     : ITextPara_Type;
      tbPos    : GNATCOM.Types.VARIANT;
      tbAlign  : GNATCOM.Types.VARIANT;
      tbLeader : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   procedure ClearAllTabs
     (This : ITextPara_Type);

   procedure DeleteTab
     (This  : ITextPara_Type;
      tbPos : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   procedure GetTab
     (This      : ITextPara_Type;
      iTab      : GNATCOM.Types.VARIANT;
      ptbPos    : GNATCOM.Types.VARIANT;
      ptbAlign  : GNATCOM.Types.VARIANT;
      ptbLeader : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);

end TOM.ITextPara_Object;
