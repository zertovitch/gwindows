package body TOM.ITextPara_Object is

   function Get_Duplicate
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_Duplicate);
   end Get_Duplicate;

   procedure Put_Duplicate
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_Duplicate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Duplicate;

   function CanChange
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextPara_CanChange);
   end CanChange;

   function IsEqual
     (This  : ITextPara_Type;
      pPara : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextPara_IsEqual,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pPara),
         Free);
   end IsEqual;

   procedure Reset
     (This  : ITextPara_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_Reset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value),
         Free);
   end Reset;

   function Get_Style
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_Style);
   end Get_Style;

   procedure Put_Style
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_Style,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Style;

   function Get_Alignment
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_Alignment);
   end Get_Alignment;

   procedure Put_Alignment
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_Alignment,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Alignment;

   function Get_Hyphenation
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_Hyphenation);
   end Get_Hyphenation;

   procedure Put_Hyphenation
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_Hyphenation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Hyphenation;

   function Get_FirstLineIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_FirstLineIndent);
   end Get_FirstLineIndent;

   function Get_KeepTogether
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_KeepTogether);
   end Get_KeepTogether;

   procedure Put_KeepTogether
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_KeepTogether,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_KeepTogether;

   function Get_KeepWithNext
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_KeepWithNext);
   end Get_KeepWithNext;

   procedure Put_KeepWithNext
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_KeepWithNext,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_KeepWithNext;

   function Get_LeftIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_LeftIndent);
   end Get_LeftIndent;

   function Get_LineSpacing
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_LineSpacing);
   end Get_LineSpacing;

   function Get_LineSpacingRule
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_LineSpacingRule);
   end Get_LineSpacingRule;

   function Get_ListAlignment
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_ListAlignment);
   end Get_ListAlignment;

   procedure Put_ListAlignment
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_ListAlignment,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ListAlignment;

   function Get_ListLevelIndex
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_ListLevelIndex);
   end Get_ListLevelIndex;

   procedure Put_ListLevelIndex
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_ListLevelIndex,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ListLevelIndex;

   function Get_ListStart
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_ListStart);
   end Get_ListStart;

   procedure Put_ListStart
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_ListStart,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ListStart;

   function Get_ListTab
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_ListTab);
   end Get_ListTab;

   procedure Put_ListTab
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_ListTab,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ListTab;

   function Get_ListType
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_ListType);
   end Get_ListType;

   procedure Put_ListType
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_ListType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ListType;

   function Get_NoLineNumber
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_NoLineNumber);
   end Get_NoLineNumber;

   procedure Put_NoLineNumber
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_NoLineNumber,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_NoLineNumber;

   function Get_PageBreakBefore
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_PageBreakBefore);
   end Get_PageBreakBefore;

   procedure Put_PageBreakBefore
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_PageBreakBefore,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_PageBreakBefore;

   function Get_RightIndent
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_RightIndent);
   end Get_RightIndent;

   procedure Put_RightIndent
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_RightIndent,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_RightIndent;

   procedure SetIndents
     (This        : ITextPara_Type;
      StartIndent : GNATCOM.Types.VARIANT;
      LeftIndent  : GNATCOM.Types.VARIANT;
      RightIndent : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_SetIndents,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => RightIndent,
          2 => LeftIndent,
          3 => StartIndent),
         Free);
   end SetIndents;

   procedure SetLineSpacing
     (This            : ITextPara_Type;
      LineSpacingRule : GNATCOM.Types.VARIANT;
      LineSpacing     : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_SetLineSpacing,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => LineSpacing,
          2 => LineSpacingRule),
         Free);
   end SetLineSpacing;

   function Get_SpaceAfter
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_SpaceAfter);
   end Get_SpaceAfter;

   procedure Put_SpaceAfter
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_SpaceAfter,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_SpaceAfter;

   function Get_SpaceBefore
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_SpaceBefore);
   end Get_SpaceBefore;

   procedure Put_SpaceBefore
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_SpaceBefore,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_SpaceBefore;

   function Get_WidowControl
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_WidowControl);
   end Get_WidowControl;

   procedure Put_WidowControl
     (This : ITextPara_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextPara_Put_WidowControl,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_WidowControl;

   function Get_TabCount
     (This : ITextPara_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextPara_Get_TabCount);
   end Get_TabCount;

   procedure AddTab
     (This     : ITextPara_Type;
      tbPos    : GNATCOM.Types.VARIANT;
      tbAlign  : GNATCOM.Types.VARIANT;
      tbLeader : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_AddTab,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => tbLeader,
          2 => tbAlign,
          3 => tbPos),
         Free);
   end AddTab;

   procedure ClearAllTabs
     (This : ITextPara_Type)
   is
   begin
      Invoke (This, ITextPara_ClearAllTabs);
   end ClearAllTabs;

   procedure DeleteTab
     (This  : ITextPara_Type;
      tbPos : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_DeleteTab,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => tbPos),
         Free);
   end DeleteTab;

   procedure GetTab
     (This      : ITextPara_Type;
      iTab      : GNATCOM.Types.VARIANT;
      ptbPos    : GNATCOM.Types.VARIANT;
      ptbAlign  : GNATCOM.Types.VARIANT;
      ptbLeader : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextPara_GetTab,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => ptbLeader,
          2 => ptbAlign,
          3 => ptbPos,
          4 => iTab),
         Free);
   end GetTab;

end TOM.ITextPara_Object;
