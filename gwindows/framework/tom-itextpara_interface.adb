with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextPara_Interface is

   procedure Initialize (This : in out ITextPara_Type) is
   begin
      Set_IID (This, IID_ITextPara);
   end Initialize;

   function Pointer (This : ITextPara_Type)
     return Pointer_To_ITextPara
   is
   begin
      return To_Pointer_To_ITextPara (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextPara_Type;
                     Pointer : in     Pointer_To_ITextPara)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Duplicate
     (This   : ITextPara_Type)
     return Pointer_To_ITextPara
   is
       RetVal : aliased Pointer_To_ITextPara;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Duplicate
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Duplicate;

   procedure Put_Duplicate
     (This   : ITextPara_Type;
      ppPara : Pointer_To_ITextPara)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Duplicate
         (Pointer (This),
          ppPara));

   end Put_Duplicate;

   function CanChange
     (This : ITextPara_Type)
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
     (This  : ITextPara_Type;
      pPara : Pointer_To_ITextPara)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsEqual
         (Pointer (This),
          pPara,
          RetVal'Unchecked_Access));

      return RetVal;
   end IsEqual;

   procedure Reset
     (This  : ITextPara_Type;
      Value : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This),
          Value));

   end Reset;

   function Get_Style
     (This   : ITextPara_Type)
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
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Style
         (Pointer (This),
          pValue));

   end Put_Style;

   function Get_Alignment
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Alignment
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Alignment;

   procedure Put_Alignment
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Alignment
         (Pointer (This),
          pValue));

   end Put_Alignment;

   function Get_Hyphenation
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Hyphenation
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Hyphenation;

   procedure Put_Hyphenation
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Hyphenation
         (Pointer (This),
          pValue));

   end Put_Hyphenation;

   function Get_FirstLineIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_FirstLineIndent
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_FirstLineIndent;

   function Get_KeepTogether
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_KeepTogether
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_KeepTogether;

   procedure Put_KeepTogether
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_KeepTogether
         (Pointer (This),
          pValue));

   end Put_KeepTogether;

   function Get_KeepWithNext
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_KeepWithNext
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_KeepWithNext;

   procedure Put_KeepWithNext
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_KeepWithNext
         (Pointer (This),
          pValue));

   end Put_KeepWithNext;

   function Get_LeftIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LeftIndent
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LeftIndent;

   function Get_LineSpacing
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LineSpacing
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LineSpacing;

   function Get_LineSpacingRule
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LineSpacingRule
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LineSpacingRule;

   function Get_ListAlignment
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ListAlignment
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ListAlignment;

   procedure Put_ListAlignment
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ListAlignment
         (Pointer (This),
          pValue));

   end Put_ListAlignment;

   function Get_ListLevelIndex
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ListLevelIndex
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ListLevelIndex;

   procedure Put_ListLevelIndex
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ListLevelIndex
         (Pointer (This),
          pValue));

   end Put_ListLevelIndex;

   function Get_ListStart
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ListStart
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ListStart;

   procedure Put_ListStart
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ListStart
         (Pointer (This),
          pValue));

   end Put_ListStart;

   function Get_ListTab
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ListTab
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ListTab;

   procedure Put_ListTab
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ListTab
         (Pointer (This),
          pValue));

   end Put_ListTab;

   function Get_ListType
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ListType
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ListType;

   procedure Put_ListType
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ListType
         (Pointer (This),
          pValue));

   end Put_ListType;

   function Get_NoLineNumber
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_NoLineNumber
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_NoLineNumber;

   procedure Put_NoLineNumber
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_NoLineNumber
         (Pointer (This),
          pValue));

   end Put_NoLineNumber;

   function Get_PageBreakBefore
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_PageBreakBefore
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_PageBreakBefore;

   procedure Put_PageBreakBefore
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_PageBreakBefore
         (Pointer (This),
          pValue));

   end Put_PageBreakBefore;

   function Get_RightIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_RightIndent
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_RightIndent;

   procedure Put_RightIndent
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_RightIndent
         (Pointer (This),
          pValue));

   end Put_RightIndent;

   procedure SetIndents
     (This        : ITextPara_Type;
      StartIndent : Interfaces.C.C_float;
      LeftIndent  : Interfaces.C.C_float;
      RightIndent : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetIndents
         (Pointer (This),
          StartIndent,
          LeftIndent,
          RightIndent));

   end SetIndents;

   procedure SetLineSpacing
     (This            : ITextPara_Type;
      LineSpacingRule : Interfaces.C.long;
      LineSpacing     : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetLineSpacing
         (Pointer (This),
          LineSpacingRule,
          LineSpacing));

   end SetLineSpacing;

   function Get_SpaceAfter
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SpaceAfter
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SpaceAfter;

   procedure Put_SpaceAfter
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_SpaceAfter
         (Pointer (This),
          pValue));

   end Put_SpaceAfter;

   function Get_SpaceBefore
     (This   : ITextPara_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SpaceBefore
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SpaceBefore;

   procedure Put_SpaceBefore
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_SpaceBefore
         (Pointer (This),
          pValue));

   end Put_SpaceBefore;

   function Get_WidowControl
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_WidowControl
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_WidowControl;

   procedure Put_WidowControl
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_WidowControl
         (Pointer (This),
          pValue));

   end Put_WidowControl;

   function Get_TabCount
     (This   : ITextPara_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_TabCount
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_TabCount;

   procedure AddTab
     (This     : ITextPara_Type;
      tbPos    : Interfaces.C.C_float;
      tbAlign  : Interfaces.C.long;
      tbLeader : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddTab
         (Pointer (This),
          tbPos,
          tbAlign,
          tbLeader));

   end AddTab;

   procedure ClearAllTabs
     (This : ITextPara_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ClearAllTabs
         (Pointer (This)));

   end ClearAllTabs;

   procedure DeleteTab
     (This  : ITextPara_Type;
      tbPos : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DeleteTab
         (Pointer (This),
          tbPos));

   end DeleteTab;

   procedure GetTab
     (This      : ITextPara_Type;
      iTab      : Interfaces.C.long;
      ptbPos    : GNATCOM.Types.Pointer_To_C_float;
      ptbAlign  : GNATCOM.Types.Pointer_To_long;
      ptbLeader : GNATCOM.Types.Pointer_To_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetTab
         (Pointer (This),
          iTab,
          ptbPos,
          ptbAlign,
          ptbLeader));

   end GetTab;

end TOM.ITextPara_Interface;
