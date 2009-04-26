with GNATCOM.Dispinterface;

package TOM.ITextPara_Interface is

   type ITextPara_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextPara_Type);

   function Pointer (This : ITextPara_Type)
     return Pointer_To_ITextPara;

   procedure Attach (This    : in out ITextPara_Type;
                     Pointer : in     Pointer_To_ITextPara);

   function Get_Duplicate
     (This   : ITextPara_Type)
     return Pointer_To_ITextPara;

   procedure Put_Duplicate
     (This   : ITextPara_Type;
      ppPara : Pointer_To_ITextPara);

   function CanChange
     (This : ITextPara_Type)
     return Interfaces.C.long;

   function IsEqual
     (This  : ITextPara_Type;
      pPara : Pointer_To_ITextPara)
     return Interfaces.C.long;

   procedure Reset
     (This  : ITextPara_Type;
      Value : Interfaces.C.long);

   function Get_Style
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_Style
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_Alignment
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_Alignment
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_Hyphenation
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_Hyphenation
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_FirstLineIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_KeepTogether
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_KeepTogether
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_KeepWithNext
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_KeepWithNext
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_LeftIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_LineSpacing
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_LineSpacingRule
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   function Get_ListAlignment
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_ListAlignment
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_ListLevelIndex
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_ListLevelIndex
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_ListStart
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_ListStart
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_ListTab
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   procedure Put_ListTab
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float);

   function Get_ListType
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_ListType
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_NoLineNumber
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_NoLineNumber
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_PageBreakBefore
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_PageBreakBefore
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_RightIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   procedure Put_RightIndent
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float);

   procedure SetIndents
     (This        : ITextPara_Type;
      StartIndent : Interfaces.C.C_float;
      LeftIndent  : Interfaces.C.C_float;
      RightIndent : Interfaces.C.C_float);

   procedure SetLineSpacing
     (This            : ITextPara_Type;
      LineSpacingRule : Interfaces.C.long;
      LineSpacing     : Interfaces.C.C_float);

   function Get_SpaceAfter
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   procedure Put_SpaceAfter
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float);

   function Get_SpaceBefore
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   procedure Put_SpaceBefore
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float);

   function Get_WidowControl
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure Put_WidowControl
     (This   : ITextPara_Type;
      pValue : Interfaces.C.long);

   function Get_TabCount
     (This   : ITextPara_Type)
     return Interfaces.C.long;

   procedure AddTab
     (This     : ITextPara_Type;
      tbPos    : Interfaces.C.C_float;
      tbAlign  : Interfaces.C.long;
      tbLeader : Interfaces.C.long);

   procedure ClearAllTabs
     (This : ITextPara_Type);

   procedure DeleteTab
     (This  : ITextPara_Type;
      tbPos : Interfaces.C.C_float);

   procedure GetTab
     (This      : ITextPara_Type;
      iTab      : Interfaces.C.long;
      ptbPos    : GNATCOM.Types.Pointer_To_C_float;
      ptbAlign  : GNATCOM.Types.Pointer_To_long;
      ptbLeader : GNATCOM.Types.Pointer_To_long);

end TOM.ITextPara_Interface;
