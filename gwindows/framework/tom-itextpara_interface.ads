with GNATCOM.Dispinterface;
with Win32_Types;

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
     return Win32_Types.Long;

   function IsEqual
     (This  : ITextPara_Type;
      pPara : Pointer_To_ITextPara)
     return Win32_Types.Long;

   procedure Reset
     (This  : ITextPara_Type;
      Value : Win32_Types.Long);

   function Get_Style
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_Style
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_Alignment
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_Alignment
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_Hyphenation
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_Hyphenation
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_FirstLineIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_KeepTogether
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_KeepTogether
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_KeepWithNext
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_KeepWithNext
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_LeftIndent
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_LineSpacing
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   function Get_LineSpacingRule
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   function Get_ListAlignment
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_ListAlignment
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_ListLevelIndex
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_ListLevelIndex
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_ListStart
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_ListStart
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_ListTab
     (This   : ITextPara_Type)
     return Interfaces.C.C_float;

   procedure Put_ListTab
     (This   : ITextPara_Type;
      pValue : Interfaces.C.C_float);

   function Get_ListType
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_ListType
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_NoLineNumber
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_NoLineNumber
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_PageBreakBefore
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure Put_PageBreakBefore
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

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
      LineSpacingRule : Win32_Types.Long;
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
     return Win32_Types.Long;

   procedure Put_WidowControl
     (This   : ITextPara_Type;
      pValue : Win32_Types.Long);

   function Get_TabCount
     (This   : ITextPara_Type)
     return Win32_Types.Long;

   procedure AddTab
     (This     : ITextPara_Type;
      tbPos    : Interfaces.C.C_float;
      tbAlign  : Win32_Types.Long;
      tbLeader : Win32_Types.Long);

   procedure ClearAllTabs
     (This : ITextPara_Type);

   procedure DeleteTab
     (This  : ITextPara_Type;
      tbPos : Interfaces.C.C_float);

   procedure GetTab
     (This      : ITextPara_Type;
      iTab      : Win32_Types.Long;
      ptbPos    : GNATCOM.Types.Pointer_To_C_float;
      ptbAlign  : GNATCOM.Types.Pointer_To_long;
      ptbLeader : GNATCOM.Types.Pointer_To_long);

end TOM.ITextPara_Interface;
