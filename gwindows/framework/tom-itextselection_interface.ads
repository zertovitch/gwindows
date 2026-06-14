with GNATCOM.Dispinterface;
with Win32_Types;

package TOM.ITextSelection_Interface is

   type ITextSelection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextSelection_Type);

   function Pointer (This : ITextSelection_Type)
     return Pointer_To_ITextSelection;

   procedure Attach (This    : in out ITextSelection_Type;
                     Pointer : in     Pointer_To_ITextSelection);

   function Get_Text
     (This  : ITextSelection_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Text
     (This  : ITextSelection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_Char
     (This : ITextSelection_Type)
     return Win32_Types.Long;

   procedure Put_Char
     (This : ITextSelection_Type;
      pch  : Win32_Types.Long);

   function Get_Duplicate
     (This    : ITextSelection_Type)
     return Pointer_To_ITextRange;

   function Get_FormattedText
     (This    : ITextSelection_Type)
     return Pointer_To_ITextRange;

   procedure Put_FormattedText
     (This    : ITextSelection_Type;
      ppRange : Pointer_To_ITextRange);

   function Get_Start
     (This     : ITextSelection_Type)
     return Win32_Types.Long;

   procedure Put_Start
     (This     : ITextSelection_Type;
      pcpFirst : Win32_Types.Long);

   function Get_End
     (This   : ITextSelection_Type)
     return Win32_Types.Long;

   procedure Put_End
     (This   : ITextSelection_Type;
      pcpLim : Win32_Types.Long);

   function Get_Font
     (This  : ITextSelection_Type)
     return Pointer_To_ITextFont;

   procedure Put_Font
     (This  : ITextSelection_Type;
      pFont : Pointer_To_ITextFont);

   function Get_Para
     (This  : ITextSelection_Type)
     return Pointer_To_ITextPara;

   procedure Put_Para
     (This  : ITextSelection_Type;
      pPara : Pointer_To_ITextPara);

   function Get_StoryLength
     (This : ITextSelection_Type)
     return Win32_Types.Long;

   function Get_StoryType
     (This   : ITextSelection_Type)
     return Win32_Types.Long;

   procedure Collapse
     (This   : ITextSelection_Type;
      bStart : Win32_Types.Long);

   function Expand
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long)
     return Win32_Types.Long;

   function GetIndex
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long)
     return Win32_Types.Long;

   procedure SetIndex
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Index  : Win32_Types.Long;
      Extend : Win32_Types.Long);

   procedure SetRange
     (This     : ITextSelection_Type;
      cpActive : Win32_Types.Long;
      cpOther  : Win32_Types.Long);

   function InRange
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   function InStory
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   function IsEqual
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   procedure uSelect
     (This : ITextSelection_Type);

   function StartOf
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function EndOf
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function Move
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStart
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEnd
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStartWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEndWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStartUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEndUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function FindText
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function FindTextStart
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function FindTextEnd
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function Delete
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   procedure Cut
     (This : ITextSelection_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure Copy
     (This : ITextSelection_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure Paste
     (This   : ITextSelection_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Win32_Types.Long);

   function CanPaste
     (This   : ITextSelection_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Win32_Types.Long)
     return Win32_Types.Long;

   function CanEdit
     (This      : ITextSelection_Type)
     return Win32_Types.Long;

   procedure ChangeCase
     (This  : ITextSelection_Type;
      uType : Win32_Types.Long);

   procedure GetPoint
     (This  : ITextSelection_Type;
      uType : Win32_Types.Long;
      px    : GNATCOM.Types.Pointer_To_long;
      py    : GNATCOM.Types.Pointer_To_long);

   procedure SetPoint
     (This   : ITextSelection_Type;
      x      : Win32_Types.Long;
      y      : Win32_Types.Long;
      uType  : Win32_Types.Long;
      Extend : Win32_Types.Long);

   procedure ScrollIntoView
     (This  : ITextSelection_Type;
      Value : Win32_Types.Long);

   function GetEmbeddedObject
     (This : ITextSelection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function Get_Flags
     (This   : ITextSelection_Type)
     return Win32_Types.Long;

   procedure Put_Flags
     (This   : ITextSelection_Type;
      pFlags : Win32_Types.Long);

   function Get_Type
     (This  : ITextSelection_Type)
     return Win32_Types.Long;

   function MoveLeft
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveRight
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveUp
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveDown
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function HomeKey
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function EndKey
     (This   : ITextSelection_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   procedure TypeText
     (This : ITextSelection_Type;
      bstr : GNATCOM.Types.BSTR;
      Free : Boolean := True);

end TOM.ITextSelection_Interface;
