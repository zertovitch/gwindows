with GNATCOM.Dispinterface;

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
     return Interfaces.C.long;

   procedure Put_Char
     (This : ITextSelection_Type;
      pch  : Interfaces.C.long);

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
     return Interfaces.C.long;

   procedure Put_Start
     (This     : ITextSelection_Type;
      pcpFirst : Interfaces.C.long);

   function Get_End
     (This   : ITextSelection_Type)
     return Interfaces.C.long;

   procedure Put_End
     (This   : ITextSelection_Type;
      pcpLim : Interfaces.C.long);

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
     return Interfaces.C.long;

   function Get_StoryType
     (This   : ITextSelection_Type)
     return Interfaces.C.long;

   procedure Collapse
     (This   : ITextSelection_Type;
      bStart : Interfaces.C.long);

   function Expand
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long;

   function GetIndex
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long;

   procedure SetIndex
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Index  : Interfaces.C.long;
      Extend : Interfaces.C.long);

   procedure SetRange
     (This     : ITextSelection_Type;
      cpActive : Interfaces.C.long;
      cpOther  : Interfaces.C.long);

   function InRange
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   function InStory
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   function IsEqual
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   procedure uSelect
     (This : ITextSelection_Type);

   function StartOf
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function EndOf
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function Move
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStart
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEnd
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStartWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEndWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStartUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEndUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function FindText
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function FindTextStart
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function FindTextEnd
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function Delete
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

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
      Format : Interfaces.C.long);

   function CanPaste
     (This   : ITextSelection_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Interfaces.C.long)
     return Interfaces.C.long;

   function CanEdit
     (This      : ITextSelection_Type)
     return Interfaces.C.long;

   procedure ChangeCase
     (This  : ITextSelection_Type;
      uType : Interfaces.C.long);

   procedure GetPoint
     (This  : ITextSelection_Type;
      uType : Interfaces.C.long;
      px    : GNATCOM.Types.Pointer_To_long;
      py    : GNATCOM.Types.Pointer_To_long);

   procedure SetPoint
     (This   : ITextSelection_Type;
      x      : Interfaces.C.long;
      y      : Interfaces.C.long;
      uType  : Interfaces.C.long;
      Extend : Interfaces.C.long);

   procedure ScrollIntoView
     (This  : ITextSelection_Type;
      Value : Interfaces.C.long);

   function GetEmbeddedObject
     (This : ITextSelection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function Get_Flags
     (This   : ITextSelection_Type)
     return Interfaces.C.long;

   procedure Put_Flags
     (This   : ITextSelection_Type;
      pFlags : Interfaces.C.long);

   function Get_Type
     (This  : ITextSelection_Type)
     return Interfaces.C.long;

   function MoveLeft
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveRight
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveUp
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveDown
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function HomeKey
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function EndKey
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   procedure TypeText
     (This : ITextSelection_Type;
      bstr : GNATCOM.Types.BSTR;
      Free : Boolean := True);

end TOM.ITextSelection_Interface;
