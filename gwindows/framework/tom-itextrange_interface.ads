with GNATCOM.Dispinterface;

package TOM.ITextRange_Interface is

   type ITextRange_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextRange_Type);

   function Pointer (This : ITextRange_Type)
     return Pointer_To_ITextRange;

   procedure Attach (This    : in out ITextRange_Type;
                     Pointer : in     Pointer_To_ITextRange);

   function Get_Text
     (This  : ITextRange_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Text
     (This  : ITextRange_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True);

   function Get_Char
     (This : ITextRange_Type)
     return Interfaces.C.long;

   procedure Put_Char
     (This : ITextRange_Type;
      pch  : Interfaces.C.long);

   function Get_Duplicate
     (This    : ITextRange_Type)
     return Pointer_To_ITextRange;

   function Get_FormattedText
     (This    : ITextRange_Type)
     return Pointer_To_ITextRange;

   procedure Put_FormattedText
     (This    : ITextRange_Type;
      ppRange : Pointer_To_ITextRange);

   function Get_Start
     (This     : ITextRange_Type)
     return Interfaces.C.long;

   procedure Put_Start
     (This     : ITextRange_Type;
      pcpFirst : Interfaces.C.long);

   function Get_End
     (This   : ITextRange_Type)
     return Interfaces.C.long;

   procedure Put_End
     (This   : ITextRange_Type;
      pcpLim : Interfaces.C.long);

   function Get_Font
     (This  : ITextRange_Type)
     return Pointer_To_ITextFont;

   procedure Put_Font
     (This  : ITextRange_Type;
      pFont : Pointer_To_ITextFont);

   function Get_Para
     (This  : ITextRange_Type)
     return Pointer_To_ITextPara;

   procedure Put_Para
     (This  : ITextRange_Type;
      pPara : Pointer_To_ITextPara);

   function Get_StoryLength
     (This : ITextRange_Type)
     return Interfaces.C.long;

   function Get_StoryType
     (This   : ITextRange_Type)
     return Interfaces.C.long;

   procedure Collapse
     (This   : ITextRange_Type;
      bStart : Interfaces.C.long);

   function Expand
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long;

   function GetIndex
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long;

   procedure SetIndex
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Index  : Interfaces.C.long;
      Extend : Interfaces.C.long);

   procedure SetRange
     (This     : ITextRange_Type;
      cpActive : Interfaces.C.long;
      cpOther  : Interfaces.C.long);

   function InRange
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   function InStory
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   function IsEqual
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long;

   procedure uSelect
     (This : ITextRange_Type);

   function StartOf
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function EndOf
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long;

   function Move
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStart
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEnd
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStartWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEndWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveStartUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function MoveEndUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   function FindText
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function FindTextStart
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function FindTextEnd
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long;

   function Delete
     (This   : ITextRange_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long;

   procedure Cut
     (This : ITextRange_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure Copy
     (This : ITextRange_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure Paste
     (This   : ITextRange_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Interfaces.C.long);

   function CanPaste
     (This   : ITextRange_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Interfaces.C.long)
     return Interfaces.C.long;

   function CanEdit
     (This      : ITextRange_Type)
     return Interfaces.C.long;

   procedure ChangeCase
     (This  : ITextRange_Type;
      uType : Interfaces.C.long);

   procedure GetPoint
     (This  : ITextRange_Type;
      uType : Interfaces.C.long;
      px    : GNATCOM.Types.Pointer_To_long;
      py    : GNATCOM.Types.Pointer_To_long);

   procedure SetPoint
     (This   : ITextRange_Type;
      x      : Interfaces.C.long;
      y      : Interfaces.C.long;
      uType  : Interfaces.C.long;
      Extend : Interfaces.C.long);

   procedure ScrollIntoView
     (This  : ITextRange_Type;
      Value : Interfaces.C.long);

   function GetEmbeddedObject
     (This : ITextRange_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

end TOM.ITextRange_Interface;
