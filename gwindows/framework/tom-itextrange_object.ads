with GNATCOM.Dispinterface;

package TOM.ITextRange_Object is

   type ITextRange_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Text
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Text
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Char
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Char
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Duplicate
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   function Get_FormattedText
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_FormattedText
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Start
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Start
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_End
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_End
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Font
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Font
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Para
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Para
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_StoryLength
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   function Get_StoryType
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure Collapse
     (This   : ITextRange_Type;
      bStart : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   function Expand
     (This : ITextRange_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function GetIndex
     (This : ITextRange_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure SetIndex
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Index  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure SetRange
     (This     : ITextRange_Type;
      cpActive : GNATCOM.Types.VARIANT;
      cpOther  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   function InRange
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function InStory
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function IsEqual
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure uSelect
     (This : ITextRange_Type);

   function StartOf
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function EndOf
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Move
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStart
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEnd
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStartWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEndWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStartUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEndUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindText
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindTextStart
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindTextEnd
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Delete
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Cut
     (This : ITextRange_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Copy
     (This : ITextRange_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Paste
     (This   : ITextRange_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   function CanPaste
     (This   : ITextRange_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function CanEdit
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

   procedure ChangeCase
     (This  : ITextRange_Type;
      uType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   procedure GetPoint
     (This  : ITextRange_Type;
      uType : GNATCOM.Types.VARIANT;
      px    : GNATCOM.Types.VARIANT;
      py    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   procedure SetPoint
     (This   : ITextRange_Type;
      x      : GNATCOM.Types.VARIANT;
      y      : GNATCOM.Types.VARIANT;
      uType  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure ScrollIntoView
     (This  : ITextRange_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function GetEmbeddedObject
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT;

end TOM.ITextRange_Object;
