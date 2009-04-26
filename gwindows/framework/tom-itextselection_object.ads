with GNATCOM.Dispinterface;

package TOM.ITextSelection_Object is

   type ITextSelection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Text
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Text
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Char
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Char
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Duplicate
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   function Get_FormattedText
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_FormattedText
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Start
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Start
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_End
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_End
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Font
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Font
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Para
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Para
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_StoryLength
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   function Get_StoryType
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Collapse
     (This   : ITextSelection_Type;
      bStart : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   function Expand
     (This : ITextSelection_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function GetIndex
     (This : ITextSelection_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure SetIndex
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Index  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure SetRange
     (This     : ITextSelection_Type;
      cpActive : GNATCOM.Types.VARIANT;
      cpOther  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   function InRange
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function InStory
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function IsEqual
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure uSelect
     (This : ITextSelection_Type);

   function StartOf
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function EndOf
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Move
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStart
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEnd
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStartWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEndWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveStartUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveEndUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindText
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindTextStart
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FindTextEnd
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Delete
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Cut
     (This : ITextSelection_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Copy
     (This : ITextSelection_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Paste
     (This   : ITextSelection_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   function CanPaste
     (This   : ITextSelection_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function CanEdit
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure ChangeCase
     (This  : ITextSelection_Type;
      uType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   procedure GetPoint
     (This  : ITextSelection_Type;
      uType : GNATCOM.Types.VARIANT;
      px    : GNATCOM.Types.VARIANT;
      py    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   procedure SetPoint
     (This   : ITextSelection_Type;
      x      : GNATCOM.Types.VARIANT;
      y      : GNATCOM.Types.VARIANT;
      uType  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);

   procedure ScrollIntoView
     (This  : ITextSelection_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function GetEmbeddedObject
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Flags
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Flags
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Type
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT;

   function MoveLeft
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveRight
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveUp
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveDown
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function HomeKey
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function EndKey
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure TypeText
     (This : ITextSelection_Type;
      bstr : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

end TOM.ITextSelection_Object;
