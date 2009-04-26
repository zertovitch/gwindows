package body TOM.ITextRange_Object is

   function Get_Text
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Text);
   end Get_Text;

   procedure Put_Text
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_Text,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Text;

   function Get_Char
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Char);
   end Get_Char;

   procedure Put_Char
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_Char,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Char;

   function Get_Duplicate
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Duplicate);
   end Get_Duplicate;

   function Get_FormattedText
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_FormattedText);
   end Get_FormattedText;

   procedure Put_FormattedText
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_FormattedText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_FormattedText;

   function Get_Start
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Start);
   end Get_Start;

   procedure Put_Start
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_Start,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Start;

   function Get_End
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_End);
   end Get_End;

   procedure Put_End
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_End,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_End;

   function Get_Font
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Font);
   end Get_Font;

   procedure Put_Font
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_Font,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Font;

   function Get_Para
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_Para);
   end Get_Para;

   procedure Put_Para
     (This : ITextRange_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextRange_Put_Para,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Para;

   function Get_StoryLength
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_StoryLength);
   end Get_StoryLength;

   function Get_StoryType
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextRange_Get_StoryType);
   end Get_StoryType;

   procedure Collapse
     (This   : ITextRange_Type;
      bStart : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_Collapse,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bStart),
         Free);
   end Collapse;

   function Expand
     (This : ITextRange_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_Expand,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Unit),
         Free);
   end Expand;

   function GetIndex
     (This : ITextRange_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_GetIndex,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Unit),
         Free);
   end GetIndex;

   procedure SetIndex
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Index  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_SetIndex,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Index,
          3 => Unit),
         Free);
   end SetIndex;

   procedure SetRange
     (This     : ITextRange_Type;
      cpActive : GNATCOM.Types.VARIANT;
      cpOther  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_SetRange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => cpOther,
          2 => cpActive),
         Free);
   end SetRange;

   function InRange
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_InRange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end InRange;

   function InStory
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_InStory,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end InStory;

   function IsEqual
     (This   : ITextRange_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_IsEqual,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end IsEqual;

   procedure uSelect
     (This : ITextRange_Type)
   is
   begin
      Invoke (This, ITextRange_uSelect);
   end uSelect;

   function StartOf
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_StartOf,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end StartOf;

   function EndOf
     (This   : ITextRange_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_EndOf,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end EndOf;

   function Move
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_Move,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end Move;

   function MoveStart
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveStart,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end MoveStart;

   function MoveEnd
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveEnd,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end MoveEnd;

   function MoveWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveWhile;

   function MoveStartWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveStartWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveStartWhile;

   function MoveEndWhile
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveEndWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveEndWhile;

   function MoveUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveUntil;

   function MoveStartUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveStartUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveStartUntil;

   function MoveEndUntil
     (This  : ITextRange_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_MoveEndUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveEndUntil;

   function FindText
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_FindText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindText;

   function FindTextStart
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_FindTextStart,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindTextStart;

   function FindTextEnd
     (This  : ITextRange_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_FindTextEnd,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindTextEnd;

   function Delete
     (This  : ITextRange_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end Delete;

   procedure Cut
     (This : ITextRange_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_Cut,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pVar),
         Free);
   end Cut;

   procedure Copy
     (This : ITextRange_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_Copy,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pVar),
         Free);
   end Copy;

   procedure Paste
     (This   : ITextRange_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_Paste,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Format,
          2 => pVar),
         Free);
   end Paste;

   function CanPaste
     (This   : ITextRange_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextRange_CanPaste,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Format,
          2 => pVar),
         Free);
   end CanPaste;

   function CanEdit
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextRange_CanEdit);
   end CanEdit;

   procedure ChangeCase
     (This  : ITextRange_Type;
      uType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_ChangeCase,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => uType),
         Free);
   end ChangeCase;

   procedure GetPoint
     (This  : ITextRange_Type;
      uType : GNATCOM.Types.VARIANT;
      px    : GNATCOM.Types.VARIANT;
      py    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_GetPoint,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => py,
          2 => px,
          3 => uType),
         Free);
   end GetPoint;

   procedure SetPoint
     (This   : ITextRange_Type;
      x      : GNATCOM.Types.VARIANT;
      y      : GNATCOM.Types.VARIANT;
      uType  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_SetPoint,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => uType,
          3 => y,
          4 => x),
         Free);
   end SetPoint;

   procedure ScrollIntoView
     (This  : ITextRange_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextRange_ScrollIntoView,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value),
         Free);
   end ScrollIntoView;

   function GetEmbeddedObject
     (This : ITextRange_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextRange_GetEmbeddedObject);
   end GetEmbeddedObject;

end TOM.ITextRange_Object;
