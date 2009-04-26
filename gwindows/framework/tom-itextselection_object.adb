package body TOM.ITextSelection_Object is

   function Get_Text
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Text);
   end Get_Text;

   procedure Put_Text
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Text,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Text;

   function Get_Char
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Char);
   end Get_Char;

   procedure Put_Char
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Char,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Char;

   function Get_Duplicate
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Duplicate);
   end Get_Duplicate;

   function Get_FormattedText
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_FormattedText);
   end Get_FormattedText;

   procedure Put_FormattedText
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_FormattedText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_FormattedText;

   function Get_Start
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Start);
   end Get_Start;

   procedure Put_Start
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Start,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Start;

   function Get_End
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_End);
   end Get_End;

   procedure Put_End
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_End,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_End;

   function Get_Font
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Font);
   end Get_Font;

   procedure Put_Font
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Font,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Font;

   function Get_Para
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Para);
   end Get_Para;

   procedure Put_Para
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Para,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Para;

   function Get_StoryLength
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_StoryLength);
   end Get_StoryLength;

   function Get_StoryType
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_StoryType);
   end Get_StoryType;

   procedure Collapse
     (This   : ITextSelection_Type;
      bStart : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_Collapse,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bStart),
         Free);
   end Collapse;

   function Expand
     (This : ITextSelection_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_Expand,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Unit),
         Free);
   end Expand;

   function GetIndex
     (This : ITextSelection_Type;
      Unit : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_GetIndex,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Unit),
         Free);
   end GetIndex;

   procedure SetIndex
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Index  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_SetIndex,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Index,
          3 => Unit),
         Free);
   end SetIndex;

   procedure SetRange
     (This     : ITextSelection_Type;
      cpActive : GNATCOM.Types.VARIANT;
      cpOther  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_SetRange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => cpOther,
          2 => cpActive),
         Free);
   end SetRange;

   function InRange
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_InRange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end InRange;

   function InStory
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_InStory,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end InStory;

   function IsEqual
     (This   : ITextSelection_Type;
      pRange : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_IsEqual,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRange),
         Free);
   end IsEqual;

   procedure uSelect
     (This : ITextSelection_Type)
   is
   begin
      Invoke (This, ITextSelection_uSelect);
   end uSelect;

   function StartOf
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_StartOf,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end StartOf;

   function EndOf
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_EndOf,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end EndOf;

   function Move
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_Move,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end Move;

   function MoveStart
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveStart,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end MoveStart;

   function MoveEnd
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveEnd,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end MoveEnd;

   function MoveWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveWhile;

   function MoveStartWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveStartWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveStartWhile;

   function MoveEndWhile
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveEndWhile,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveEndWhile;

   function MoveUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveUntil;

   function MoveStartUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveStartUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveStartUntil;

   function MoveEndUntil
     (This  : ITextSelection_Type;
      Cset  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveEndUntil,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Cset),
         Free);
   end MoveEndUntil;

   function FindText
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_FindText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindText;

   function FindTextStart
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_FindTextStart,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindTextStart;

   function FindTextEnd
     (This  : ITextSelection_Type;
      bstr  : GNATCOM.Types.VARIANT;
      cch   : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_FindTextEnd,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Flags,
          2 => cch,
          3 => bstr),
         Free);
   end FindTextEnd;

   function Delete
     (This  : ITextSelection_Type;
      Unit  : GNATCOM.Types.VARIANT;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count,
          2 => Unit),
         Free);
   end Delete;

   procedure Cut
     (This : ITextSelection_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_Cut,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pVar),
         Free);
   end Cut;

   procedure Copy
     (This : ITextSelection_Type;
      pVar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_Copy,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pVar),
         Free);
   end Copy;

   procedure Paste
     (This   : ITextSelection_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_Paste,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Format,
          2 => pVar),
         Free);
   end Paste;

   function CanPaste
     (This   : ITextSelection_Type;
      pVar   : GNATCOM.Types.VARIANT;
      Format : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_CanPaste,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Format,
          2 => pVar),
         Free);
   end CanPaste;

   function CanEdit
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextSelection_CanEdit);
   end CanEdit;

   procedure ChangeCase
     (This  : ITextSelection_Type;
      uType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_ChangeCase,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => uType),
         Free);
   end ChangeCase;

   procedure GetPoint
     (This  : ITextSelection_Type;
      uType : GNATCOM.Types.VARIANT;
      px    : GNATCOM.Types.VARIANT;
      py    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_GetPoint,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => py,
          2 => px,
          3 => uType),
         Free);
   end GetPoint;

   procedure SetPoint
     (This   : ITextSelection_Type;
      x      : GNATCOM.Types.VARIANT;
      y      : GNATCOM.Types.VARIANT;
      uType  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_SetPoint,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => uType,
          3 => y,
          4 => x),
         Free);
   end SetPoint;

   procedure ScrollIntoView
     (This  : ITextSelection_Type;
      Value : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_ScrollIntoView,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Value),
         Free);
   end ScrollIntoView;

   function GetEmbeddedObject
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextSelection_GetEmbeddedObject);
   end GetEmbeddedObject;

   function Get_Flags
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Flags);
   end Get_Flags;

   procedure Put_Flags
     (This : ITextSelection_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextSelection_Put_Flags,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Flags;

   function Get_Type
     (This : ITextSelection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextSelection_Get_Type);
   end Get_Type;

   function MoveLeft
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveLeft,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Count,
          3 => Unit),
         Free);
   end MoveLeft;

   function MoveRight
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveRight,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Count,
          3 => Unit),
         Free);
   end MoveRight;

   function MoveUp
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveUp,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Count,
          3 => Unit),
         Free);
   end MoveUp;

   function MoveDown
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Count  : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_MoveDown,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Count,
          3 => Unit),
         Free);
   end MoveDown;

   function HomeKey
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_HomeKey,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end HomeKey;

   function EndKey
     (This   : ITextSelection_Type;
      Unit   : GNATCOM.Types.VARIANT;
      Extend : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextSelection_EndKey,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Extend,
          2 => Unit),
         Free);
   end EndKey;

   procedure TypeText
     (This : ITextSelection_Type;
      bstr : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextSelection_TypeText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstr),
         Free);
   end TypeText;

end TOM.ITextSelection_Object;
