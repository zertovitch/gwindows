with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextSelection_Interface is

   procedure Initialize (This : in out ITextSelection_Type) is
   begin
      Set_IID (This, IID_ITextSelection);
   end Initialize;

   function Pointer (This : ITextSelection_Type)
     return Pointer_To_ITextSelection
   is
   begin
      return To_Pointer_To_ITextSelection (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextSelection_Type;
                     Pointer : in     Pointer_To_ITextSelection)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Text
     (This  : ITextSelection_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Text
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Text;

   procedure Put_Text
     (This  : ITextSelection_Type;
      pbstr : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Text
         (Pointer (This),
          pbstr));

      if Free then
               GNATCOM.Iinterface.Free (pbstr);

      end if;

   end Put_Text;

   function Get_Char
     (This : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Char
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Char;

   procedure Put_Char
     (This : ITextSelection_Type;
      pch  : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Char
         (Pointer (This),
          pch));

   end Put_Char;

   function Get_Duplicate
     (This    : ITextSelection_Type)
     return Pointer_To_ITextRange
   is
       RetVal : aliased Pointer_To_ITextRange;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Duplicate
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Duplicate;

   function Get_FormattedText
     (This    : ITextSelection_Type)
     return Pointer_To_ITextRange
   is
       RetVal : aliased Pointer_To_ITextRange;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_FormattedText
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_FormattedText;

   procedure Put_FormattedText
     (This    : ITextSelection_Type;
      ppRange : Pointer_To_ITextRange)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_FormattedText
         (Pointer (This),
          ppRange));

   end Put_FormattedText;

   function Get_Start
     (This     : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Start
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Start;

   procedure Put_Start
     (This     : ITextSelection_Type;
      pcpFirst : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Start
         (Pointer (This),
          pcpFirst));

   end Put_Start;

   function Get_End
     (This   : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_End
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_End;

   procedure Put_End
     (This   : ITextSelection_Type;
      pcpLim : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_End
         (Pointer (This),
          pcpLim));

   end Put_End;

   function Get_Font
     (This  : ITextSelection_Type)
     return Pointer_To_ITextFont
   is
       RetVal : aliased Pointer_To_ITextFont;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Font
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Font;

   procedure Put_Font
     (This  : ITextSelection_Type;
      pFont : Pointer_To_ITextFont)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Font
         (Pointer (This),
          pFont));

   end Put_Font;

   function Get_Para
     (This  : ITextSelection_Type)
     return Pointer_To_ITextPara
   is
       RetVal : aliased Pointer_To_ITextPara;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Para
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Para;

   procedure Put_Para
     (This  : ITextSelection_Type;
      pPara : Pointer_To_ITextPara)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Para
         (Pointer (This),
          pPara));

   end Put_Para;

   function Get_StoryLength
     (This : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StoryLength
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StoryLength;

   function Get_StoryType
     (This   : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StoryType
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StoryType;

   procedure Collapse
     (This   : ITextSelection_Type;
      bStart : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Collapse
         (Pointer (This),
          bStart));

   end Collapse;

   function Expand
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Expand
         (Pointer (This),
          Unit,
          RetVal'Unchecked_Access));

      return RetVal;
   end Expand;

   function GetIndex
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetIndex
         (Pointer (This),
          Unit,
          RetVal'Unchecked_Access));

      return RetVal;
   end GetIndex;

   procedure SetIndex
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Index  : Interfaces.C.long;
      Extend : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetIndex
         (Pointer (This),
          Unit,
          Index,
          Extend));

   end SetIndex;

   procedure SetRange
     (This     : ITextSelection_Type;
      cpActive : Interfaces.C.long;
      cpOther  : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetRange
         (Pointer (This),
          cpActive,
          cpOther));

   end SetRange;

   function InRange
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InRange
         (Pointer (This),
          pRange,
          RetVal'Unchecked_Access));

      return RetVal;
   end InRange;

   function InStory
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InStory
         (Pointer (This),
          pRange,
          RetVal'Unchecked_Access));

      return RetVal;
   end InStory;

   function IsEqual
     (This   : ITextSelection_Type;
      pRange : Pointer_To_ITextRange)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsEqual
         (Pointer (This),
          pRange,
          RetVal'Unchecked_Access));

      return RetVal;
   end IsEqual;

   procedure uSelect
     (This : ITextSelection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uSelect
         (Pointer (This)));

   end uSelect;

   function StartOf
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.StartOf
         (Pointer (This),
          Unit,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end StartOf;

   function EndOf
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EndOf
         (Pointer (This),
          Unit,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end EndOf;

   function Move
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Move
         (Pointer (This),
          Unit,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end Move;

   function MoveStart
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveStart
         (Pointer (This),
          Unit,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveStart;

   function MoveEnd
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveEnd
         (Pointer (This),
          Unit,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveEnd;

   function MoveWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveWhile
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveWhile;

   function MoveStartWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveStartWhile
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveStartWhile;

   function MoveEndWhile
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveEndWhile
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveEndWhile;

   function MoveUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveUntil
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveUntil;

   function MoveStartUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveStartUntil
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveStartUntil;

   function MoveEndUntil
     (This   : ITextSelection_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveEndUntil
         (Pointer (This),
          Cset,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveEndUntil;

   function FindText
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FindText
         (Pointer (This),
          bstr,
          cch,
          Flags,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.Iinterface.Free (bstr);

      end if;

      return RetVal;
   end FindText;

   function FindTextStart
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FindTextStart
         (Pointer (This),
          bstr,
          cch,
          Flags,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.Iinterface.Free (bstr);

      end if;

      return RetVal;
   end FindTextStart;

   function FindTextEnd
     (This    : ITextSelection_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Interfaces.C.long;
      Flags   : Interfaces.C.long;
      Free    : Boolean := True)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FindTextEnd
         (Pointer (This),
          bstr,
          cch,
          Flags,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.Iinterface.Free (bstr);

      end if;

      return RetVal;
   end FindTextEnd;

   function Delete
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Delete
         (Pointer (This),
          Unit,
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end Delete;

   procedure Cut
     (This : ITextSelection_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Cut
         (Pointer (This),
          pVar));

   end Cut;

   procedure Copy
     (This : ITextSelection_Type;
      pVar : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Copy
         (Pointer (This),
          pVar));

   end Copy;

   procedure Paste
     (This   : ITextSelection_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Paste
         (Pointer (This),
          pVar,
          Format));

   end Paste;

   function CanPaste
     (This   : ITextSelection_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CanPaste
         (Pointer (This),
          pVar,
          Format,
          RetVal'Unchecked_Access));

      return RetVal;
   end CanPaste;

   function CanEdit
     (This      : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CanEdit
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end CanEdit;

   procedure ChangeCase
     (This  : ITextSelection_Type;
      uType : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ChangeCase
         (Pointer (This),
          uType));

   end ChangeCase;

   procedure GetPoint
     (This  : ITextSelection_Type;
      uType : Interfaces.C.long;
      px    : GNATCOM.Types.Pointer_To_long;
      py    : GNATCOM.Types.Pointer_To_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetPoint
         (Pointer (This),
          uType,
          px,
          py));

   end GetPoint;

   procedure SetPoint
     (This   : ITextSelection_Type;
      x      : Interfaces.C.long;
      y      : Interfaces.C.long;
      uType  : Interfaces.C.long;
      Extend : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetPoint
         (Pointer (This),
          x,
          y,
          uType,
          Extend));

   end SetPoint;

   procedure ScrollIntoView
     (This  : ITextSelection_Type;
      Value : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ScrollIntoView
         (Pointer (This),
          Value));

   end ScrollIntoView;

   function GetEmbeddedObject
     (This : ITextSelection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetEmbeddedObject
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end GetEmbeddedObject;

   function Get_Flags
     (This   : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Flags
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Flags;

   procedure Put_Flags
     (This   : ITextSelection_Type;
      pFlags : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Flags
         (Pointer (This),
          pFlags));

   end Put_Flags;

   function Get_Type
     (This  : ITextSelection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Type
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Type;

   function MoveLeft
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveLeft
         (Pointer (This),
          Unit,
          Count,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveLeft;

   function MoveRight
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveRight
         (Pointer (This),
          Unit,
          Count,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveRight;

   function MoveUp
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveUp
         (Pointer (This),
          Unit,
          Count,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveUp;

   function MoveDown
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Count  : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveDown
         (Pointer (This),
          Unit,
          Count,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end MoveDown;

   function HomeKey
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.HomeKey
         (Pointer (This),
          Unit,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end HomeKey;

   function EndKey
     (This   : ITextSelection_Type;
      Unit   : Interfaces.C.long;
      Extend : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EndKey
         (Pointer (This),
          Unit,
          Extend,
          RetVal'Unchecked_Access));

      return RetVal;
   end EndKey;

   procedure TypeText
     (This : ITextSelection_Type;
      bstr : GNATCOM.Types.BSTR;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.TypeText
         (Pointer (This),
          bstr));

      if Free then
               GNATCOM.Iinterface.Free (bstr);

      end if;

   end TypeText;

end TOM.ITextSelection_Interface;
