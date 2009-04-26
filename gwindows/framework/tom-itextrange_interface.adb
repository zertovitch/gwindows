with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextRange_Interface is

   procedure Initialize (This : in out ITextRange_Type) is
   begin
      Set_IID (This, IID_ITextRange);
   end Initialize;

   function Pointer (This : ITextRange_Type)
     return Pointer_To_ITextRange
   is
   begin
      return To_Pointer_To_ITextRange (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextRange_Type;
                     Pointer : in     Pointer_To_ITextRange)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Text
     (This  : ITextRange_Type)
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
     (This  : ITextRange_Type;
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
     (This : ITextRange_Type)
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
     (This : ITextRange_Type;
      pch  : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Char
         (Pointer (This),
          pch));

   end Put_Char;

   function Get_Duplicate
     (This    : ITextRange_Type)
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
     (This    : ITextRange_Type)
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
     (This    : ITextRange_Type;
      ppRange : Pointer_To_ITextRange)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_FormattedText
         (Pointer (This),
          ppRange));

   end Put_FormattedText;

   function Get_Start
     (This     : ITextRange_Type)
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
     (This     : ITextRange_Type;
      pcpFirst : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Start
         (Pointer (This),
          pcpFirst));

   end Put_Start;

   function Get_End
     (This   : ITextRange_Type)
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
     (This   : ITextRange_Type;
      pcpLim : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_End
         (Pointer (This),
          pcpLim));

   end Put_End;

   function Get_Font
     (This  : ITextRange_Type)
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
     (This  : ITextRange_Type;
      pFont : Pointer_To_ITextFont)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Font
         (Pointer (This),
          pFont));

   end Put_Font;

   function Get_Para
     (This  : ITextRange_Type)
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
     (This  : ITextRange_Type;
      pPara : Pointer_To_ITextPara)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Para
         (Pointer (This),
          pPara));

   end Put_Para;

   function Get_StoryLength
     (This : ITextRange_Type)
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
     (This   : ITextRange_Type)
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
     (This   : ITextRange_Type;
      bStart : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Collapse
         (Pointer (This),
          bStart));

   end Collapse;

   function Expand
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This     : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This : ITextRange_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uSelect
         (Pointer (This)));

   end uSelect;

   function StartOf
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This    : ITextRange_Type;
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
     (This    : ITextRange_Type;
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
     (This    : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This : ITextRange_Type;
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
     (This : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This      : ITextRange_Type)
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
     (This  : ITextRange_Type;
      uType : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ChangeCase
         (Pointer (This),
          uType));

   end ChangeCase;

   procedure GetPoint
     (This  : ITextRange_Type;
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
     (This   : ITextRange_Type;
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
     (This  : ITextRange_Type;
      Value : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ScrollIntoView
         (Pointer (This),
          Value));

   end ScrollIntoView;

   function GetEmbeddedObject
     (This : ITextRange_Type)
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

end TOM.ITextRange_Interface;
