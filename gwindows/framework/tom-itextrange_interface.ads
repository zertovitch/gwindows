with GNATCOM.Dispinterface;
with Win32_Types;

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
     return Win32_Types.Long;

   procedure Put_Char
     (This : ITextRange_Type;
      pch  : Win32_Types.Long);

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
     return Win32_Types.Long;

   procedure Put_Start
     (This     : ITextRange_Type;
      pcpFirst : Win32_Types.Long);

   function Get_End
     (This   : ITextRange_Type)
     return Win32_Types.Long;

   procedure Put_End
     (This   : ITextRange_Type;
      pcpLim : Win32_Types.Long);

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
     return Win32_Types.Long;

   function Get_StoryType
     (This   : ITextRange_Type)
     return Win32_Types.Long;

   procedure Collapse
     (This   : ITextRange_Type;
      bStart : Win32_Types.Long);

   function Expand
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long)
     return Win32_Types.Long;

   function GetIndex
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long)
     return Win32_Types.Long;

   procedure SetIndex
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Index  : Win32_Types.Long;
      Extend : Win32_Types.Long);

   procedure SetRange
     (This     : ITextRange_Type;
      cpActive : Win32_Types.Long;
      cpOther  : Win32_Types.Long);

   function InRange
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   function InStory
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   function IsEqual
     (This   : ITextRange_Type;
      pRange : Pointer_To_ITextRange)
     return Win32_Types.Long;

   procedure uSelect
     (This : ITextRange_Type);

   function StartOf
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function EndOf
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Extend : Win32_Types.Long)
     return Win32_Types.Long;

   function Move
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStart
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEnd
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStartWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEndWhile
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveStartUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function MoveEndUntil
     (This   : ITextRange_Type;
      Cset   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

   function FindText
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function FindTextStart
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function FindTextEnd
     (This    : ITextRange_Type;
      bstr    : GNATCOM.Types.BSTR;
      cch     : Win32_Types.Long;
      Flags   : Win32_Types.Long;
      Free    : Boolean := True)
     return Win32_Types.Long;

   function Delete
     (This   : ITextRange_Type;
      Unit   : Win32_Types.Long;
      Count  : Win32_Types.Long)
     return Win32_Types.Long;

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
      Format : Win32_Types.Long);

   function CanPaste
     (This   : ITextRange_Type;
      pVar   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Format : Win32_Types.Long)
     return Win32_Types.Long;

   function CanEdit
     (This      : ITextRange_Type)
     return Win32_Types.Long;

   procedure ChangeCase
     (This  : ITextRange_Type;
      uType : Win32_Types.Long);

   procedure GetPoint
     (This  : ITextRange_Type;
      uType : Win32_Types.Long;
      px    : GNATCOM.Types.Pointer_To_long;
      py    : GNATCOM.Types.Pointer_To_long);

   procedure SetPoint
     (This   : ITextRange_Type;
      x      : Win32_Types.Long;
      y      : Win32_Types.Long;
      uType  : Win32_Types.Long;
      Extend : Win32_Types.Long);

   procedure ScrollIntoView
     (This  : ITextRange_Type;
      Value : Win32_Types.Long);

   function GetEmbeddedObject
     (This : ITextRange_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

end TOM.ITextRange_Interface;
