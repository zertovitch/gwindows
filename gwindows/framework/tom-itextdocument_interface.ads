with GNATCOM.Dispinterface;

package TOM.ITextDocument_Interface is

   type ITextDocument_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextDocument_Type);

   function Pointer (This : ITextDocument_Type)
     return Pointer_To_ITextDocument;

   procedure Attach (This    : in out ITextDocument_Type;
                     Pointer : in     Pointer_To_ITextDocument);

   function Get_Name
     (This  : ITextDocument_Type)
     return GNATCOM.Types.BSTR;

   function Get_Selection
     (This  : ITextDocument_Type)
     return Pointer_To_ITextSelection;

   function Get_StoryCount
     (This   : ITextDocument_Type)
     return Interfaces.C.long;

   function Get_StoryRanges
     (This      : ITextDocument_Type)
     return Pointer_To_ITextStoryRanges;

   function Get_Saved
     (This   : ITextDocument_Type)
     return Interfaces.C.long;

   procedure Put_Saved
     (This   : ITextDocument_Type;
      pValue : Interfaces.C.long);

   function Get_DefaultTabStop
     (This   : ITextDocument_Type)
     return Interfaces.C.C_float;

   procedure Put_DefaultTabStop
     (This   : ITextDocument_Type;
      pValue : Interfaces.C.C_float);

   procedure uNew
     (This : ITextDocument_Type);

   procedure Open
     (This     : ITextDocument_Type;
      pVar     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Flags    : Interfaces.C.long;
      CodePage : Interfaces.C.long);

   procedure Save
     (This     : ITextDocument_Type;
      pVar     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Flags    : Interfaces.C.long;
      CodePage : Interfaces.C.long);

   function Freeze
     (This   : ITextDocument_Type)
     return Interfaces.C.long;

   function Unfreeze
     (This   : ITextDocument_Type)
     return Interfaces.C.long;

   procedure BeginEditCollection
     (This : ITextDocument_Type);

   procedure EndEditCollection
     (This : ITextDocument_Type);

   function Undo
     (This  : ITextDocument_Type;
      Count : Interfaces.C.long)
     return Interfaces.C.long;

   function Redo
     (This  : ITextDocument_Type;
      Count : Interfaces.C.long)
     return Interfaces.C.long;

   function uRange
     (This    : ITextDocument_Type;
      cp1     : Interfaces.C.long;
      cp2     : Interfaces.C.long)
     return Pointer_To_ITextRange;

   function RangeFromPoint
     (This    : ITextDocument_Type;
      x       : Interfaces.C.long;
      y       : Interfaces.C.long)
     return Pointer_To_ITextRange;

end TOM.ITextDocument_Interface;
