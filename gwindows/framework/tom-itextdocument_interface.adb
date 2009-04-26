with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextDocument_Interface is

   procedure Initialize (This : in out ITextDocument_Type) is
   begin
      Set_IID (This, IID_ITextDocument);
   end Initialize;

   function Pointer (This : ITextDocument_Type)
     return Pointer_To_ITextDocument
   is
   begin
      return To_Pointer_To_ITextDocument (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextDocument_Type;
                     Pointer : in     Pointer_To_ITextDocument)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Name
     (This  : ITextDocument_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Name
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Name;

   function Get_Selection
     (This  : ITextDocument_Type)
     return Pointer_To_ITextSelection
   is
       RetVal : aliased Pointer_To_ITextSelection;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Selection
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Selection;

   function Get_StoryCount
     (This   : ITextDocument_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StoryCount
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StoryCount;

   function Get_StoryRanges
     (This      : ITextDocument_Type)
     return Pointer_To_ITextStoryRanges
   is
       RetVal : aliased Pointer_To_ITextStoryRanges;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StoryRanges
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StoryRanges;

   function Get_Saved
     (This   : ITextDocument_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Saved
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Saved;

   procedure Put_Saved
     (This   : ITextDocument_Type;
      pValue : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Saved
         (Pointer (This),
          pValue));

   end Put_Saved;

   function Get_DefaultTabStop
     (This   : ITextDocument_Type)
     return Interfaces.C.C_float
   is
       RetVal : aliased Interfaces.C.C_float;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DefaultTabStop
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DefaultTabStop;

   procedure Put_DefaultTabStop
     (This   : ITextDocument_Type;
      pValue : Interfaces.C.C_float)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_DefaultTabStop
         (Pointer (This),
          pValue));

   end Put_DefaultTabStop;

   procedure uNew
     (This : ITextDocument_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNew
         (Pointer (This)));

   end uNew;

   procedure Open
     (This     : ITextDocument_Type;
      pVar     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Flags    : Interfaces.C.long;
      CodePage : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Open
         (Pointer (This),
          pVar,
          Flags,
          CodePage));

   end Open;

   procedure Save
     (This     : ITextDocument_Type;
      pVar     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Flags    : Interfaces.C.long;
      CodePage : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Save
         (Pointer (This),
          pVar,
          Flags,
          CodePage));

   end Save;

   function Freeze
     (This   : ITextDocument_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Freeze
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Freeze;

   function Unfreeze
     (This   : ITextDocument_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Unfreeze
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Unfreeze;

   procedure BeginEditCollection
     (This : ITextDocument_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.BeginEditCollection
         (Pointer (This)));

   end BeginEditCollection;

   procedure EndEditCollection
     (This : ITextDocument_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EndEditCollection
         (Pointer (This)));

   end EndEditCollection;

   function Undo
     (This  : ITextDocument_Type;
      Count : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Undo
         (Pointer (This),
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end Undo;

   function Redo
     (This  : ITextDocument_Type;
      Count : Interfaces.C.long)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Redo
         (Pointer (This),
          Count,
          RetVal'Unchecked_Access));

      return RetVal;
   end Redo;

   function uRange
     (This    : ITextDocument_Type;
      cp1     : Interfaces.C.long;
      cp2     : Interfaces.C.long)
     return Pointer_To_ITextRange
   is
       RetVal : aliased Pointer_To_ITextRange;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uRange
         (Pointer (This),
          cp1,
          cp2,
          RetVal'Unchecked_Access));

      return RetVal;
   end uRange;

   function RangeFromPoint
     (This    : ITextDocument_Type;
      x       : Interfaces.C.long;
      y       : Interfaces.C.long)
     return Pointer_To_ITextRange
   is
       RetVal : aliased Pointer_To_ITextRange;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RangeFromPoint
         (Pointer (This),
          x,
          y,
          RetVal'Unchecked_Access));

      return RetVal;
   end RangeFromPoint;

end TOM.ITextDocument_Interface;
