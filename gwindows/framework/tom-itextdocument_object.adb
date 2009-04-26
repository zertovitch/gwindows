package body TOM.ITextDocument_Object is

   function Get_Name
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_Name);
   end Get_Name;

   function Get_Selection
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_Selection);
   end Get_Selection;

   function Get_StoryCount
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_StoryCount);
   end Get_StoryCount;

   function Get_StoryRanges
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_StoryRanges);
   end Get_StoryRanges;

   function Get_Saved
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_Saved);
   end Get_Saved;

   procedure Put_Saved
     (This : ITextDocument_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextDocument_Put_Saved,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Saved;

   function Get_DefaultTabStop
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextDocument_Get_DefaultTabStop);
   end Get_DefaultTabStop;

   procedure Put_DefaultTabStop
     (This : ITextDocument_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         ITextDocument_Put_DefaultTabStop,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_DefaultTabStop;

   procedure uNew
     (This : ITextDocument_Type)
   is
   begin
      Invoke (This, ITextDocument_uNew);
   end uNew;

   procedure Open
     (This     : ITextDocument_Type;
      pVar     : GNATCOM.Types.VARIANT;
      Flags    : GNATCOM.Types.VARIANT;
      CodePage : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextDocument_Open,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => CodePage,
          2 => Flags,
          3 => pVar),
         Free);
   end Open;

   procedure Save
     (This     : ITextDocument_Type;
      pVar     : GNATCOM.Types.VARIANT;
      Flags    : GNATCOM.Types.VARIANT;
      CodePage : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         ITextDocument_Save,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => CodePage,
          2 => Flags,
          3 => pVar),
         Free);
   end Save;

   function Freeze
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextDocument_Freeze);
   end Freeze;

   function Unfreeze
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextDocument_Unfreeze);
   end Unfreeze;

   procedure BeginEditCollection
     (This : ITextDocument_Type)
   is
   begin
      Invoke (This, ITextDocument_BeginEditCollection);
   end BeginEditCollection;

   procedure EndEditCollection
     (This : ITextDocument_Type)
   is
   begin
      Invoke (This, ITextDocument_EndEditCollection);
   end EndEditCollection;

   function Undo
     (This  : ITextDocument_Type;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextDocument_Undo,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count),
         Free);
   end Undo;

   function Redo
     (This  : ITextDocument_Type;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextDocument_Redo,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Count),
         Free);
   end Redo;

   function uRange
     (This : ITextDocument_Type;
      cp1  : GNATCOM.Types.VARIANT;
      cp2  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextDocument_uRange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => cp2,
          2 => cp1),
         Free);
   end uRange;

   function RangeFromPoint
     (This : ITextDocument_Type;
      x    : GNATCOM.Types.VARIANT;
      y    : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextDocument_RangeFromPoint,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => y,
          2 => x),
         Free);
   end RangeFromPoint;

end TOM.ITextDocument_Object;
