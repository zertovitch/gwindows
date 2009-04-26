with GNATCOM.Dispinterface;

package TOM.ITextDocument_Object is

   type ITextDocument_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Name
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Selection
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   function Get_StoryCount
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   function Get_StoryRanges
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Saved
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Saved
     (This : ITextDocument_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_DefaultTabStop
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_DefaultTabStop
     (This : ITextDocument_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure uNew
     (This : ITextDocument_Type);

   procedure Open
     (This     : ITextDocument_Type;
      pVar     : GNATCOM.Types.VARIANT;
      Flags    : GNATCOM.Types.VARIANT;
      CodePage : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   procedure Save
     (This     : ITextDocument_Type;
      pVar     : GNATCOM.Types.VARIANT;
      Flags    : GNATCOM.Types.VARIANT;
      CodePage : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   function Freeze
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   function Unfreeze
     (This : ITextDocument_Type)
     return GNATCOM.Types.VARIANT;

   procedure BeginEditCollection
     (This : ITextDocument_Type);

   procedure EndEditCollection
     (This : ITextDocument_Type);

   function Undo
     (This  : ITextDocument_Type;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Redo
     (This  : ITextDocument_Type;
      Count : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function uRange
     (This : ITextDocument_Type;
      cp1  : GNATCOM.Types.VARIANT;
      cp2  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function RangeFromPoint
     (This : ITextDocument_Type;
      x    : GNATCOM.Types.VARIANT;
      y    : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

end TOM.ITextDocument_Object;
