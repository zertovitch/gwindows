------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . S C I N T I L L A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2023 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

--  Bindings to Scintilla Edit Control for Windows

with System;

with GWindows.Base,
     GWindows.Colors,
     GWindows.Types,
     GWindows.Windows;

package GWindows.Scintilla is

   -------------------------------------------------------------------------
   --  Scintilla_Type
   -------------------------------------------------------------------------

   type Scintilla_Type is new GWindows.Base.Base_Window_Type with private;
   type Scintilla_Access is access all Scintilla_Type;
   type Pointer_To_Scintilla_Class is access all Scintilla_Type'Class;

   -------------------------------------------------------------------------
   --  Scintilla_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Scintilla  : in out Scintilla_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Scintilla Edit Control

   -------------------------------------------------------------------------
   --  Basics
   -------------------------------------------------------------------------

   INVALID_POSITION : constant := -1;

   --  Types are as described in https://www.scintilla.org/ScintillaDoc.html
   --  as of 2021-11-20.
   --  Subject to change...

   type Int is range
     -(2 ** (Standard'Address_Size - 1)) ..
      (2 ** (Standard'Address_Size - 1)) - 1;
   for Int'Size use Standard'Address_Size;
   --  "Arguments are 32-bit or 64-bit signed integers depending
   --   on the platform. Equivalent to intptr_t."

   subtype Position is Int;

   type Cell_Array_Type is array (Natural range <>) of Integer;

   type Int_32 is range -(2 ** 31) .. (2 ** 31) - 1;
   for Int_32'Size use 32;

   subtype Sci_PositionCR is Int_32;
   --  "In a future release the type Sci_PositionCR will be redefined
   --   to be 64-bits when Scintilla is built for 64-bits on all platforms."

   type Text_Range_Type is
      record
         Min  : Sci_PositionCR;
         Max  : Sci_PositionCR;
         Text : System.Address;
      end record;
   --  Scintilla API name: Sci_TextRange

   type Format_Range_Type is new Text_Range_Type;

   type Key_Mod is new Int_32;

   type Find_Text_Type is
      record
         Min  : Sci_PositionCR;
         Max  : Sci_PositionCR;
         Text : System.Address;
         TMin : Sci_PositionCR;
         TMax : Sci_PositionCR;
      end record;
   --  Scintilla API name: Sci_TextToFind

   type Find_Text_Access is access all Find_Text_Type;

   type Pointer is new System.Address;

   -------------
   -- Methods --
   -------------

   procedure Add_Text
     (Control : in out Scintilla_Type;
      text    : in     GString);
   --  Add text to the document

   procedure Add_Styled_Text
     (Control : in out Scintilla_Type; c : Cell_Array_Type);
   --  Add array of cells to document

   procedure Insert_Text
     (Control : in out Scintilla_Type; pos : Position; text : GString);
   --  Insert string at a position

   procedure Clear_All (Control : in out Scintilla_Type);
   --  Delete all text in the document

   procedure Clear_Document_Style (Control : in out Scintilla_Type);
   --  Set all style bytes to 0, remove all folding information

   function Get_Length (Control : Scintilla_Type) return Position;
   --  Retrieve the number of characters in the document.

   function Get_Text_Length (Control : Scintilla_Type) return Position
      renames Get_Length;

   function Line_Length (Control : Scintilla_Type; line : Integer)
                       return Integer;
   --  Returns the length of a line, *including* any line end characters.

   function Lines_On_Screen (Control : Scintilla_Type) return Integer;
   --  Retrieves the number of lines completely visible.

   function Get_Char_At
     (Control : Scintilla_Type; pos : Position) return GCharacter;
   --  Returns the character at the position pos, or GCharacter'Val (0)
   --  if pos is negative or past the end of the document.

   function Get_Current_Line_Number (Control : Scintilla_Type) return Integer;
   --  Returns the current line number

   function Get_Current_Pos (Control : Scintilla_Type) return Position;
   --  Returns the position of the caret

   function Get_Anchor (Control : Scintilla_Type) return Position;
   --  Returns the position of the opposite end of the selection to the caret

   function Get_Style_At
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Returns the style byte at the position

   procedure Redo (Control : in out Scintilla_Type);
   --  Redoes the next action on the undo history

   procedure Set_Undo_Collection
     (Control : in out Scintilla_Type; Collect_Undo : Boolean);
   --  Choose between collecting actions into the undo
   --  history and discarding them.

   procedure Select_All (Control : in out Scintilla_Type);
   --  Select all the text in the document.

   procedure Set_Save_Point (Control : Scintilla_Type);
   --  Remember the current position in the undo history as the position
   --  at which the document was saved.

   function Get_Styled_Text
     (Control : Scintilla_Type; tr : Text_Range_Type) return Integer;
   --  Retrieve a buffer of cells. Returns the number of bytes in the
   --  buffer not including terminating nulls.

   function Can_Redo (Control : Scintilla_Type) return Boolean;
   --  Are there any redoable actions in the undo history.

   function Marker_Line_From_Handle
     (Control : Scintilla_Type; mhandle : Integer) return Integer;
   --  Retrieve the line number at which a particular marker is located

   procedure Marker_Delete_Handle
     (Control : in out Scintilla_Type; mhandle : Integer);
   --  Delete a marker.

   function Get_Undo_Collection (Control : Scintilla_Type) return Boolean;
   --  Is undo history being collected?

   SCWS_INVISIBLE          : constant := 0;
   SCWS_VISIBLEALWAYS      : constant := 1;
   SCWS_VISIBLEAFTERINDENT : constant := 2;

   function Get_View_WS (Control : Scintilla_Type) return Integer;
   --  Are white space characters currently visible?
   --  Returns one of SCWS_* constants.

   procedure Set_View_WS  (Control : in out Scintilla_Type; views : Integer);
   --  Make white space characters invisible, always visible or
   --  visible outside indentation.

   function Position_From_Point
     (Control : Scintilla_Type; x : Integer; y : Integer) return Position;
   --  Find the position from a point within the window.

   function Position_From_Point_Close
     (Control : Scintilla_Type; x : Integer; y : Integer) return Position;
   --  Find the position from a point within the window but return
   --  INVALID_POSITION if not close to text.

   procedure Go_To_Line (Control : in out Scintilla_Type; line : Integer);
   --  Set caret to start of a line and ensure it is visible.

   procedure Go_To_Pos (Control : in out Scintilla_Type; pos : Position);
   --  Set caret to a position and ensure it is visible.

   procedure Set_Anchor
     (Control : in out Scintilla_Type; posAnchor : Position);
   --  Set the selection anchor to a position. The anchor is the opposite
   --  end of the selection from the caret.

   procedure Get_Cur_Line
     (Control        : in     Scintilla_Type;
      text           :    out GString;
      Caret_Position :    out Integer);
   --  Retrieve the text of the line containing the caret.
   --  Returns the index of the caret on the line.

   function Get_End_Styled (Control : Scintilla_Type) return Position;
   --  Retrieve the position of the last correctly styled character.

   SC_EOL_CRLF : constant := 0;
   SC_EOL_CR   : constant := 1;
   SC_EOL_LF   : constant := 2;

   procedure Convert_EOLs (Control : in out Scintilla_Type; eolMode : Integer);
   --  Convert all line endings in the document to one mode.

   function Get_EOL_Mode (Control : Scintilla_Type) return Integer;
   --  Retrieve the current end of line mode - one of CRLF, CR, or LF.

   procedure Set_EOL_Mode (Control : in out Scintilla_Type; eolMode : Integer);
   --  Set the current end of line mode.

   procedure Start_Styling
     (Control : in out Scintilla_Type; pos : Position; mask : Integer);
   --  Set the current styling position to pos and the styling mask to
   --  mask.  The styling mask can be used to protect some bits in
   --  each styling byte from modification.

   procedure Set_Styling
     (Control : in out Scintilla_Type; length : Integer; style : Integer);
   --  Change style from current styling position for length
   --  characters to a style and move the current styling position to
   --  after this newly styled segment.

   function Get_Buffered_Draw (Control : Scintilla_Type) return Boolean;
   --  Is drawing done first into a buffer or direct to the screen.

   procedure Set_Buffered_Draw
     (Control : in out Scintilla_Type; buffered : Boolean);
   --  If drawing is buffered then each line of text is drawn into a
   --  bitmap buffer before drawing it to the screen to avoid flicker.

   procedure Set_Tab_Width
     (Control : in out Scintilla_Type; tabWidth : Integer);
   --  Change the visible size of a tab to be a multiple of the width
   --  of a space character.

   function Get_Tab_Width (Control : Scintilla_Type) return Integer;
   --  Retrieve the visible size of a tab.

   SC_CP_UTF8 : constant := 16#0000_FDE9#;

   procedure Set_Code_Page
     (Control : in out Scintilla_Type; codePage : Integer);
   --  Set the code page used to interpret the bytes of the document
   --  as characters.  The SC_CP_UTF8 value can be used to enter
   --  Unicode mode.

   procedure Set_Use_Palette
     (Control : in out Scintilla_Type; usePalette : Boolean);
   --  In palette mode, Scintilla uses the environments palette calls
   --  to display more colors. This may lead to ugly displays.

   MARKER_MAX                   : constant := 16#001F#;
   SC_MARK_CIRCLE               : constant := 0;
   SC_MARK_ROUNDRECT            : constant := 1;
   SC_MARK_ARROW                : constant := 2;
   SC_MARK_SMALLRECT            : constant := 3;
   SC_MARK_SHORTARROW           : constant := 4;
   SC_MARK_EMPTY                : constant := 5;
   SC_MARK_ARROWDOWN            : constant := 6;
   SC_MARK_MINUS                : constant := 7;
   SC_MARK_PLUS                 : constant := 8;
   SC_MARK_VLINE                : constant := 9;
   SC_MARK_LCORNER              : constant := 16#000A#;
   SC_MARK_TCORNER              : constant := 16#000B#;
   SC_MARK_BOXPLUS              : constant := 16#000C#;
   SC_MARK_BOXPLUSCONNECTED     : constant := 16#000D#;
   SC_MARK_BOXMINUS             : constant := 16#000E#;
   SC_MARK_BOXMINUSCONNECTED    : constant := 16#000F#;
   SC_MARK_LCORNERCURVE         : constant := 16#0010#;
   SC_MARK_TCORNERCURVE         : constant := 16#0011#;
   SC_MARK_CIRCLEPLUS           : constant := 16#0012#;
   SC_MARK_CIRCLEPLUSCONNECTED  : constant := 16#0013#;
   SC_MARK_CIRCLEMINUS          : constant := 16#0014#;
   SC_MARK_CIRCLEMINUSCONNECTED : constant := 16#0015#;
   SC_MARK_BACKGROUND           : constant := 16#0016#;
   SC_MARK_BOOKMARK             : constant := 16#001F#;
   SC_MARK_CHARACTER            : constant := 16#2710#;
   SC_MARKNUM_FOLDEREND         : constant := 16#0019#;
   SC_MARKNUM_FOLDEROPENMID     : constant := 16#001A#;
   SC_MARKNUM_FOLDERMIDTAIL     : constant := 16#001B#;
   SC_MARKNUM_FOLDERTAIL        : constant := 16#001C#;
   SC_MARKNUM_FOLDERSUB         : constant := 16#001D#;
   SC_MARKNUM_FOLDER            : constant := 16#001E#;
   SC_MARKNUM_FOLDEROPEN        : constant := 16#001F#;
   SC_MASK_FOLDERS              : constant := 16#FE00_0000#;

   procedure Marker_Define
     (Control : in out Scintilla_Type;
      markerNumber : in Integer;
      markerSymbol : in Integer);
   --  Set the symbol used for a particular marker number.

   procedure Marker_Set_Fore
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      fore         : in     GWindows.Colors.Color_Type);
   --  Set the foreground color used for a particular marker number.

   procedure Marker_Set_Back
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      back         : in     GWindows.Colors.Color_Type);
   --  Set the background color used for a particular marker number.

   function Marker_Add
     (Control : Scintilla_Type; line : Integer; markerNumber : Integer)
     return Integer;
   --  Add a marker to a line, returning an ID which can be used to
   --  find or delete the marker.

   procedure Marker_Delete
     (Control : in out Scintilla_Type; line : Integer; markerNumber : Integer);
   --  Delete a marker from a line

   procedure Marker_Delete_All
     (Control : in out Scintilla_Type; markerNumber : Integer);
   --  Delete all markers with a particular number from all lines

   function Marker_Get (Control : Scintilla_Type; line : Integer)
                      return Integer;
   --  Get a bit mask of all the markers set on a line.

   function Marker_Next
     (Control : Scintilla_Type; lineStart : Integer; markerMask : Integer)
     return Integer;
   --  Find the next line after lineStart that includes a marker in mask.

   function Marker_Previous
     (Control : Scintilla_Type; lineStart : Integer; markerMask : Integer)
     return Integer;
   --  Find the previous line before lineStart that includes a marker in mask.

   SC_MARGIN_SYMBOL : constant := 0;
   SC_MARGIN_NUMBER : constant := 1;

   procedure Set_Margin_Type_N
     (Control : in out Scintilla_Type; margin : Integer; marginType : Integer);
   --  Set a margin to be either numeric or symbolic.

   function Get_Margin_Type_N
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the type of a margin.

   procedure Set_Margin_Width_N
     (Control : in out Scintilla_Type; margin : Integer; pixelWidth : Integer);
   --  Set the width of a margin to a width expressed in pixels.

   function Get_Margin_Width_N
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the width of a margin in pixels.

   procedure Set_Margin_Mask_N
     (Control : in out Scintilla_Type; margin : Integer; mask : Integer);
   --  Set a mask that determines which markers are displayed in a margin.

   function Get_Margin_Mask_N
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the marker mask of a margin.

   procedure Set_Margin_Sensitive_N
     (Control : in out Scintilla_Type; margin : Integer; sensitive : Boolean);
   --  Make a margin sensitive or insensitive to mouse clicks.

   function Get_Margin_Sensitive_N
     (Control : Scintilla_Type; margin : Integer) return Boolean;
   --  Retrieve the mouse click sensitivity of a margin.

   STYLE_DEFAULT               : constant := 16#0020#;
   STYLE_LINENUMBER            : constant := 16#0021#;
   STYLE_BRACELIGHT            : constant := 16#0022#;
   STYLE_BRACEBAD              : constant := 16#0023#;
   STYLE_CONTROLCHAR           : constant := 16#0024#;
   STYLE_INDENTGUIDE           : constant := 16#0025#;
   STYLE_LASTPREDEFINED        : constant := 16#0027#;
   STYLE_MAX                   : constant := 16#007F#;
   SC_CHARSET_ANSI             : constant := 0;
   SC_CHARSET_DEFAULT          : constant := 1;
   SC_CHARSET_BALTIC           : constant := 16#00BA#;
   SC_CHARSET_CHINESEBIG5      : constant := 16#0088#;
   SC_CHARSET_EASTEUROPE       : constant := 16#00EE#;
   SC_CHARSET_GB2312           : constant := 16#0086#;
   SC_CHARSET_GREEK            : constant := 16#00A1#;
   SC_CHARSET_HANGUL           : constant := 16#0081#;
   SC_CHARSET_MAC              : constant := 16#004D#;
   SC_CHARSET_OEM              : constant := 16#00FF#;
   SC_CHARSET_RUSSIAN          : constant := 16#00CC#;
   SC_CHARSET_SHIFTJIS         : constant := 16#0080#;
   SC_CHARSET_SYMBOL           : constant := 2;
   SC_CHARSET_TURKISH          : constant := 16#00A2#;
   SC_CHARSET_JOHAB            : constant := 16#0082#;
   SC_CHARSET_HEBREW           : constant := 16#00B1#;
   SC_CHARSET_ARABIC           : constant := 16#00B2#;
   SC_CHARSET_VIETNAMESE       : constant := 16#00A3#;
   SC_CHARSET_THAI             : constant := 16#00DE#;

   procedure Style_Clear_All (Control : in out Scintilla_Type);
   --  Clear all the styles and make equivalent to the global default style.

   procedure Style_Set_Fore
     (Control : in out Scintilla_Type;
      style   : in     Integer;
      fore    : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of a style.

   procedure Style_Set_Back
     (Control : in out Scintilla_Type;
      style   : in     Integer;
      back    : in     GWindows.Colors.Color_Type);
   --  Set the background color of a style.

   procedure Style_Set_Bold
     (Control : in out Scintilla_Type; style : Integer; bold : Boolean);
   --  Set a style to be bold or not.

   procedure Style_Set_Italic
     (Control : in out Scintilla_Type; style : Integer; italic : Boolean);
   --  Set a style to be italic or not.

   procedure Style_Set_Size
     (Control : in out Scintilla_Type; style : Integer; sizePoints : Integer);
   --  Set the size of characters of a style.

   procedure Style_Set_Font
     (Control : in out Scintilla_Type; style : Integer; fontName : GString);
   --  Set the font of a style.

   procedure Style_Set_EOL_Filled
     (Control : in out Scintilla_Type; style : Integer; filled : Boolean);
   --  Set a style to have its end of line filled or not.

   procedure Style_Reset_Default
     (Control : in out Scintilla_Type);
   --  Reset the default style to its state at startup

   procedure Style_Set_Underline
     (Control : in out Scintilla_Type; style : Integer; underline : Boolean);
   --  Set a style to be underlined or not.

   SC_CASE_MIXED               : constant := 0;
   SC_CASE_UPPER               : constant := 1;
   SC_CASE_LOWER               : constant := 2;

   procedure Style_Set_Case
     (Control : in out Scintilla_Type; style : Integer; caseForce : Integer);
   --  Set a style to be mixed case, or to force upper or lower case.

   procedure Style_Set_Character_Set
     (Control      : in out Scintilla_Type;
      style        : in     Integer;
      characterSet : in     Integer);
   --  Set the character set of the font in a style.

   procedure Set_Sel_Fore
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      fore       : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of the selection and whether to use
   --  this setting.

   procedure Set_Sel_Back
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      back       : in     GWindows.Colors.Color_Type);
   --  Set the background color of the selection and whether to use
   --  this setting.

   procedure Set_Caret_Fore
     (Control : in out Scintilla_Type; fore : GWindows.Colors.Color_Type);
   --  Set the foreground color of the caret.

   procedure Assign_Cmd_Key
     (Control : in out Scintilla_Type; km : Key_Mod; msg : Integer);
   --  When key+modifier combination km is pressed, perform msg.

   procedure Clear_Cmd_Key (Control : in out Scintilla_Type; km : Key_Mod);
   --  When key+modifier combination km, do nothing.

   procedure Clear_All_Cmd_Keys (Control : in out Scintilla_Type);
   --  Drop all key mappings.

   procedure Set_Styling_Ex
     (Control : in out Scintilla_Type; length : Integer; styles : GString);
   --  Set the styles for a segment of the document.

   procedure Style_Set_Visible
     (Control : in out Scintilla_Type; style : Integer; visible : Boolean);
   --  Set a style to be visible or not.

   function Get_Caret_Period (Control : in out Scintilla_Type) return Integer;
   --  Get the time in milliseconds that the caret is on and off.

   procedure Set_Caret_Period
     (Control : in out Scintilla_Type; periodMilliseconds : Integer);
   --  Get the time in milliseconds that the caret is on and off.
   --  0 = steady on.

   procedure Set_Word_Chars
     (Control : in out Scintilla_Type; characters : GString);
   --  Set the set of characters making up words for when moving or selecting
   --  by word.

   procedure Begin_Undo_Action (Control : in out Scintilla_Type);
   --  Start a sequence of actions that is undone and redone as a unit.
   --  May be nested.

   procedure End_Undo_Action (Control : in out Scintilla_Type);
   --  End a sequence of actions that is undone and redone as a unit.

   -----------------------------------------------------------------
   --  Indicators  -  special markings other than text selection  --
   -----------------------------------------------------------------

   INDIC_MAX : constant := 7;
   --
   INDIC_PLAIN          : constant :=  0;  --  Single, straight underline.
   INDIC_SQUIGGLE       : constant :=  1;  --  Squiggly underline.
   INDIC_SQUIGGLELOW    : constant := 11;  --  Squiggle, only 2 vert. pixels.
   INDIC_TT             : constant :=  2;  --  Line of small T shapes.
   INDIC_DIAGONAL       : constant :=  3;  --  Diagonal hatching.
   INDIC_STRIKE         : constant :=  4;  --  Strike out.
   INDIC_HIDDEN         : constant :=  5;  --  No visual effect.
   INDIC_BOX            : constant :=  6;  --  Rectangle around the text.
   INDIC_ROUNDBOX       : constant :=  7;  --  Rectangle with rounded corners.
   INDIC_STRAIGHTBOX    : constant :=  8;
   INDIC_FULLBOX        : constant := 16;
   INDIC_DASH           : constant :=  9;  --  Dashed underline.
   INDIC_DOTS           : constant := 10;  --  Dotted underline.
   INDIC_DOTBOX         : constant := 12;  --  Dotted rectangle around text.
   INDIC_POINT          : constant := 18;  --  Triangle below start.
   INDIC_POINTCHARACTER : constant := 19;  --  Tri. below centre of 1st char.
   --  Deprecated after 3.4.2:
   INDIC0_MASK                 : constant := 16#0020#;
   INDIC1_MASK                 : constant := 16#0040#;
   INDIC2_MASK                 : constant := 16#0080#;
   INDICS_MASK                 : constant := 16#00E0#;

   procedure Indic_Set_Style
     (Control : in out Scintilla_Type; indic : Integer; style : Integer);
   --  Set an indicator to plain, squiggle or TT.

   function Indic_Get_Style
     (Control : in out Scintilla_Type; indic : Integer) return Integer;
   --  Retrieve the style of an indicator.

   procedure Indic_Set_Fore
     (Control : in out Scintilla_Type;
      indic   : in     Integer;
      fore    : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of an indicator.

   function Indic_Get_Fore
     (Control : Scintilla_Type; indic : Integer)
     return GWindows.Colors.Color_Type;
   --  Retrieve the foreground color of an indicator.

   procedure Indicator_Clear_Range
     (Control : in out Scintilla_Type;
      start   : Position;
      length  : Position);

   procedure Indicator_Fill_Range
     (Control : in out Scintilla_Type;
      start   : Position;
      length  : Position);

   procedure Set_Style_Bits
     (Control : in out Scintilla_Type; bits : Integer);
   --  Divide each styling byte into lexical class bits (default:5)
   --  and indicator bits (default:3). If a lexer requires more than
   --  32 lexical states, then this is used to expand the possible
   --  states.

   function Get_Style_Bits (Control : Scintilla_Type) return Integer;
   --  Retrieve number of bits in style bytes used to hold the lexical state.

   --  Lexical states for SCLEX_PYTHON
   SCE_P_DEFAULT                : constant := 0;
   SCE_P_COMMENTLINE            : constant := 1;
   SCE_P_NUMBER                 : constant := 2;
   SCE_P_STRING                 : constant := 3;
   SCE_P_CHARACTER              : constant := 4;
   SCE_P_WORD                   : constant := 5;
   SCE_P_TRIPLE                 : constant := 6;
   SCE_P_TRIPLEDOUBLE           : constant := 7;
   SCE_P_CLASSNAME              : constant := 8;
   SCE_P_DEFNAME                : constant := 9;
   SCE_P_OPERATOR               : constant := 10;
   SCE_P_IDENTIFIER             : constant := 11;
   SCE_P_COMMENTBLOCK           : constant := 12;
   SCE_P_STRINGEOL              : constant := 13;
   --  Lexical states for SCLEX_CPP
   SCE_C_DEFAULT                : constant := 0;
   SCE_C_COMMENT                : constant := 1;
   SCE_C_COMMENTLINE            : constant := 2;
   SCE_C_COMMENTDOC             : constant := 3;
   SCE_C_NUMBER                 : constant := 4;
   SCE_C_WORD                   : constant := 5;
   SCE_C_STRING                 : constant := 6;
   SCE_C_CHARACTER              : constant := 7;
   SCE_C_UUID                   : constant := 8;
   SCE_C_PREPROCESSOR           : constant := 9;
   SCE_C_OPERATOR               : constant := 10;
   SCE_C_IDENTIFIER             : constant := 11;
   SCE_C_STRINGEOL              : constant := 12;
   SCE_C_VERBATIM               : constant := 13;
   SCE_C_REGEX                  : constant := 14;
   SCE_C_COMMENTLINEDOC         : constant := 15;
   SCE_C_WORD2                  : constant := 16;
   SCE_C_COMMENTDOCKEYWORD      : constant := 17;
   SCE_C_COMMENTDOCKEYWORDERROR : constant := 18;
   --  Lexical states for SCLEX_HTML, SCLEX_XML
   SCE_H_DEFAULT                : constant := 0;
   SCE_H_TAG                    : constant := 1;
   SCE_H_TAGUNKNOWN             : constant := 2;
   SCE_H_ATTRIBUTE              : constant := 3;
   SCE_H_ATTRIBUTEUNKNOWN       : constant := 4;
   SCE_H_NUMBER                 : constant := 5;
   SCE_H_DOUBLESTRING           : constant := 6;
   SCE_H_SINGLESTRING           : constant := 7;
   SCE_H_OTHER                  : constant := 8;
   SCE_H_COMMENT                : constant := 9;
   SCE_H_ENTITY                 : constant := 10;
   --  XML and ASP
   SCE_H_TAGEND                 : constant := 11;
   SCE_H_XMLSTART               : constant := 12;
   SCE_H_XMLEND                 : constant := 13;
   SCE_H_SCRIPT                 : constant := 14;
   SCE_H_ASP                    : constant := 15;
   SCE_H_ASPAT                  : constant := 16;
   SCE_H_CDATA                  : constant := 17;
   SCE_H_QUESTION               : constant := 18;
   --  More HTML
   SCE_H_VALUE                  : constant := 19;
   --  X-Code
   SCE_H_XCCOMMENT              : constant := 20;
   --  SGML
   SCE_H_SGML_DEFAULT           : constant := 21;
   SCE_H_SGML_COMMAND           : constant := 22;
   SCE_H_SGML_1ST_PARAM         : constant := 23;
   SCE_H_SGML_DOUBLESTRING      : constant := 24;
   SCE_H_SGML_SIMPLESTRING      : constant := 25;
   SCE_H_SGML_ERROR             : constant := 26;
   SCE_H_SGML_SPECIAL           : constant := 27;
   SCE_H_SGML_ENTITY            : constant := 28;
   SCE_H_SGML_COMMENT           : constant := 29;
   SCE_H_SGML_1ST_PARAM_COMMENT : constant := 30;
   SCE_H_SGML_BLOCK_DEFAULT     : constant := 31;
   --  Embedded Javascript
   SCE_HJ_START        : constant := 40;
   SCE_HJ_DEFAULT      : constant := 41;
   SCE_HJ_COMMENT      : constant := 42;
   SCE_HJ_COMMENTLINE  : constant := 43;
   SCE_HJ_COMMENTDOC   : constant := 44;
   SCE_HJ_NUMBER       : constant := 45;
   SCE_HJ_WORD         : constant := 46;
   SCE_HJ_KEYWORD      : constant := 47;
   SCE_HJ_DOUBLESTRING : constant := 48;
   SCE_HJ_SINGLESTRING : constant := 49;
   SCE_HJ_SYMBOLS      : constant := 50;
   SCE_HJ_STRINGEOL    : constant := 51;
   SCE_HJ_REGEX        : constant := 52;
   --  ASP Javascript
   SCE_HJA_START : constant := 55;
   SCE_HJA_DEFAULT : constant := 56;
   SCE_HJA_COMMENT : constant := 57;
   SCE_HJA_COMMENTLINE : constant := 58;
   SCE_HJA_COMMENTDOC : constant := 59;
   SCE_HJA_NUMBER : constant := 60;
   SCE_HJA_WORD : constant := 61;
   SCE_HJA_KEYWORD : constant := 62;
   SCE_HJA_DOUBLESTRING : constant := 63;
   SCE_HJA_SINGLESTRING : constant := 64;
   SCE_HJA_SYMBOLS : constant := 65;
   SCE_HJA_STRINGEOL : constant := 66;
   SCE_HJA_REGEX : constant := 67;
   --  Embedded VBScript
   SCE_HB_START : constant := 70;
   SCE_HB_DEFAULT : constant := 71;
   SCE_HB_COMMENTLINE : constant := 72;
   SCE_HB_NUMBER : constant := 73;
   SCE_HB_WORD : constant := 74;
   SCE_HB_STRING : constant := 75;
   SCE_HB_IDENTIFIER : constant := 76;
   SCE_HB_STRINGEOL : constant := 77;
   --  ASP VBScript
   SCE_HBA_START : constant := 80;
   SCE_HBA_DEFAULT : constant := 81;
   SCE_HBA_COMMENTLINE : constant := 82;
   SCE_HBA_NUMBER : constant := 83;
   SCE_HBA_WORD : constant := 84;
   SCE_HBA_STRING : constant := 85;
   SCE_HBA_IDENTIFIER : constant := 86;
   SCE_HBA_STRINGEOL : constant := 87;
   --  Embedded Python
   SCE_HP_START : constant := 90;
   SCE_HP_DEFAULT : constant := 91;
   SCE_HP_COMMENTLINE : constant := 92;
   SCE_HP_NUMBER : constant := 93;
   SCE_HP_STRING : constant := 94;
   SCE_HP_CHARACTER : constant := 95;
   SCE_HP_WORD : constant := 96;
   SCE_HP_TRIPLE : constant := 97;
   SCE_HP_TRIPLEDOUBLE : constant := 98;
   SCE_HP_CLASSNAME : constant := 99;
   SCE_HP_DEFNAME : constant := 100;
   SCE_HP_OPERATOR : constant := 101;
   SCE_HP_IDENTIFIER : constant := 102;
   --  ASP Python
   SCE_HPA_START : constant := 105;
   SCE_HPA_DEFAULT : constant := 106;
   SCE_HPA_COMMENTLINE : constant := 107;
   SCE_HPA_NUMBER : constant := 108;
   SCE_HPA_STRING : constant := 109;
   SCE_HPA_CHARACTER : constant := 110;
   SCE_HPA_WORD : constant := 111;
   SCE_HPA_TRIPLE : constant := 112;
   SCE_HPA_TRIPLEDOUBLE : constant := 113;
   SCE_HPA_CLASSNAME : constant := 114;
   SCE_HPA_DEFNAME : constant := 115;
   SCE_HPA_OPERATOR : constant := 116;
   SCE_HPA_IDENTIFIER : constant := 117;
   --  PHP
   SCE_HPHP_DEFAULT : constant := 118;
   SCE_HPHP_HSTRING : constant := 119;
   SCE_HPHP_SIMPLESTRING : constant := 120;
   SCE_HPHP_WORD : constant := 121;
   SCE_HPHP_NUMBER : constant := 122;
   SCE_HPHP_VARIABLE : constant := 123;
   SCE_HPHP_COMMENT : constant := 124;
   SCE_HPHP_COMMENTLINE : constant := 125;
   SCE_HPHP_HSTRING_VARIABLE : constant := 126;
   SCE_HPHP_OPERATOR : constant := 127;
   --  Lexical states for SCLEX_PERL
   SCE_PL_DEFAULT : constant := 0;
   SCE_PL_ERROR : constant := 1;
   SCE_PL_COMMENTLINE : constant := 2;
   SCE_PL_POD : constant := 3;
   SCE_PL_NUMBER : constant := 4;
   SCE_PL_WORD : constant := 5;
   SCE_PL_STRING : constant := 6;
   SCE_PL_CHARACTER : constant := 7;
   SCE_PL_PUNCTUATION : constant := 8;
   SCE_PL_PREPROCESSOR : constant := 9;
   SCE_PL_OPERATOR : constant := 10;
   SCE_PL_IDENTIFIER : constant := 11;
   SCE_PL_SCALAR : constant := 12;
   SCE_PL_ARRAY : constant := 13;
   SCE_PL_HASH : constant := 14;
   SCE_PL_SYMBOLTABLE : constant := 15;
   SCE_PL_REGEX : constant := 17;
   SCE_PL_REGSUBST : constant := 18;
   SCE_PL_LONGQUOTE : constant := 19;
   SCE_PL_BACKTICKS : constant := 20;
   SCE_PL_DATASECTION : constant := 21;
   SCE_PL_HERE_DELIM : constant := 22;
   SCE_PL_HERE_Q : constant := 23;
   SCE_PL_HERE_QQ : constant := 24;
   SCE_PL_HERE_QX : constant := 25;
   SCE_PL_STRING_Q : constant := 26;
   SCE_PL_STRING_QQ : constant := 27;
   SCE_PL_STRING_QX : constant := 28;
   SCE_PL_STRING_QR : constant := 29;
   SCE_PL_STRING_QW : constant := 30;
   --  Lexical states for SCLEX_VB, SCLEX_VBSCRIPT
   SCE_B_DEFAULT : constant := 0;
   SCE_B_COMMENT : constant := 1;
   SCE_B_NUMBER : constant := 2;
   SCE_B_KEYWORD : constant := 3;
   SCE_B_STRING : constant := 4;
   SCE_B_PREPROCESSOR : constant := 5;
   SCE_B_OPERATOR : constant := 6;
   SCE_B_IDENTIFIER : constant := 7;
   SCE_B_DATE : constant := 8;
   --  Lexical states for SCLEX_PROPERTIES
   SCE_PROPS_DEFAULT : constant := 0;
   SCE_PROPS_COMMENT : constant := 1;
   SCE_PROPS_SECTION : constant := 2;
   SCE_PROPS_ASSIGNMENT : constant := 3;
   SCE_PROPS_DEFVAL : constant := 4;
   --  Lexical states for SCLEX_LATEX
   SCE_L_DEFAULT : constant := 0;
   SCE_L_COMMAND : constant := 1;
   SCE_L_TAG : constant := 2;
   SCE_L_MATH : constant := 3;
   SCE_L_COMMENT : constant := 4;
   --  Lexical states for SCLEX_LUA
   SCE_LUA_DEFAULT : constant := 0;
   SCE_LUA_COMMENT : constant := 1;
   SCE_LUA_COMMENTLINE : constant := 2;
   SCE_LUA_COMMENTDOC : constant := 3;
   SCE_LUA_NUMBER : constant := 4;
   SCE_LUA_WORD : constant := 5;
   SCE_LUA_STRING : constant := 6;
   SCE_LUA_CHARACTER : constant := 7;
   SCE_LUA_LITERALSTRING : constant := 8;
   SCE_LUA_PREPROCESSOR : constant := 9;
   SCE_LUA_OPERATOR : constant := 10;
   SCE_LUA_IDENTIFIER : constant := 11;
   SCE_LUA_STRINGEOL : constant := 12;
   SCE_LUA_WORD2 : constant := 13;
   SCE_LUA_WORD3 : constant := 14;
   SCE_LUA_WORD4 : constant := 15;
   SCE_LUA_WORD5 : constant := 16;
   SCE_LUA_WORD6 : constant := 17;
   --  Lexical states for SCLEX_ERRORLIST
   SCE_ERR_DEFAULT : constant := 0;
   SCE_ERR_PYTHON : constant := 1;
   SCE_ERR_GCC : constant := 2;
   SCE_ERR_MS : constant := 3;
   SCE_ERR_CMD : constant := 4;
   SCE_ERR_BORLAND : constant := 5;
   SCE_ERR_PERL : constant := 6;
   SCE_ERR_NET : constant := 7;
   SCE_ERR_LUA : constant := 8;
   SCE_ERR_CTAG : constant := 9;
   SCE_ERR_DIFF_CHANGED : constant := 10;
   SCE_ERR_DIFF_ADDITION : constant := 11;
   SCE_ERR_DIFF_DELETION : constant := 12;
   SCE_ERR_DIFF_MESSAGE : constant := 13;
   --  Lexical states for SCLEX_BATCH
   SCE_BAT_DEFAULT : constant := 0;
   SCE_BAT_COMMENT : constant := 1;
   SCE_BAT_WORD : constant := 2;
   SCE_BAT_LABEL : constant := 3;
   SCE_BAT_HIDE : constant := 4;
   SCE_BAT_COMMAND : constant := 5;
   SCE_BAT_IDENTIFIER : constant := 6;
   SCE_BAT_OPERATOR : constant := 7;
   --  Lexical states for SCLEX_MAKEFILE
   SCE_MAKE_DEFAULT : constant := 0;
   SCE_MAKE_COMMENT : constant := 1;
   SCE_MAKE_PREPROCESSOR : constant := 2;
   SCE_MAKE_IDENTIFIER : constant := 3;
   SCE_MAKE_OPERATOR : constant := 4;
   SCE_MAKE_TARGET : constant := 5;
   SCE_MAKE_IDEOL : constant := 9;
   --  Lexical states for SCLEX_DIFF
   SCE_DIFF_DEFAULT : constant := 0;
   SCE_DIFF_COMMENT : constant := 1;
   SCE_DIFF_COMMAND : constant := 2;
   SCE_DIFF_HEADER : constant := 3;
   SCE_DIFF_POSITION : constant := 4;
   SCE_DIFF_DELETED : constant := 5;
   SCE_DIFF_ADDED : constant := 6;
   --  Lexical states for SCLEX_CONF (Apache Configuration Files Lexer)
   SCE_CONF_DEFAULT : constant := 0;
   SCE_CONF_COMMENT : constant := 1;
   SCE_CONF_NUMBER : constant := 2;
   SCE_CONF_IDENTIFIER : constant := 3;
   SCE_CONF_EXTENSION : constant := 4;
   SCE_CONF_PARAMETER : constant := 5;
   SCE_CONF_STRING : constant := 6;
   SCE_CONF_OPERATOR : constant := 7;
   SCE_CONF_IP : constant := 8;
   SCE_CONF_DIRECTIVE : constant := 9;
   --  Lexical states for SCLEX_AVE, Avenue
   SCE_AVE_DEFAULT : constant := 0;
   SCE_AVE_COMMENT : constant := 1;
   SCE_AVE_NUMBER : constant := 2;
   SCE_AVE_WORD : constant := 3;
   SCE_AVE_KEYWORD : constant := 4;
   SCE_AVE_STATEMENT : constant := 5;
   SCE_AVE_STRING : constant := 6;
   SCE_AVE_ENUM : constant := 7;
   SCE_AVE_STRINGEOL : constant := 8;
   SCE_AVE_IDENTIFIER : constant := 9;
   SCE_AVE_OPERATOR : constant := 10;
   --  Lexical states for SCLEX_ADA (see SciLexer.h)
   SCE_ADA_DEFAULT      : constant :=  0;
   SCE_ADA_WORD         : constant :=  1;
   SCE_ADA_IDENTIFIER   : constant :=  2;
   SCE_ADA_NUMBER       : constant :=  3;
   SCE_ADA_DELIMITER    : constant :=  4;
   SCE_ADA_CHARACTER    : constant :=  5;
   SCE_ADA_CHARACTEREOL : constant :=  6;
   SCE_ADA_STRING       : constant :=  7;
   SCE_ADA_STRINGEOL    : constant :=  8;
   SCE_ADA_LABEL        : constant :=  9;
   SCE_ADA_COMMENTLINE  : constant := 10;
   SCE_ADA_ILLEGAL      : constant := 11;
   --  Lexical states for SCLEX_BAAN
   SCE_BAAN_DEFAULT : constant := 0;
   SCE_BAAN_COMMENT : constant := 1;
   SCE_BAAN_COMMENTDOC : constant := 2;
   SCE_BAAN_NUMBER : constant := 3;
   SCE_BAAN_WORD : constant := 4;
   SCE_BAAN_STRING : constant := 5;
   SCE_BAAN_PREPROCESSOR : constant := 6;
   SCE_BAAN_OPERATOR : constant := 7;
   SCE_BAAN_IDENTIFIER : constant := 8;
   SCE_BAAN_STRINGEOL : constant := 9;
   SCE_BAAN_WORD2 : constant := 10;
   --  Lexical states for SCLEX_LISP
   SCE_LISP_DEFAULT : constant := 0;
   SCE_LISP_COMMENT : constant := 1;
   SCE_LISP_NUMBER : constant := 2;
   SCE_LISP_KEYWORD : constant := 3;
   SCE_LISP_STRING : constant := 6;
   SCE_LISP_STRINGEOL : constant := 8;
   SCE_LISP_IDENTIFIER : constant := 9;
   SCE_LISP_OPERATOR : constant := 10;
   --  Lexical states for SCLEX_EIFFEL and SCLEX_EIFFELKW
   SCE_EIFFEL_DEFAULT : constant := 0;
   SCE_EIFFEL_COMMENTLINE : constant := 1;
   SCE_EIFFEL_NUMBER : constant := 2;
   SCE_EIFFEL_WORD : constant := 3;
   SCE_EIFFEL_STRING : constant := 4;
   SCE_EIFFEL_CHARACTER : constant := 5;
   SCE_EIFFEL_OPERATOR : constant := 6;
   SCE_EIFFEL_IDENTIFIER : constant := 7;
   SCE_EIFFEL_STRINGEOL : constant := 8;
   --  Lexical states for SCLEX_NNCRONTAB (nnCron crontab Lexer)
   SCE_NNCRONTAB_DEFAULT : constant := 0;
   SCE_NNCRONTAB_COMMENT : constant := 1;
   SCE_NNCRONTAB_TASK : constant := 2;
   SCE_NNCRONTAB_SECTION : constant := 3;
   SCE_NNCRONTAB_KEYWORD : constant := 4;
   SCE_NNCRONTAB_MODIFIER : constant := 5;
   SCE_NNCRONTAB_ASTERISK : constant := 6;
   SCE_NNCRONTAB_NUMBER : constant := 7;
   SCE_NNCRONTAB_STRING : constant := 8;
   SCE_NNCRONTAB_ENVIRONMENT : constant := 9;
   SCE_NNCRONTAB_IDENTIFIER : constant := 10;
   --  Lexical states for SCLEX_MATLAB
   SCE_MATLAB_DEFAULT    : constant := 0;
   SCE_MATLAB_COMMENT    : constant := 1;
   SCE_MATLAB_COMMAND    : constant := 2;
   SCE_MATLAB_NUMBER     : constant := 3;
   SCE_MATLAB_KEYWORD    : constant := 4;
   SCE_MATLAB_STRING     : constant := 5;
   SCE_MATLAB_OPERATOR   : constant := 6;
   SCE_MATLAB_IDENTIFIER : constant := 7;

   procedure Set_Line_State
     (Control : in out Scintilla_Type; line : Integer; state : Integer);
   --  Used to hold extra styling information for each line.

   function Get_Line_State
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the extra styling information for a line.

   function Get_Max_Line_State (Control : Scintilla_Type) return Integer;
   --  Retrieve the last line number that has line state.

   function Get_Caret_Line_Visible (Control : Scintilla_Type) return Boolean;
   --  Is the background of the line containing the caret in a
   --  different color?

   procedure Set_Caret_Line_Visible
     (Control : in out Scintilla_Type; show : Boolean);
   --  Display the background of the line containing the caret in a
   --  different color.

   function Get_Caret_Line_Back
     (Control : Scintilla_Type) return GWindows.Colors.Color_Type;
   --  Get the color of the background of the line containing the caret.

   procedure Set_Caret_Line_Back
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   --  Set the color of the background of the line containing the caret.

   procedure Style_Set_Changeable
     (Control : in out Scintilla_Type; style : Integer; changeable : Boolean);
   --  Set a style to be changeable or not (read only).
   --  Experimental feature, currently buggy.

   -----------------------------
   --  Auto-completion lists  --
   -----------------------------

   procedure Auto_C_Show
     (Control    : in out Scintilla_Type;
      lenEntered : in     Integer;
      itemList   : in     GString);
   --  Display a auto-completion list.  The lenEntered parameter
   --  indicates how many characters before the caret should be used
   --  to provide context.

   procedure Auto_C_Cancel (Control : in out Scintilla_Type);
   --  Remove the auto-completion list from the screen.

   function Auto_C_Active (Control : Scintilla_Type) return Boolean;
   --  Is there an auto-completion list visible?

   function Auto_C_Pos_Start (Control : Scintilla_Type) return Position;
   --  Retrieve the Position of the caret when the auto-completion list was
   --  displayed.

   procedure Auto_C_Complete (Control : in out Scintilla_Type);
   --  User has selected an item so remove the list and insert the selection.

   procedure Auto_C_Stops
     (Control : in out Scintilla_Type; characterSet : GString);
   --  Define a set of character that when typed cancel the
   --  auto-completion list.

   procedure Auto_C_Set_Separator
     (Control : in out Scintilla_Type; separatorCharacter : Integer);
   --  Change the separator character in the GString setting up an
   --  auto-completion list. Default is space but can be changed if
   --  items contain space.

   function Auto_C_Get_Separator (Control : Scintilla_Type) return Integer;
   --  Retrieve the auto-completion list separator character.

   procedure Auto_C_Select (Control : in out Scintilla_Type; text : GString);
   --  Select the item in the auto-completion list that starts with a GString.

   procedure Auto_C_Set_Cancel_At_Start
     (Control : in out Scintilla_Type; cancel : Boolean);
   --  Should the auto-completion list be cancelled if the user
   --  backspaces to a Position before where the box was created.

   function Auto_C_Get_Cancel_At_Start
     (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether auto-completion cancelled by backspacing before start.

   procedure Auto_C_Set_Fill_Ups
     (Control : in out Scintilla_Type; characterSet : GString);
   --  Define a set of characters that when typed will cause the
   --  auto-completion to choose the selected item.

   procedure Auto_C_Set_Choose_Single
     (Control : in out Scintilla_Type; chooseSingle : Boolean);
   --  Should a single item auto-completion list automatically choose
   --  the item.

   function Auto_C_Get_Choose_Single (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether a single item auto-completion list
   --  automatically choose the item.

   procedure Auto_C_Set_Ignore_Case
     (Control : in out Scintilla_Type; ignoreCase : Boolean);
   --  Set whether case is significant when performing auto-completion
   --  searches.

   function Auto_C_Get_Ignore_Case (Control : Scintilla_Type) return Boolean;
   --  Retrieve state of ignore case flag.

   procedure User_List_Show
     (Control : in out Scintilla_Type; listType : Integer; itemList : GString);
   --  Display a list of GStrings and send notification when user chooses one.

   procedure Auto_C_Set_Auto_Hide
     (Control : in out Scintilla_Type; autoHide : Boolean);
   --  Set whether or not autocompletion is hidden automatically when
   --  nothing matches

   function Auto_C_Get_Auto_Hide (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether or not autocompletion is hidden automatically
   --  when nothing matches

   procedure Auto_C_Set_Drop_Rest_Of_Word
     (Control : in out Scintilla_Type; dropRestOfWord : Boolean);
   --  Set whether or not autocompletion deletes any word characters
   --  after the inserted text upon completion

   function Auto_C_Get_Drop_Rest_Of_Word
     (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether or not autocompletion deletes any word
   --  characters after the inserted text upon completion

   -------------------
   --  Indentation  --
   -------------------

   procedure Set_Indent
     (Control : in out Scintilla_Type; indentSize : Integer);
   --  Set the number of spaces used for one level of indentation.

   function Get_Indent (Control : Scintilla_Type) return Integer;
   --  Retrieve indentation size.

   procedure Set_Use_Tabs (Control : in out Scintilla_Type; useTabs : Boolean);
   --  Indentation will only use space characters if useTabs is false,
   --  otherwise it will use a combination of tabs and spaces.
   --  See the Tab and BackTab methods for programatically activating
   --  indentation and dedentation.

   function Get_Use_Tabs (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether tabs will be used in indentation.

   procedure Set_Line_Indentation
     (Control : in out Scintilla_Type; line : Integer; indentSize : Integer);
   --  Change the indentation of a line to a number of columns.

   function Get_Line_Indentation
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the number of columns that a line is indented.

   function Get_Line_Indent_Position
     (Control : Scintilla_Type; line : Integer) return Position;
   --  Retrieve the Position before the first non indentation
   --  character on a line.

   function Get_Column
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the column number of a Position, taking tab width into account.

   procedure Set_H_Scroll_Bar
     (Control : in out Scintilla_Type; show : Boolean);
   --  Show or hide the horizontal scroll bar.

   function Get_H_Scroll_Bar (Control : Scintilla_Type) return Boolean;
   --  Is the horizontal scroll bar visible?

   procedure Set_Indentation_Guides
     (Control : in out Scintilla_Type; show : Boolean);
   --  Show or hide indentation guides.

   function Get_Indentation_Guides (Control : Scintilla_Type) return Boolean;
   --  Are the indentation guides visible?

   procedure Set_Highlight_Guide
     (Control : in out Scintilla_Type; column : Integer);
   --  Set the highlighted indentation guide column.
   --  0 = no highlighted guide.

   function Get_Highlight_Guide (Control : Scintilla_Type) return Integer;
   --  Get the highlighted indentation guide column.

   function Get_Line_End_Position
     (Control : Scintilla_Type; line : Integer) return Position;
   --  Get the position at the end of the line, before any line end characters.

   function Get_Code_Page (Control : Scintilla_Type) return Integer;
   --  Get the code page used to interpret the bytes of the document
   --  as characters.

   function Get_Caret_Fore (Control : Scintilla_Type)
                           return GWindows.Colors.Color_Type;
   --  Get the foreground color of the caret.

   function Get_Use_Palette (Control : Scintilla_Type) return Boolean;
   --  In palette mode?

   function Get_Read_Only (Control : Scintilla_Type) return Boolean;
   --  In read-only mode?

   procedure Set_Current_Pos (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position of the caret.

   -------------------------------------------
   --  Text selection  -  single selection  --
   -------------------------------------------

   procedure Set_Selection_Start
     (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that starts the selection - this becomes the anchor.

   function Get_Selection_Start (Control : Scintilla_Type) return Position;
   --  Returns the Position at the start of the selection.

   procedure Set_Selection_End
     (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that ends the selection - this becomes the
   --  currentPosition.

   function Get_Selection_End (Control : Scintilla_Type) return Position;
   --  Returns the Position at the end of the selection.

   procedure Set_Sel
     (Control : in out Scintilla_Type; start, endp : Position);
   --  Select a range of text.

   procedure Get_Sel_Text
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer);
   --  Retrieve the selected text.
   --  Return the length of the text.

   procedure Hide_Selection
     (Control : in out Scintilla_Type; normal : Boolean);
   --  Draw the selection in normal style or with selection highlighted.

   ---------------------------------------------------------------------------
   --  Text selection  -  multiple selection (including rectangular         --
   --                     and multi-line editing)                           --
   ---------------------------------------------------------------------------

   procedure Set_Additional_Selection_Typing
     (Control : in out Scintilla_Type; additional_typing : Boolean := True);

   procedure Set_Multiple_Selection
     (Control : in out Scintilla_Type; multiple_selection : Boolean := True);

   procedure Set_Mouse_Selection_Rectangular
     (Control               : in out Scintilla_Type;
      rectangular_selection : in     Boolean := True);

   function Get_Selections (Control : Scintilla_Type) return Positive;
   --  There is always at least one selection - eventually empty: start = end

   function Get_Selection_N_Start
     (Control : Scintilla_Type;
      N       : Positive)
      return Position;

   function Get_Selection_N_End
     (Control : Scintilla_Type;
      N       : Positive)
      return Position;
   --  Get bounds of selection number N.

   function Get_Selection_N_Caret
     (Control : Scintilla_Type;
      N       : Positive) return Position;
   --  The caret is either at the end or at start, depending on how
   --  the selection was made (left to right, or right to left).

   procedure Set_Selection
     (Control       : in out Scintilla_Type;
      caret, anchor :        Position);
   --  Equivalent of Set_Sel for first selection - but with inverted positions!

   procedure Add_Selection
     (Control       : in out Scintilla_Type;
      caret, anchor :        Position);
   --  This is for supplemental selections after first one.

   procedure Selection_Duplicate (Control : in out Scintilla_Type);
   --  Duplicate selection(s).

   function Selection_Is_Rectangle (Control : Scintilla_Type) return Boolean;
   --  Is the selection a rectangular?
   --  The alternative is the more common stream selection.

   ---------------------------------------------------------------------------

   procedure Set_Print_Magnification
     (Control : in out Scintilla_Type; magnification : Integer);
   --  Sets the printer magnification added to the poInteger size of
   --  each style for printing.

   function Get_Print_Magnification (Control : Scintilla_Type) return Integer;
   --  Returns the printer magnification.

   SC_PRINT_NORMAL                 : constant := 0;
   SC_PRINT_INVERTLIGHT            : constant := 1;
   SC_PRINT_BLACKONWHITE           : constant := 2;
   SC_PRINT_COLORONWHITE           : constant := 3;
   SC_PRINT_COLORONWHITEDEFAULTBG  : constant := 4;

   procedure Set_Print_Color_Mode
     (Control : in out Scintilla_Type; mode : Integer);
   --  Modify colors when printing for clearer printed text.

   function Get_Print_Color_Mode (Control : Scintilla_Type) return Integer;
   --  Returns the printer color mode.

   SCFIND_WHOLEWORD               : constant := 2;
   SCFIND_MATCHCASE               : constant := 4;
   SCFIND_WORDSTART               : constant := 16#0010_0000#;
   SCFIND_REGEXP                  : constant := 16#0020_0000#;

   function Find_Text
     (Control : Scintilla_Type; flags : Integer; ft : Find_Text_Access)
     return Position;
   --  Find some text in the document.

   procedure Format_Range
     (Control : in out Scintilla_Type; draw : Boolean; fr : Text_Range_Type);
   --  On Windows will draw the document into a display context such
   --  as a printer.

   function Get_First_Visible_Line (Control : Scintilla_Type) return Integer;
   --  Retrieve the line at the top of the display.

   function Get_Line
     (Control : in     Scintilla_Type;
      line    : in     Integer)
     return GString;
   --  Retrieve the contents of a line.

   function Get_Line_Count (Control : Scintilla_Type) return Integer;
   --  Returns the number of lines in the document. There is always at
   --  least one.

   procedure Set_Margin_Left
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the size in pixels of the left margin.

   function Get_Margin_Left (Control : Scintilla_Type) return Integer;
   --  Returns the size in pixels of the left margin.

   procedure Set_Margin_Right
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the size in pixels of the right margin.

   function Get_Margin_Right (Control : Scintilla_Type) return Integer;
   --  Returns the size in pixels of the right margin.

   function Get_Modify (Control : Scintilla_Type) return Boolean;
   --  Is the document different from when it was last saved?

   function Get_Text_Range
     (Control : Scintilla_Type; tr : Text_Range_Type) return Integer;
   --  Retrieve a range of text (low level routine).
   --  Return the length of the text.

   function Get_Text_Range
     (Control : Scintilla_Type;
      Min     : Position;
      Max     : Position)
     return GString;
   --  Retrieve a range of text.

   function Point_X_From_Position
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the x value of the poInteger in the window where a
   --  Position is displayed.

   function Point_Y_From_Position
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the y value of the poInteger in the window where a
   --  Position is displayed.

   function Line_From_Position
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the line number containing a Position.

   function Position_From_Line
     (Control : Scintilla_Type; line : Integer) return Position;
   --  Retrieve the Position at the start of a line.

   procedure Line_Scroll
     (Control : in out Scintilla_Type; columns : Integer; lines : Integer);
   --  Scroll horizontally and vertically.

   procedure Scroll_Caret (Control : in out Scintilla_Type);
   --  Ensure the caret is visible.

   procedure Replace_Sel (Control : in out Scintilla_Type; text : GString);
   --  Replace the selected text with the argument text.

   procedure Set_Read_Only
     (Control : in out Scintilla_Type; readOnly : Boolean);
   --  Set to read only or read write.

   function Can_Paste (Control : Scintilla_Type) return Boolean;
   --  Will a paste succeed?

   function Can_Undo (Control : Scintilla_Type) return Boolean;
   --  Are there any undoable actions in the undo history.

   procedure Empty_Undo_Buffer (Control : in out Scintilla_Type);
   --  Delete the undo history.

   procedure Undo (Control : in out Scintilla_Type);
   --  Undo one action in the undo history.

   procedure Cut (Control : in out Scintilla_Type);
   --  Cut the selection to the clipboard.

   procedure Copy (Control : in out Scintilla_Type);
   --  Copy the selection to the clipboard.

   procedure Paste (Control : in out Scintilla_Type);
   --  Paste the contents of the clipboard into the document replacing
   --  the selection.

   procedure Clear (Control : in out Scintilla_Type);
   --  Clear the selection.

   procedure Set_Text (Control : in out Scintilla_Type; text : GString);
   --  Replace the contents of the document with the argument text.

   procedure Get_Text
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer);
   --  Retrieve all the text in the document.
   --  Returns number of characters retrieved.

   ---------------------------------
   --  Overtype vs. insert modes  --
   ---------------------------------

   procedure Set_Overtype
     (Control : in out Scintilla_Type; overtype : Boolean);
   --  Set to overtype (true) or insert mode.

   function Get_Overtype (Control : Scintilla_Type) return Boolean;
   --  Returns true if overtype mode is active otherwise false is returned.

   procedure Edit_Toggle_Overtype (Control : in out Scintilla_Type);
   --  Switch from insert to overtype mode or the reverse.

   procedure Set_Caret_Width
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Set the width of the insert mode caret.

   function Get_Caret_Width (Control : Scintilla_Type) return Integer;
   --  Returns the width of the insert mode caret

   procedure Set_Target_Start
     (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that starts the target which is used for updating the
   --  document without affecting the scroll Position.

   function Get_Target_Start (Control : Scintilla_Type) return Position;
   --  Get the Position that starts the target.

   procedure Set_Target_End (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that ends the target which is used for updating the
   --  document without affecting the scroll Position.

   function Get_Target_End (Control : Scintilla_Type) return Position;
   --  Get the Position that ends the target.

   function Replace_Target
     (Control : Scintilla_Type; text : GString)
     return Integer;
   --  Replace the target text with the argument text.
   --  Returns the length of the replacement text.

   function Replace_Target_RE
     (Control : Scintilla_Type; text : GString)
     return Integer;
   --  Replace the target text with the argument text after \d
   --  processing.  Text is counted so it can contain nulls.  Looks
   --  for \d where d is between 1 and 9 and replaces these with the
   --  GStrings matched in the last search operation which were
   --  surrounded by \( and \).  Returns the length of the replacement
   --  text including any change caused by processing the \d patterns.

   function Search_In_Target
     (Control : Scintilla_Type; text : GString)
     return Position;
   --  Search for a counted GString in the target and set the target
   --  to the found range. Text is counted so it can contain nulls.
   --  Returns length of range or -1 for failure in which case target
   --  is not moved.

   procedure Set_Search_Flags
     (Control : in out Scintilla_Type; flags : Integer);
   --  Set the search flags used by SearchInTarget

   function Get_Search_Flags (Control : Scintilla_Type) return Integer;
   --  Get the search flags used by SearchInTarget

   --------------------------------------------------
   --  Call tips: Rectangles with contextual help  --
   --------------------------------------------------

   procedure Call_Tip_Show
     (Control : in out Scintilla_Type; pos : Position; definition : GString);
   --  Show a call tip containing a definition near Position pos.

   procedure Call_Tip_Cancel (Control : in out Scintilla_Type);
   --  Remove the call tip from the screen.

   function Call_Tip_Active (Control : Scintilla_Type) return Boolean;
   --  Is there an active call tip?

   function Call_Tip_Pos_Start (Control : Scintilla_Type) return Position;
   --  Retrieve the Position where the caret was before displaying the
   --  call tip.

   procedure Call_Tip_Set_Hlt
     (Control : in out Scintilla_Type; start : Natural; endp : Natural);
   procedure Call_Tip_Set_Highlight
     (Control : in out Scintilla_Type; start : Natural; endp : Natural)
     renames Call_Tip_Set_Hlt;
   --  Highlight a segment of the definition.

   procedure Call_Tip_Set_Back
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   procedure Call_Tip_Set_Background_Color
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type)
     renames Call_Tip_Set_Back;
   --  Set the background color for the call tip.

   procedure Call_Tip_Set_Foreground_Color
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   --  Set the forground color for the call tip.

   procedure Call_Tip_Set_Foreground_Color_Highlighted
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   --  Set the forground color for highlighted parts of the call tip.

   ---------------------------------
   --  Doc line <-> display line  --
   ---------------------------------

   function Visible_From_Doc_Line
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Find the display line of a document line taking hidden lines
   --  into account.

   function Doc_Line_From_Visible
     (Control : Scintilla_Type; lineDisplay : Integer) return Integer;
   --  Find the document line of a display line taking hidden lines
   --  into account.

   ------------
   --  Fold  --
   ------------

   SC_FOLDLEVELBASE        : constant := 16#0400#;
   SC_FOLDLEVELWHITEFLAG   : constant := 16#1000#;
   SC_FOLDLEVELHEADERFLAG  : constant := 16#2000#;
   SC_FOLDLEVELNUMBERMASK  : constant := 16#0FFF#;

   procedure Set_Fold_Level
     (Control : in out Scintilla_Type; line : Integer; level : Integer);
   --  Set the fold level of a line.
   --  This encodes an integer level along with flags indicating whether the
   --  line is a header and whether it is effectively white space.

   function Get_Fold_Level
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the fold level of a line.

   function Get_Last_Child
     (Control : Scintilla_Type; line : Integer; level : Integer)
     return Integer;
   --  Find the last child line of a header line.

   function Get_Fold_Parent
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Find the parent line of a child line.

   procedure Show_Lines
     (Control : in out Scintilla_Type; lineStart : Integer; lineEnd : Integer);
   --  Make a range of lines visible.

   procedure Hide_Lines
     (Control : in out Scintilla_Type; lineStart : Integer; lineEnd : Integer);
   --  Make a range of lines invisible.

   function Get_Line_Visible
     (Control : Scintilla_Type; line : Integer) return Boolean;
   --  Is a line visible?

   procedure Set_Fold_Expanded
     (Control : in out Scintilla_Type; line : Integer; expanded : Boolean);
   --  Show the children of a header line.

   function Get_Fold_Expanded
     (Control : Scintilla_Type; line : Integer) return Boolean;
   --  Is a header line expanded?

   procedure Toggle_Fold (Control : in out Scintilla_Type; line : Integer);
   --  Switch a header line between expanded and contracted.

   procedure Ensure_Visible
     (Control : in out Scintilla_Type; line : Integer);
   --  Ensure a particular line is visible by expanding any header
   --  line hiding it.

   procedure Set_Fold_Flags (Control : in out Scintilla_Type; Flags : Integer);
   --  Set some debugging options for folding

   procedure Ensure_Visible_Enforce_Policy
     (Control : in out Scintilla_Type; line : Integer);
   --  Ensure a particular line is visible by expanding any header
   --  line hiding it.  Use the currently set visibility policy to
   --  determine which range to display.

   procedure Set_Tab_Indents
     (Control : in out Scintilla_Type; tabIndents : Boolean);
   --  Sets whether a tab pressed when caret is within indentation indents

   function Get_Tab_Indents (Control : Scintilla_Type) return Boolean;
   --  Does a tab pressed when caret is within indentation indent?

   procedure Set_Back_Space_Un_Indents
     (Control : in out Scintilla_Type; bsUnIndents : Boolean);
   --  Sets whether a backspace pressed when caret is within
   --  indentation unindents

   function Get_Back_Space_Un_Indents
     (Control : Scintilla_Type) return Boolean;
   --  Does a backspace pressed when caret is within indentation unindent?

   SC_TIME_FOREVER : constant := 16#0098_9680#;

   procedure Set_Mouse_Dwell_Time
     (Control : in out Scintilla_Type; periodMilliseconds : Integer);
   --  Sets the time the mouse must sit still to generate a mouse
   --  dwell event.  If set to SC_TIME_FOREVER, the default, no dwell
   --  events will be generated.

   function Get_Mouse_Dwell_Time (Control : Scintilla_Type) return Integer;
   --  Retrieve the time the mouse must sit still to generate a mouse
   --  dwell event.

   -------------
   --  Words  --
   -------------

   function Word_Start_Position
     (Control : Scintilla_Type; pos : Position; onlyWordCharacters : Boolean)
     return Position;
   --  Get Position of start of word.

   function Word_End_Position
     (Control : Scintilla_Type; pos : Position; onlyWordCharacters : Boolean)
     return Position;
   --  Get Position of end of word.

   function Get_Word_At
     (Control              : Scintilla_Type;
      pos                  : Position;
      only_word_characters : Boolean) return GString;
   --  Get the word at position pos.

   SC_WRAP_NONE : constant := 0;
   SC_WRAP_WORD : constant := 1;

   procedure Set_Wrap_Mode (Control : in out Scintilla_Type; mode : Integer);
   --  Sets whether text is word wrapped.

   function Get_Wrap_Mode (Control : Scintilla_Type) return Integer;
   --  Retrieve whether text is word wrapped.

   SC_CACHE_NONE                  : constant := 0;
   SC_CACHE_CARET                 : constant := 1;
   SC_CACHE_PAGE                  : constant := 2;
   SC_CACHE_DOCUMENT              : constant := 3;

   procedure Set_Layout_Cache
     (Control : in out Scintilla_Type; mode : Integer);
   --  Sets the degree of caching of layout information.

   function Get_Layout_Cache (Control : Scintilla_Type) return Integer;
   --  Retrieve the degree of caching of layout information.

   procedure Set_Scroll_Width
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the document width assumed for scrolling.

   function Get_Scroll_Width (Control : Scintilla_Type) return Integer;
   --  Retrieve the document width assumed for scrolling.

   function Text_Width
     (Control : Scintilla_Type; style : Integer; text : GString)
     return Integer;
   --  Measure the pixel width of some text in a particular style.
   --  Nul terminated text argument.
   --  Does not handle tab or control characters.

   procedure Set_End_At_Last_Line
     (Control : in out Scintilla_Type; endAtLastLine : Boolean);
   --  Sets the scroll range so that maximum scroll Position has
   --  the last line at the bottom of the view (default).
   --  Setting this to false allows scrolling one page below the last line.

   function Get_End_At_Last_Line (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether the maximum scroll Position has the last
   --  line at the bottom of the view.

   ------------------------------------------------------
   --  Caret movements, by line, character, word, ...  --
   ------------------------------------------------------

   procedure Line_Down (Control : in out Scintilla_Type);
   --  Start of key messages
   --  Move caret down one line.

   procedure Line_Down_Extend (Control : in out Scintilla_Type);
   --  Move caret down one line extending selection to new caret Position.

   procedure Line_Up (Control : in out Scintilla_Type);
   --  Move caret up one line.

   procedure Line_Up_Extend (Control : in out Scintilla_Type);
   --  Move caret up one line extending selection to new caret Position.

   procedure Char_Left (Control : in out Scintilla_Type);
   --  Move caret left one character.

   procedure Char_Left_Extend (Control : in out Scintilla_Type);
   --  Move caret left one character extending selection to new caret Position.

   procedure Char_Right (Control : in out Scintilla_Type);
   --  Move caret right one character.

   procedure Char_Right_Extend (Control : in out Scintilla_Type);
   --  Move caret right one character extending selection to new caret
   --  Position.

   procedure Word_Left (Control : in out Scintilla_Type);
   --  Move caret left one word.

   procedure Word_Left_Extend (Control : in out Scintilla_Type);
   --  Move caret left one word extending selection to new caret Position.

   procedure Word_Part_Left (Control : in out Scintilla_Type);
   --  Move to the previous change in capitalisation

   procedure Word_Part_Left_Extend (Control : in out Scintilla_Type);
   --  Move to the previous change in capitalisation extending
   --  selection to new caret Position.

   procedure Word_Part_Right (Control : in out Scintilla_Type);
   --  Move to the change next in capitalistion

   procedure Word_Part_Right_Extend (Control : in out Scintilla_Type);
   --  Move to the next change in capitalistion extending selection to
   --  new caret Position.

   procedure Word_Right (Control : in out Scintilla_Type);
   --  Move caret right one word.

   procedure Word_Right_Extend (Control : in out Scintilla_Type);
   --  Move caret right one word extending selection to new caret Position.

   procedure Home (Control : in out Scintilla_Type);
   --  Move caret to first Position on line.

   procedure Home_Extend (Control : in out Scintilla_Type);
   --  Move caret to first Position on line extending selection to new
   --  caret Position.

   procedure Line_End (Control : in out Scintilla_Type);
   --  Move caret to last Position on line.

   procedure Line_End_Extend (Control : in out Scintilla_Type);
   --  Move caret to last Position on line extending selection to new
   --  caret Position.

   procedure Document_Start (Control : in out Scintilla_Type);
   --  Move caret to first Position in document.

   procedure Document_Start_Extend (Control : in out Scintilla_Type);
   --  Move caret to first Position in document extending selection to
   --  new caret Position.

   procedure Document_End (Control : in out Scintilla_Type);
   --  Move caret to last Position in document.

   procedure Document_End_Extend (Control : in out Scintilla_Type);
   --  Move caret to last Position in document extending selection to
   --  new caret Position.

   procedure Page_Up (Control : in out Scintilla_Type);
   --  Move caret one page up.

   procedure Page_Up_Extend (Control : in out Scintilla_Type);
   --  Move caret one page up extending selection to new caret Position.

   procedure Page_Down (Control : in out Scintilla_Type);
   --  Move caret one page down.

   procedure Page_Down_Extend (Control : in out Scintilla_Type);
   --  Move caret one page down extending selection to new caret Position.

   procedure Cancel (Control : in out Scintilla_Type);
   --  Cancel any modes such as call tip or auto-completion list display.

   procedure Delete_Back (Control : in out Scintilla_Type);
   --  Delete the selection or if no selection, the character before the caret.

   procedure Delete_Back_Not_Line (Control : in out Scintilla_Type);
   --  Delete the selection or if no selection, the character before the caret.
   --  Will not delete the character before at the start of a line.

   procedure Tab (Control : in out Scintilla_Type);
   --  If selection is empty or all on one line replace the selection
   --  with a tab character or with the equivalent amount of spaces if
   --  the editor was set up with SetUseTabs (Control, False).
   --  If more than one line selected, indent the lines.

   procedure Back_Tab (Control : in out Scintilla_Type);
   --  Dedent the selected lines. Reverse of the Tab method.

   procedure New_Line (Control : in out Scintilla_Type);
   --  Insert a new line, may use a CRLF, CR or LF depending on EOL mode.

   procedure Form_Feed (Control : in out Scintilla_Type);
   --  Insert a Form Feed character.

   procedure VC_Home (Control : in out Scintilla_Type);
   --  Move caret to before first visible character on line.
   --  If already there move to first character on line.

   procedure VC_Home_Extend (Control : in out Scintilla_Type);
   --  Like VC_Home but extending selection to new caret Position.

   procedure Del_Word_Left (Control : in out Scintilla_Type);
   --  Delete the word to the left of the caret.

   procedure Del_Word_Right (Control : in out Scintilla_Type);
   --  Delete the word to the right of the caret.

   procedure Line_Cut (Control : in out Scintilla_Type);
   --  Cut the line containing the caret.

   procedure Line_Delete (Control : in out Scintilla_Type);
   --  Delete the line containing the caret.

   procedure Line_Duplicate (Control : in out Scintilla_Type);
   --  Duplicate the line containing the caret.

   procedure Line_Transpose (Control : in out Scintilla_Type);
   --  Switch the current line with the previous.

   procedure Lower_Case (Control : in out Scintilla_Type);
   --  Transform the selection to lower case.

   procedure Upper_Case (Control : in out Scintilla_Type);
   --  Transform the selection to upper case.

   procedure Line_Scroll_Down (Control : in out Scintilla_Type);
   --  Scroll the document down, keeping the caret visible.

   procedure Line_Scroll_Up (Control : in out Scintilla_Type);
   --  Scroll the document up, keeping the caret visible.

   procedure Move_Caret_Inside_View (Control : in out Scintilla_Type);
   --  Move the caret inside current view if it's not there already

   procedure Brace_Highlight
     (Control : in out Scintilla_Type; pos1 : Position; ppos2 : Position);
   --  Highlight the characters at two Positions.

   procedure Brace_Bad_Light (Control : in out Scintilla_Type; pos : Position);
   --  Highlight the character at a Position indicating there is no
   --  matching brace.

   function Brace_Match (Control : Scintilla_Type; pos : Position)
                       return Position;
   --  Find the Position of a matching brace or INVALID_POSITION if no match.

   function Get_View_EOL (Control : Scintilla_Type) return Boolean;
   --  Are the end of line characters visible.

   procedure Set_View_EOL (Control : in out Scintilla_Type; visible : Boolean);
   --  Make the end of line characters visible or invisible

   ---------------
   --  Zooming  --
   ---------------

   procedure Zoom_In (Control : in out Scintilla_Type);
   --  Magnify the displayed text by increasing the sizes by 1 point.

   procedure Zoom_Out (Control : in out Scintilla_Type);
   --  Make the displayed text smaller by decreasing the sizes by 1 point.

   procedure Set_Zoom (Control : in out Scintilla_Type; zoom : Integer);
   --  Set the zoom level. This number of points is added to the size
   --  of all fonts.  It may be positive to magnify or negative to
   --  reduce.

   function Get_Zoom (Control : Scintilla_Type) return Integer;
   --  Retrieve the zoom level.

   ----------------------
   --  Multiple views  --
   ----------------------

   function Get_Doc_Pointer (Control : Scintilla_Type) return Pointer;
   --  Retrieve a pointer to the document object.
   --  Used for multiple views of the same document.

   procedure Set_Doc_Pointer
     (Control : in out Scintilla_Type; doc_pointer : Pointer);
   --  Change the document object used.
   --  Used for multiple views of the same document.

   --  Notifications
   --  Type of modification and the action which caused the
   --  modification. These are defined as a bit mask to make it easy to
   --  specify which notifications are wanted.  One bit is set from
   --  each of SC_MOD_* and SC_PERFORMED_*.

   SC_MOD_INSERTTEXT              : constant := 16#00_0001#;
   SC_MOD_DELETETEXT              : constant := 16#00_0002#;
   SC_MOD_CHANGESTYLE             : constant := 16#00_0004#;
   SC_MOD_CHANGEFOLD              : constant := 16#00_0008#;
   SC_PERFORMED_USER              : constant := 16#00_0010#;
   SC_PERFORMED_UNDO              : constant := 16#00_0020#;
   SC_PERFORMED_REDO              : constant := 16#00_0040#;
   SC_LASTSTEPINUNDOREDO          : constant := 16#00_0100#;
   SC_MOD_CHANGEMARKER            : constant := 16#00_0200#;
   SC_MOD_BEFOREINSERT            : constant := 16#00_0400#;
   SC_MOD_BEFOREDELETE            : constant := 16#00_0800#;
   SC_MULTILINEUNDOREDO           : constant := 16#00_1000#;
   SC_STARTACTION                 : constant := 16#00_2000#;
   SC_MOD_CHANGEINDICATOR         : constant := 16#00_4000#;
   SC_MOD_CHANGELINESTATE         : constant := 16#00_8000#;
   SC_MOD_CHANGEMARGIN            : constant := 16#01_0000#;
   SC_MOD_CHANGEANNOTATION        : constant := 16#02_0000#;
   SC_MOD_CONTAINER               : constant := 16#04_0000#;
   SC_MOD_LEXERSTATE              : constant := 16#08_0000#;
   SC_MOD_INSERTCHECK             : constant := 16#10_0000#;
   SC_MOD_CHANGETABSTOPS          : constant := 16#20_0000#;
   SC_MODEVENTMASKALL             : constant := 16#3F_FFFF#;

   procedure Set_Mod_Event_Mask
     (Control : in out Scintilla_Type; Mask : Interfaces.Unsigned_32);
   --  Set which document modification events are sent to the container.

   function Get_Mod_Event_Mask (Control : Scintilla_Type)
      return Interfaces.Unsigned_32;
   --  Get which document modification events are sent to the container.

   generic
      with procedure Show_Line (S : String);
   procedure Show_Details (Mask : Interfaces.Unsigned_32);
   --  Display via Show_Line "Mod Insert Text" if the bit SC_MOD_INSERTTEXT
   --  is set, and so on.

   EDGE_NONE                      : constant := 0;
   EDGE_LINE                      : constant := 1;
   EDGE_BACKGROUND                : constant := 2;

   function Get_Edge_Column (Control : Scintilla_Type) return Integer;
   --  Retrieve the column number which text should be kept within.

   procedure Set_Edge_Column
     (Control : in out Scintilla_Type; column : Integer);
   --  Set the column number of the edge.
   --  If text goes past the edge then it is highlighted.

   function Get_Edge_Mode (Control : Scintilla_Type) return Integer;
   --  Retrieve the edge highlight mode.

   procedure Set_Edge_Mode (Control : in out Scintilla_Type; mode : Integer);
   --  The edge may be displayed by a line (EDGE_LINE) or by highlighting
   --  text that goes beyond it (EDGE_BACKGROUND) or not displayed at all
   --  (EDGE_NONE).

   function Get_Edge_Color (Control : Scintilla_Type)
                         return GWindows.Colors.Color_Type;
   --  Retrieve the color used in edge indication.

   procedure Set_Edge_Color
     (Control   : in out Scintilla_Type;
      edgeColor : in     GWindows.Colors.Color_Type);
   --  Change the color used in edge indication.

   procedure Search_Anchor (Control : in out Scintilla_Type);
   --  Sets the current caret Position to be the search anchor.

   function Search_Next
     (Control : Scintilla_Type; flags : Integer; text : GString)
     return Integer;
   --  Find some text starting at the search anchor.
   --  Does not ensure the selection is visible.

   function Search_Prev
     (Control : Scintilla_Type; flags : Integer; text : GString)
     return Integer;
   --  Find some text starting at the search anchor and moving backwards.
   --  Does not ensure the selection is visible.

   CARET_SLOP                     : constant := 16#0001#;
   --  Show caret within N lines of edge when it's scrolled to view
   --  If CARET_SLOP not set then centre caret on screen when it's
   --  scrolled to view

   CARET_CENTER                   : constant := 16#0002#;
   --  Value not used

   CARET_STRICT                   : constant := 16#0004#;
   --  If CARET_SLOP also set then rePosition whenever outside slop border
   --  If CARET_SLOP not set then recentre even when visible

   CARET_XEVEN                    : constant := 16#0008#;
   --  If CARET_XEVEN set then both left and right margins are given
   --  equal weight rather than favouring left following behaviour.

   CARET_XJUMPS                   : constant := 16#0010#;
   --  If CARET_XJUMPS set then when caret reaches the margin the
   --  display jumps enough to leave the caret solidly within the
   --  display.

   procedure Set_Caret_Policy
     (Control     : in out Scintilla_Type;
      caretPolicy : in     Integer;
      caretSlop   : in     Integer);
   --  Set the way the line the caret is on is kept visible.

   procedure Use_Pop_Up
     (Control : in out Scintilla_Type; Allow_Pop_Up : Boolean);
   --  Set whether a pop up menu is displayed automatically when the
   --  user presses the wrong mouse button.

   function Create_Document (Control : Scintilla_Type) return Integer;
   --  Create a new document object.
   --  Starts with reference count of 1 and not selected into editor.

   procedure Add_Ref_Document (Control : in out Scintilla_Type; doc : Integer);
   --  Extend life of document.

   procedure Release_Document (Control : in out Scintilla_Type; doc : Integer);
   --  Release a reference to the document, deleting document if it
   --  fades to black.

   procedure Set_Focus (Control : in out Scintilla_Type; focus : Boolean);
   --  Change internal focus flag

   function Get_Focus (Control : Scintilla_Type) return Boolean;
   --  Get internal focus flag

   procedure Set_Status
     (Control : in out Scintilla_Type; statusCode : Integer);
   --  Change error status - 0 = OK

   function Get_Status (Control : Scintilla_Type) return Integer;
   --  Get error status

   procedure Set_Mouse_Down_Captures
     (Control : in out Scintilla_Type; captures : Boolean);
   --  Set whether the mouse is captured when its button is pressed

   function Get_Mouse_Down_Captures (Control : Scintilla_Type) return Boolean;
   --  Get whether mouse gets captured

   SC_CURSORNORMAL                : constant := -1;
   SC_CURSORWAIT                  : constant := 3;

   procedure Set_Cursor
     (Control : in out Scintilla_Type; cursorType : Integer);
   --  Sets the cursor to one of the SC_CURSOR* values

   function Get_Cursor (Control : Scintilla_Type) return Integer;
   --  Get cursor type

   procedure Set_Control_Char_Symbol
     (Control : in out Scintilla_Type; symbol : Integer);
   --  Change the way control characters are displayed:
   --  If symbol is < 32, keep the drawn way, else, use the given character

   function Get_Control_Char_Symbol (Control : Scintilla_Type) return Integer;
   --  Get the way control characters are displayed

   VISIBLE_SLOP     : constant := 16#0001#;
   VISIBLE_STRICT   : constant := 16#0004#;

   procedure Set_Visible_Policy
     (Control       : in out Scintilla_Type;
      visiblePolicy : in     Integer;
      visibleSlop   : in     Integer);
   --  Set the way the display area is determined when a particular
   --  line is to be moved to.

   procedure Del_Line_Left (Control : in out Scintilla_Type);
   --  Delete back from the current Position to the start of the line

   procedure Del_Line_Right (Control : in out Scintilla_Type);
   --  Delete forwards from the current Position to the end of the line

   procedure Set_X_Offset
     (Control : in out Scintilla_Type; newOffset : Integer);
   --  Set the x Offset (ie, horizontal scroll Position)

   function Get_X_Offset (Control : Scintilla_Type) return Integer;
   --  Get the x Offset (ie, horizontal scroll Position)

   procedure Start_Record (Control : in out Scintilla_Type);
   --  Start notifying the container of all key presses and commands.

   procedure Stop_Record (Control : in out Scintilla_Type);
   --  Stop notifying the container of all key presses and commands.

   SCLEX_CONTAINER  : constant := 0;
   SCLEX_NULL       : constant := 1;
   SCLEX_PYTHON     : constant := 2;
   SCLEX_CPP        : constant := 3;
   SCLEX_HTML       : constant := 4;
   SCLEX_XML        : constant := 5;
   SCLEX_PERL       : constant := 6;
   SCLEX_SQL        : constant := 7;
   SCLEX_VB         : constant := 8;
   SCLEX_PROPERTIES : constant := 9;
   SCLEX_ERRORLIST  : constant := 10;
   SCLEX_MAKEFILE   : constant := 11;
   SCLEX_BATCH      : constant := 12;
   SCLEX_XCODE      : constant := 13;
   SCLEX_LATEX      : constant := 14;
   SCLEX_LUA        : constant := 15;
   SCLEX_DIFF       : constant := 16;
   SCLEX_CONF       : constant := 17;
   SCLEX_PASCAL     : constant := 18;
   SCLEX_AVE        : constant := 19;
   SCLEX_ADA        : constant := 20;
   SCLEX_LISP       : constant := 21;
   SCLEX_RUBY       : constant := 22;
   SCLEX_EIFFEL     : constant := 23;
   SCLEX_EIFFELKW   : constant := 24;
   SCLEX_TCL        : constant := 25;
   SCLEX_NNCRONTAB  : constant := 26;
   SCLEX_BULLANT    : constant := 27;
   SCLEX_VBSCRIPT   : constant := 28;
   SCLEX_ASP        : constant := 29;
   SCLEX_PHP        : constant := 30;
   SCLEX_BAAN       : constant := 31;
   SCLEX_MATLAB     : constant := 32;
   SCLEX_AUTOMATIC  : constant := 1000;

   procedure Set_Lexer (Control : in out Scintilla_Type; lexer : Integer);
   --  Set the lexing language of the document.

   function Get_Lexer (Control : Scintilla_Type) return Integer;
   --  Retrieve the lexing language of the document.

   procedure Colorise
     (Control : in out Scintilla_Type; start, endp : Position);
   --  Colorise a segment of the document using the current lexing language.

   procedure Set_Property
     (Control : in out Scintilla_Type; key : GString; value : GString);
   --  Set up a value that may be used by a lexer for some optional feature.

   procedure Set_Key_Words
     (Control    : in out Scintilla_Type;
      keywordSet : in     Integer;
      keyWords   : in     GString);
   --  Set up the key words used by the lexer.

   procedure Set_Lexer_Language
     (Control : in out Scintilla_Type; language : GString);
   --  Set the lexing language of the document based on GString name.

   --  Symbolic Key Codes
   SCK_DOWN                       : constant := 16#012C#;
   SCK_UP                         : constant := 16#012D#;
   SCK_LEFT                       : constant := 16#012E#;
   SCK_RIGHT                      : constant := 16#012F#;
   SCK_HOME                       : constant := 16#0130#;
   SCK_END                        : constant := 16#0131#;
   SCK_PRIOR                      : constant := 16#0132#;
   SCK_NEXT                       : constant := 16#0133#;
   SCK_DELETE                     : constant := 16#0134#;
   SCK_INSERT                     : constant := 16#0135#;
   SCK_ESCAPE                     : constant := 7;
   SCK_BACK                       : constant := 8;
   SCK_TAB                        : constant := 9;
   SCK_RETURN                     : constant := 16#000D#;
   SCK_ADD                        : constant := 16#0136#;
   SCK_SUBTRACT                   : constant := 16#0137#;
   SCK_DIVIDE                     : constant := 16#0138#;

   --  Key Modifiers
   SCMOD_SHIFT                    : constant := 1;
   SCMOD_CTRL                     : constant := 2;
   SCMOD_ALT                      : constant := 4;

   --  Virtual space is space beyond the end of each line

   SCVS_NONE                 : constant := 0;
   SCVS_RECTANGULARSELECTION : constant := 1;
   SCVS_USERACCESSIBLE       : constant := 2;
   SCVS_NOWRAPLINESTART      : constant := 4;

   procedure Set_Virtual_Space_Options
     (Control               : in out Scintilla_Type;
      virtual_space_options : in     Integer       := SCVS_NONE);

   -------------------------------------------------------------------------
   --  Scintilla_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   type Position_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Pos     : in     Position);

   type Modified_Event is access procedure
     (Control             : in out GWindows.Base.Base_Window_Type'Class;
      Pos                 : in     Position;
      Modification_Type   : in     Interfaces.Unsigned_32;
      Text                : in     GString;
      Lines_Added         : in     Integer;
      Line                : in     Integer;
      Fold_Level_Now      : in     Integer;
      Fold_Level_Previous : in     Integer);

   type Macro_Read_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Message : in     Integer;
                wParam  : in     Types.Wparam;
                lParam  : in     Types.Lparam);

   type Margin_Click_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Pos     : in     Position;
                Margin  : in     Integer);

   type Need_Shown_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Pos     : in     Position;
                Length  : in     Integer);

   type User_List_Selection_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                List_Type : in     Integer;
                Text      : in     GString);

   procedure On_Focus_Handler (Control : in out Scintilla_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Focus (Control : in out Scintilla_Type);

   procedure On_Lost_Focus_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (Control : in out Scintilla_Type);

   procedure On_Change_Handler (Control : in out Scintilla_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Change (Control : in out Scintilla_Type);

   procedure On_Character_Added_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Windows.Character_Event);
   procedure Fire_On_Character_Added
     (Control     : in out Scintilla_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter);

   procedure On_Update_UI_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Update_UI (Control : in out Scintilla_Type);

   procedure On_Save_Point_Reached_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Save_Point_Reached (Control : in out Scintilla_Type);

   procedure On_Save_Point_Left_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Save_Point_Left (Control : in out Scintilla_Type);

   procedure On_Attempt_To_Modify_Read_Only_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Attempt_To_Modify_Read_Only
     (Control : in out Scintilla_Type);

   procedure On_Painted_Handler (Control : in out Scintilla_Type;
                                 Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Painted (Control : in out Scintilla_Type);

   procedure On_Zoom_Handler (Control : in out Scintilla_Type;
                              Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Zoom (Control : in out Scintilla_Type);

   procedure On_Style_Needed_Handler (Control : in out Scintilla_Type;
                                      Handler : in     Position_Event);
   procedure Fire_On_Style_Needed (Control : in out Scintilla_Type;
                                   Pos     : in     Position);

   procedure On_Position_Changed_Handler (Control : in out Scintilla_Type;
                                          Handler : in     Position_Event);
   procedure Fire_On_Position_Changed
     (Control : in out Scintilla_Type;
      Pos     : in     Position);

   procedure On_Double_Click_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Double_Click (Control : in out Scintilla_Type);

   procedure On_Modified_Handler (Control : in out Scintilla_Type;
                                  Handler : in     Modified_Event);
   procedure Fire_On_Modified
     (Control             : in out Scintilla_Type;
      Pos                 : in     Position;
      Modification_Type   : in     Interfaces.Unsigned_32;
      Text                : in     GString;
      Lines_Added         : in     Integer;
      Line                : in     Integer;
      Fold_Level_Now      : in     Integer;
      Fold_Level_Previous : in     Integer);

   procedure On_Macro_Read_Handler
     (Control : in out Scintilla_Type;
      Handler : in     Macro_Read_Event);
   procedure Fire_On_Macro_Read (Control : in out Scintilla_Type;
                                 Message : in     Integer;
                                 wParam  : in     Types.Wparam;
                                 lParam  : in     Types.Lparam);

   procedure On_Margin_Click_Handler (Control : in out Scintilla_Type;
                                      Handler : in     Margin_Click_Event);
   procedure Fire_On_Margin_Click (Control : in out Scintilla_Type;
                                   Pos     : in     Position;
                                   Margin  : in     Integer);

   procedure On_Need_Shown_Handler
     (Control : in out Scintilla_Type;
      Handler : in     Need_Shown_Event);
   procedure Fire_On_Need_Shown (Control : in out Scintilla_Type;
                                 Pos     : in     Position;
                                 Length  : in     Integer);

   procedure On_User_List_Selection_Handler
     (Control : in out Scintilla_Type;
      Handler : in     User_List_Selection_Event);
   procedure Fire_On_User_List_Selection (Control : in out Scintilla_Type;
                                          List_Type : in     Integer;
                                          Text      : in     GString);

   procedure On_Dwell_Start_Handler (Control : in out Scintilla_Type;
                                     Handler : in     Position_Event);
   procedure Fire_On_Dwell_Start (Control : in out Scintilla_Type;
                                  Pos     : in     Position);

   procedure On_Dwell_End_Handler (Control : in out Scintilla_Type;
                                   Handler : in     Position_Event);
   procedure Fire_On_Dwell_End (Control : in out Scintilla_Type;
                                Pos     : in     Position);

   -------------------------------------------------------------------------
   --  Scintilla_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Focus (Control : in out Scintilla_Type);
   --  Received focus

   procedure On_Lost_Focus (Control : in out Scintilla_Type);
   --  Lost focus

   procedure On_Change (Control : in out Scintilla_Type);
   --  On_Change is fired when the text of the document has been
   --  changed for any reason

   procedure On_Character_Added
     (Control     : in out Scintilla_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter);
   --  Fired when the user types an ordinary text character (as
   --  opposed to a command character) which is entered into the
   --  text. Can be used to decide to display a call tip or auto
   --  completion list. Special_Key is always None.

   procedure On_Update_UI (Control : in out Scintilla_Type);
   --  Either the text or styling of the document has changed or the
   --  selection range has changed. Now would be a good time to update
   --  any UI elements that depend on document or view state. A common
   --  use is to check whether the caret is next to a brace and set
   --  highlights on this brace and its corresponding matching brace.

   procedure On_Save_Point_Reached (Control : in out Scintilla_Type);
   --  Sent when the savepoint is entered, allowing the container to
   --  display a dirty indicator and change its menus.

   procedure On_Save_Point_Left (Control : in out Scintilla_Type);
   --  Sent when the savepoint is left, allowing the container to
   --  display a dirty indicator and change its menus.

   procedure On_Attempt_To_Modify_Read_Only (Control : in out Scintilla_Type);
   --  When in read-only mode, this notification is sent should the
   --  user try to edit the document. This can be used to check the
   --  document out of a version control system.

   procedure On_Painted (Control : in out Scintilla_Type);
   --  Painting has just been done. Useful when you want to update
   --  some other widgets based on a change in Scintilla, but want to
   --  have the paint occur first to appear more responsive.

   procedure On_Zoom (Control : in out Scintilla_Type);
   --  On_Zoom is generated when the user zooms the display using the
   --  keyboard or the Set_Zoom method is called. This notification
   --  can be used to recalculate Positions, such as the width of the
   --  line number margin to maintain sizes in terms of characters
   --  rather than pixels.

   procedure On_Style_Needed (Control : in out Scintilla_Type;
                              Pos     : in     Position);
   --  Before displaying a page or printing, this message is sent to.
   --  It is a good opportunity for the container to ensure that
   --  syntax styling information for the visible text.

   procedure On_Position_Changed (Control : in out Scintilla_Type;
                                  Pos     : in     Position);
   --  Fired when the user moves the cursor to a different Position in
   --  the text. Can be used by to cancel some time consuming
   --  thread. (Note this is a deprecated notification)

   procedure On_Double_Click (Control   : in out Scintilla_Type);
   --  Mouse button was double clicked in editor.

   procedure On_Modified (Control             : in out Scintilla_Type;
                          Pos                 : in     Position;
                          Modification_Type   : in     Interfaces.Unsigned_32;
                          Text                : in     GString;
                          Lines_Added         : in     Integer;
                          Line                : in     Integer;
                          Fold_Level_Now      : in     Integer;
                          Fold_Level_Previous : in     Integer);
   --  SC_MOD_* used for Modification_Type. On_Modification is fired
   --  when the document has been changed including changes to both
   --  the text and styling. The contains information about what
   --  changed, how the change occurred and whether this changed the
   --  number of lines in the document. __NO__ modifications may be
   --  performed while handling this event.  It can be masked by the
   --  Set_Mod_Event_Mask function which sets which notification types
   --  are sent to the container. For example, a container may decide
   --  to see only notifications about changes to text and not styling
   --  changes by calling:

   --  Set_Mod_Event_Mask
   --     (Control, SC_MOD_INSERTTEXT or SC_MOD_DELETETEXT)

   procedure On_Macro_Read (Control : in out Scintilla_Type;
                            Message : in     Integer;
                            wParam  : in     Types.Wparam;
                            lParam  : in     Types.Lparam);
   --  Tell that an operation is being performed so that a choice can
   --  be made to record the fact if it is in a macro recording mode

   procedure On_Margin_Click (Control : in out Scintilla_Type;
                              Pos     : in     Position;
                              Margin  : in     Integer);
   --  Tells that the mouse was clicked inside a margin marked
   --  sensitive. Can be used to perform folding or to place
   --  breakpoints

   procedure On_Need_Shown (Control : in out Scintilla_Type;
                            Pos     : in     Position;
                            Length  : in     Integer);
   --  Scintilla has determined that a range of lines that is
   --  currently invisible should be made visible. An example of where
   --  this may be needed is if the end of line of a contracted fold
   --  point is deleted. This message in case there is a need to make the
   --  line visible in some unusual way such as making the whole
   --  document visible. Most containers will just ensure each line in
   --  the range is visible by calling EnsureVisible.

   procedure On_User_List_Selection
     (Control   : in out Scintilla_Type;
      List_Type : in     Integer;
      Text      : in     GString);
   --  User has selected an item in a user list.

   procedure On_Dwell_Start (Control : in out Scintilla_Type;
                             Pos     : in     Position);
   --  On_Dwell_Start is generated when the user hold the mouse still
   --  in one spot for the dwell period.
   --  If SetMouseDwellTime is set to SC_TIME_FOREVER, the default, no
   --  dwell events will be generated.

   procedure On_Dwell_End (Control : in out Scintilla_Type;
                           Pos     : in     Position);
   --  On_Dwell_End is generated after a SCN_DWELLSTART and the mouse
   --  is moved or other activity such as key press indicates the
   --  dwell is over.

   --  -----------------------------------------------------------------------
   --  Common_Control_Type - Event Framework Methods
   --  -----------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Notify
     (Window       : in out Scintilla_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult);
   --  Handles Notify Messages

   procedure On_Command (Window  : in out Scintilla_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Receives command messags from parent window

   function SCI_Lexer_DLL_Successfully_Loaded return Boolean;

   procedure Try_Loading_Lexer_DLL;
   --  Retry loading scilexer.dll if first attempt (automatically
   --  done on package initialization) was not successful.
   --  It lets time to copy or unpack the missing DLL.
   --
   --  Alternatively, the DLL could be loaded from memory using
   --  MemoryModule.c ( https://github.com/fancycode/MemoryModule ).
   --  Then, the DLL is never existing as a "physical" file;
   --  the executable is standalone and can be run from a read-only drive.
   --  See the project LEA ( http://l-e-a.sf.net ) for an example.

private
   type Scintilla_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Character_Added_Event : GWindows.Windows.Character_Event := null;

         On_Focus_Event                 : GWindows.Base.Action_Event := null;
         On_Lost_Focus_Event            : GWindows.Base.Action_Event := null;
         On_Change_Event                : GWindows.Base.Action_Event := null;
         On_Update_UI_Event             : GWindows.Base.Action_Event := null;
         On_Save_Point_Reached_Event    : GWindows.Base.Action_Event := null;
         On_Save_Point_Left_Event       : GWindows.Base.Action_Event := null;
         On_Attempt_To_Modify_Read_Only_Event
                                        : GWindows.Base.Action_Event := null;
         On_Painted_Event               : GWindows.Base.Action_Event := null;
         On_Zoom_Event                  : GWindows.Base.Action_Event := null;
         On_Double_Click_Event          : GWindows.Base.Action_Event := null;
         On_Style_Needed_Event          : Position_Event := null;
         On_Position_Changed_Event      : Position_Event := null;
         On_Modified_Event              : Modified_Event := null;
         On_Macro_Read_Event            : Macro_Read_Event := null;
         On_Margin_Click_Event          : Margin_Click_Event := null;
         On_Need_Shown_Event            : Need_Shown_Event := null;
         On_User_List_Selection_Event   : User_List_Selection_Event := null;
         On_Dwell_End_Event             : Position_Event := null;
         On_Dwell_Start_Event           : Position_Event := null;
      end record;

end GWindows.Scintilla;
