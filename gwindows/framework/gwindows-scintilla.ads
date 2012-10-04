------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . S C I N T I L L A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  Bindings to Scintilla Edit Control for Windows

with System;

with GWindows.Base;
with GWindows.Windows;
with GWindows.Colors;
with GWindows.Types;

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

   subtype Position is Integer;

   type Cell_Array_Type is array (Natural range <>) of Integer;

   type Text_Range_Type is
      record
         Min  : Integer;
         Max  : Integer;
         Text : System.Address;
      end record;

   type Format_Range_Type is new Text_Range_Type;

   type Key_Mod is new Integer;

   type Find_Text_Type is
      record
         Min  : Integer;
         Max  : Integer;
         Text : System.Address;
         TMin : Integer;
         TMax : Integer;
      end record;
   type Find_Text_Access is access all Find_Text_Type;

   procedure AddText
     (Control : in out Scintilla_Type;
      text    : in     GString);
   --  Add text to the document

   procedure AddStyledText
     (Control : in out Scintilla_Type; c : Cell_Array_Type);
   --  Add array of cells to document

   procedure InsertText
     (Control : in out Scintilla_Type; pos : Position; text : GString);
   --  Insert string at a position

   procedure ClearAll (Control : in out Scintilla_Type);
   --  Delete all text in the document

   procedure ClearDocumentStyle (Control : in out Scintilla_Type);
   --  Set all style bytes to 0, remove all folding information

   function GetLength (Control : Scintilla_Type) return Integer;
   --  The number of characters in the document

   function GetCharAt
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Returns the character byte at the position

   function GetCurrentPos (Control : Scintilla_Type) return Position;
   --  Returns the position of the caret

   function GetAnchor (Control : Scintilla_Type) return Position;
   --  Returns the position of the opposite end of the selection to the caret

   function GetStyleAt
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Returns the style byte at the position

   procedure Redo (Control : in out Scintilla_Type);
   --  Redoes the next action on the undo history

   procedure SetUndoCollection
     (Control : in out Scintilla_Type; collectUndo : Boolean);
   --  Choose between collecting actions into the undo
   --  history and discarding them.

   procedure SelectAll (Control : in out Scintilla_Type);
   --  Select all the text in the document.

   procedure SetSavePoint (Control : Scintilla_Type);
   --  Remember the current position in the undo history as the position
   --  at which the document was saved.

   function GetStyledText
     (Control : Scintilla_Type; tr : Text_Range_Type) return Integer;
   --  Retrieve a buffer of cells. Returns the number of bytes in the
   --  buffer not including terminating nulls.

   function CanRedo (Control : Scintilla_Type) return Boolean;
   --  Are there any redoable actions in the undo history.

   function MarkerLineFromHandle
     (Control : Scintilla_Type; mhandle : Integer) return Integer;
   --  Retrieve the line number at which a particular marker is located

   procedure MarkerDeleteHandle
     (Control : in out Scintilla_Type; mhandle : Integer);
   --  Delete a marker.

   function GetUndoCollection (Control : Scintilla_Type) return Boolean;
   --  Is undo history being collected?

   SCWS_INVISIBLE          : constant := 0;
   SCWS_VISIBLEALWAYS      : constant := 1;
   SCWS_VISIBLEAFTERINDENT : constant := 2;

   function GetViewWS (Control : Scintilla_Type) return Integer;
   --  Are white space characters currently visible?
   --  Returns one of SCWS_* constants.

   procedure SetViewWS  (Control : in out Scintilla_Type; views : Integer);
   --  Make white space characters invisible, always visible or
   --  visible outside indentation.

   function PositionFromPoint
     (Control : Scintilla_Type; x : Integer; y : Integer) return Integer;
   --  Find the position from a point within the window.

   function PositionFromPointClose
     (Control : Scintilla_Type; x : Integer; y : Integer) return Integer;
   --  Find the position from a point within the window but return
   --  INVALID_POSITION if not close to text.

   procedure GotoLine
     (Control : in out Scintilla_Type; line : Integer);
   --  Set caret to start of a line and ensure it is visible.

   procedure GotoPos (Control : in out Scintilla_Type; pos : Position);
   --  Set caret to a position and ensure it is visible.

   procedure SetAnchor (Control : in out Scintilla_Type; posAnchor : Position);
   --  Set the selection anchor to a position. The anchor is the opposite
   --  end of the selection from the caret.

   procedure GetCurLine
     (Control        : in     Scintilla_Type;
      text           :    out GString;
      Caret_Position :    out Integer);
   --  Retrieve the text of the line containing the caret.
   --  Returns the index of the caret on the line.

   function GetEndStyled (Control : Scintilla_Type) return Position;
   --  Retrieve the position of the last correctly styled character.

   SC_EOL_CRLF               : constant := 0;
   SC_EOL_CR                 : constant := 1;
   SC_EOL_LF                 : constant := 2;

   procedure ConvertEOLs (Control : in out Scintilla_Type; eolMode : Integer);
   --  Convert all line endings in the document to one mode.

   function GetEOLMode (Control : Scintilla_Type) return Integer;
   --  Retrieve the current end of line mode - one of CRLF, CR, or LF.

   procedure SetEOLMode (Control : in out Scintilla_Type; eolMode : Integer);
   --  Set the current end of line mode.

   procedure StartStyling
     (Control : in out Scintilla_Type; pos : Position; mask : Integer);
   --  Set the current styling position to pos and the styling mask to
   --  mask.  The styling mask can be used to protect some bits in
   --  each styling byte from modification.

   procedure SetStyling
     (Control : in out Scintilla_Type; length : Integer; style : Integer);
   --  Change style from current styling position for length
   --  characters to a style and move the current styling position to
   --  after this newly styled segment.

   function GetBufferedDraw (Control : Scintilla_Type) return Boolean;
   --  Is drawing done first into a buffer or direct to the screen.

   procedure SetBufferedDraw
     (Control : in out Scintilla_Type; buffered : Boolean);
   --  If drawing is buffered then each line of text is drawn into a
   --  bitmap buffer before drawing it to the screen to avoid flicker.

   procedure SetTabWidth (Control : in out Scintilla_Type; tabWidth : Integer);
   --  Change the visible size of a tab to be a multiple of the width
   --  of a space character.

   function GetTabWidth (Control : Scintilla_Type) return Integer;
   --  Retrieve the visible size of a tab.

   SC_CP_UTF8                : constant := 16#0000_FDE9#;

   procedure SetCodePage (Control : in out Scintilla_Type; codePage : Integer);
   --  Set the code page used to interpret the bytes of the document
   --  as characters.  The SC_CP_UTF8 value can be used to enter
   --  Unicode mode.

   procedure SetUsePalette
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
   SC_MARK_CHARACTER            : constant := 16#2710#;
   SC_MARKNUM_FOLDEREND         : constant := 16#0019#;
   SC_MARKNUM_FOLDEROPENMID     : constant := 16#001A#;
   SC_MARKNUM_FOLDERMIDTAIL     : constant := 16#001B#;
   SC_MARKNUM_FOLDERTAIL        : constant := 16#001C#;
   SC_MARKNUM_FOLDERSUB         : constant := 16#001D#;
   SC_MARKNUM_FOLDER            : constant := 16#001E#;
   SC_MARKNUM_FOLDEROPEN        : constant := 16#001F#;
   SC_MASK_FOLDERS              : constant := 16#FE00_0000#;

   procedure MarkerDefine
     (Control : in out Scintilla_Type;
      markerNumber : in Integer;
      markerSymbol : in Integer);
   --  Set the symbol used for a particular marker number.

   procedure MarkerSetFore
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      fore         : in     GWindows.Colors.Color_Type);
   --  Set the foreground color used for a particular marker number.

   procedure MarkerSetBack
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      back         : in     GWindows.Colors.Color_Type);
   --  Set the background color used for a particular marker number.

   function MarkerAdd
     (Control : Scintilla_Type; line : Integer; markerNumber : Integer)
     return Integer;
   --  Add a marker to a line, returning an ID which can be used to
   --  find or delete the marker.

   procedure MarkerDelete
     (Control : in out Scintilla_Type; line : Integer; markerNumber : Integer);
   --  Delete a marker from a line

   procedure MarkerDeleteAll
     (Control : in out Scintilla_Type; markerNumber : Integer);
   --  Delete all markers with a particular number from all lines

   function MarkerGet (Control : Scintilla_Type; line : Integer)
                      return Integer;
   --  Get a bit mask of all the markers set on a line.

   function MarkerNext
     (Control : Scintilla_Type; lineStart : Integer; markerMask : Integer)
     return Integer;
   --  Find the next line after lineStart that includes a marker in mask.

   function MarkerPrevious
     (Control : Scintilla_Type; lineStart : Integer; markerMask : Integer)
     return Integer;
   --  Find the previous line before lineStart that includes a marker in mask.

   SC_MARGIN_SYMBOL            : constant := 0;
   SC_MARGIN_NUMBER            : constant := 1;

   procedure SetMarginTypeN
     (Control : in out Scintilla_Type; margin : Integer; marginType : Integer);
   --  Set a margin to be either numeric or symbolic.

   function GetMarginTypeN
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the type of a margin.

   procedure SetMarginWidthN
     (Control : in out Scintilla_Type; margin : Integer; pixelWidth : Integer);
   --  Set the width of a margin to a width expressed in pixels.

   function GetMarginWidthN
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the width of a margin in pixels.

   procedure SetMarginMaskN
     (Control : in out Scintilla_Type; margin : Integer; mask : Integer);
   --  Set a mask that determines which markers are displayed in a margin.

   function GetMarginMaskN
     (Control : Scintilla_Type; margin : Integer) return Integer;
   --  Retrieve the marker mask of a margin.

   procedure SetMarginSensitiveN
     (Control : in out Scintilla_Type; margin : Integer; sensitive : Boolean);
   --  Make a margin sensitive or insensitive to mouse clicks.

   function GetMarginSensitiveN
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

   procedure StyleClearAll (Control : in out Scintilla_Type);
   --  Clear all the styles and make equivalent to the global default style.

   procedure StyleSetFore
     (Control : in out Scintilla_Type;
      style   : in     Integer;
      fore    : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of a style.

   procedure StyleSetBack
     (Control : in out Scintilla_Type;
      style   : in     Integer;
      back    : in     GWindows.Colors.Color_Type);
   --  Set the background color of a style.

   procedure StyleSetBold
     (Control : in out Scintilla_Type; style : Integer; bold : Boolean);
   --  Set a style to be bold or not.

   procedure StyleSetItalic
     (Control : in out Scintilla_Type; style : Integer; italic : Boolean);
   --  Set a style to be italic or not.

   procedure StyleSetSize
     (Control : in out Scintilla_Type; style : Integer; sizePoints : Integer);
   --  Set the size of characters of a style.

   procedure StyleSetFont
     (Control : in out Scintilla_Type; style : Integer; fontName : GString);
   --  Set the font of a style.

   procedure StyleSetEOLFilled
     (Control : in out Scintilla_Type; style : Integer; filled : Boolean);
   --  Set a style to have its end of line filled or not.

   procedure StyleResetDefault
     (Control : in out Scintilla_Type);
   --  Reset the default style to its state at startup

   procedure StyleSetUnderline
     (Control : in out Scintilla_Type; style : Integer; underline : Boolean);
   --  Set a style to be underlined or not.

   SC_CASE_MIXED               : constant := 0;
   SC_CASE_UPPER               : constant := 1;
   SC_CASE_LOWER               : constant := 2;

   procedure StyleSetCase
     (Control : in out Scintilla_Type; style : Integer; caseForce : Integer);
   --  Set a style to be mixed case, or to force upper or lower case.

   procedure StyleSetCharacterSet
     (Control      : in out Scintilla_Type;
      style        : in     Integer;
      characterSet : in     Integer);
   --  Set the character set of the font in a style.

   procedure SetSelFore
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      fore       : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of the selection and whether to use
   --  this setting.

   procedure SetSelBack
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      back       : in     GWindows.Colors.Color_Type);
   --  Set the background color of the selection and whether to use
   --  this setting.

   procedure SetCaretFore
     (Control : in out Scintilla_Type; fore : GWindows.Colors.Color_Type);
   --  Set the foreground color of the caret.

   procedure AssignCmdKey
     (Control : in out Scintilla_Type; km : Key_Mod; msg : Integer);
   --  When key+modifier combination km is pressed perform msg.

   procedure ClearCmdKey (Control : in out Scintilla_Type; km : Key_Mod);
   --  When key+modifier combination km do nothing.

   procedure ClearAllCmdKeys (Control : in out Scintilla_Type);
   --  Drop all key mappings.

   procedure SetStylingEx
     (Control : in out Scintilla_Type; length : Integer; styles : GString);
   --  Set the styles for a segment of the document.

   procedure StyleSetVisible
     (Control : in out Scintilla_Type; style : Integer; visible : Boolean);
   --  Set a style to be visible or not.

   procedure GetCaretPeriod (Control : in out Scintilla_Type);
   --  Get the time in milliseconds that the caret is on and off.

   procedure SetCaretPeriod
     (Control : in out Scintilla_Type; periodMilliseconds : Integer);
   --  Get the time in milliseconds that the caret is on and off. 0 =
   --  steady on.

   procedure SetWordChars
     (Control : in out Scintilla_Type; characters : GString);
   --  Set the set of characters making up words for when moving or selecting
   --  by word.

   procedure BeginUndoAction (Control : in out Scintilla_Type);
   --  Start a sequence of actions that is undone and redone as a unit.
   --  May be nested.

   procedure EndUndoAction (Control : in out Scintilla_Type);
   --  End a sequence of actions that is undone and redone as a unit.

   INDIC_MAX                   : constant := 7;
   INDIC_PLAIN                 : constant := 0;
   INDIC_SQUIGGLE              : constant := 1;
   INDIC_TT                    : constant := 2;
   INDIC_DIAGONAL              : constant := 3;
   INDIC_STRIKE                : constant := 4;
   INDIC0_MASK                 : constant := 16#0020#;
   INDIC1_MASK                 : constant := 16#0040#;
   INDIC2_MASK                 : constant := 16#0080#;
   INDICS_MASK                 : constant := 16#00E0#;

   procedure IndicSetStyle
     (Control : in out Scintilla_Type; indic : Integer; style : Integer);
   --  Set an indicator to plain, squiggle or TT.

   procedure IndicGetStyle
     (Control : in out Scintilla_Type; indic : Integer);
   --  Retrieve the style of an indicator.

   procedure IndicSetFore
     (Control : in out Scintilla_Type;
      indic   : in     Integer;
      fore    : in     GWindows.Colors.Color_Type);
   --  Set the foreground color of an indicator.

   function IndicGetFore
     (Control : Scintilla_Type; indic : Integer)
     return GWindows.Colors.Color_Type;
   --  Retrieve the foreground color of an indicator.

   procedure SetStyleBits
     (Control : in out Scintilla_Type; bits : Integer);
   --  Divide each styling byte into lexical class bits (default:5)
   --  and indicator bits (default:3). If a lexer requires more than
   --  32 lexical states, then this is used to expand the possible
   --  states.

   function GetStyleBits (Control : Scintilla_Type) return Integer;
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
   SCE_HJ_START : constant := 40;
   SCE_HJ_DEFAULT : constant := 41;
   SCE_HJ_COMMENT : constant := 42;
   SCE_HJ_COMMENTLINE : constant := 43;
   SCE_HJ_COMMENTDOC : constant := 44;
   SCE_HJ_NUMBER : constant := 45;
   SCE_HJ_WORD : constant := 46;
   SCE_HJ_KEYWORD : constant := 47;
   SCE_HJ_DOUBLESTRING : constant := 48;
   SCE_HJ_SINGLESTRING : constant := 49;
   SCE_HJ_SYMBOLS : constant := 50;
   SCE_HJ_STRINGEOL : constant := 51;
   SCE_HJ_REGEX : constant := 52;
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
   --  Lexical states for SCLEX_ADA
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
   SCE_MATLAB_DEFAULT : constant := 0;
   SCE_MATLAB_COMMENT : constant := 1;
   SCE_MATLAB_COMMAND : constant := 2;
   SCE_MATLAB_NUMBER : constant := 3;
   SCE_MATLAB_KEYWORD : constant := 4;
   SCE_MATLAB_STRING : constant := 5;
   SCE_MATLAB_OPERATOR : constant := 6;
   SCE_MATLAB_IDENTIFIER : constant := 7;

   procedure SetLineState
     (Control : in out Scintilla_Type; line : Integer; state : Integer);
   --  Used to hold extra styling information for each line.

   function GetLineState
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the extra styling information for a line.

   function GetMaxLineState (Control : Scintilla_Type) return Integer;
   --  Retrieve the last line number that has line state.

   function GetCaretLineVisible (Control : Scintilla_Type) return Boolean;
   --  Is the background of the line containing the caret in a
   --  different color?

   procedure SetCaretLineVisible
     (Control : in out Scintilla_Type; show : Boolean);
   --  Dsplay the background of the line containing the caret in a
   --  different color.

   function GetCaretLineBack
     (Control : Scintilla_Type) return GWindows.Colors.Color_Type;
   --  Get the color of the background of the line containing the caret.

   procedure SetCaretLineBack
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   --  Set the color of the background of the line containing the caret.

   procedure StyleSetChangeable
     (Control : in out Scintilla_Type; style : Integer; changeable : Boolean);
   --  Set a style to be changeable or not (read only).
   --  Experimental feature, currently buggy.

   procedure AutoCShow
     (Control    : in out Scintilla_Type;
      lenEntered : in     Integer;
      itemList   : in     GString);
   --  Display a auto-completion list.  The lenEntered parameter
   --  indicates how many characters before the caret should be used
   --  to provide context.

   procedure AutoCCancel (Control : in out Scintilla_Type);
   --  Remove the auto-completion list from the screen.

   function AutoCActive (Control : Scintilla_Type) return Boolean;
   --  Is there an auto-completion list visible?

   function AutoCPosStart (Control : Scintilla_Type) return Position;
   --  Retrieve the Position of the caret when the auto-completion list was
   --  displayed.

   procedure AutoCComplete (Control : in out Scintilla_Type);
   --  User has selected an item so remove the list and insert the selection.

   procedure AutoCStops
     (Control : in out Scintilla_Type; characterSet : GString);
   --  Define a set of character that when typed cancel the
   --  auto-completion list.

   procedure AutoCSetSeparator
     (Control : in out Scintilla_Type; separatorCharacter : Integer);
   --  Change the separator character in the GString setting up an
   --  auto-completion list. Default is space but can be changed if
   --  items contain space.

   function AutoCGetSeparator (Control : Scintilla_Type) return Integer;
   --  Retrieve the auto-completion list separator character.

   procedure AutoCSelect (Control : in out Scintilla_Type; text : GString);
   --  Select the item in the auto-completion list that starts with a GString.

   procedure AutoCSetCancelAtStart
     (Control : in out Scintilla_Type; cancel : Boolean);
   --  Should the auto-completion list be cancelled if the user
   --  backspaces to a Position before where the box was created.

   function AutoCGetCancelAtStart (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether auto-completion cancelled by backspacing before start.

   procedure AutoCSetFillUps
     (Control : in out Scintilla_Type; characterSet : GString);
   --  Define a set of characters that when typed will cause the
   --  autocompletion to choose the selected item.

   procedure AutoCSetChooseSingle
     (Control : in out Scintilla_Type; chooseSingle : Boolean);
   --  Should a single item auto-completion list automatically choose
   --  the item.

   function AutoCGetChooseSingle (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether a single item auto-completion list
   --  automatically choose the item.

   procedure AutoCSetIgnoreCase
     (Control : in out Scintilla_Type; ignoreCase : Boolean);
   --  Set whether case is significant when performing auto-completion
   --  searches.

   function AutoCGetIgnoreCase (Control : Scintilla_Type) return Boolean;
   --  Retrieve state of ignore case flag.

   procedure UserListShow
     (Control : in out Scintilla_Type; listType : Integer; itemList : GString);
   --  Display a list of GStrings and send notification when user chooses one.

   procedure AutoCSetAutoHide
     (Control : in out Scintilla_Type; autoHide : Boolean);
   --  Set whether or not autocompletion is hidden automatically when
   --  nothing matches

   function AutoCGetAutoHide (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether or not autocompletion is hidden automatically
   --  when nothing matches

   procedure AutoCSetDropRestOfWord
     (Control : in out Scintilla_Type; dropRestOfWord : Boolean);
   --  Set whether or not autocompletion deletes any word characters
   --  after the inserted text upon completion

   function AutoCGetDropRestOfWord (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether or not autocompletion deletes any word
   --  characters after the inserted text upon completion

   procedure SetIndent (Control : in out Scintilla_Type; indentSize : Integer);
   --  Set the number of spaces used for one level of indentation.

   function GetIndent (Control : Scintilla_Type) return Integer;
   --  Retrieve indentation size.

   procedure SetUseTabs (Control : in out Scintilla_Type; useTabs : Boolean);
   --  Indentation will only use space characters if useTabs is false,
   --  otherwise it will use a combination of tabs and spaces.

   function GetUseTabs (Control : Scintilla_Type) return Boolean;
   --  Retrieve whether tabs will be used in indentation.

   procedure SetLineIndentation
     (Control : in out Scintilla_Type; line : Integer; indentSize : Integer);
   --  Change the indentation of a line to a number of columns.

   function GetLineIndentation
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the number of columns that a line is indented.

   function GetLineIndentPosition
     (Control : Scintilla_Type; line : Integer) return Position;
   --  Retrieve the Position before the first non indentation
   --  character on a line.

   function GetColumn
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the column number of a Position, taking tab width into account.

   procedure SetHScrollBar (Control : in out Scintilla_Type; show : Boolean);
   --  Show or hide the horizontal scroll bar.

   function GetHScrollBar (Control : Scintilla_Type) return Boolean;
   --  Is the horizontal scroll bar visible?

   procedure SetIndentationGuides
     (Control : in out Scintilla_Type; show : Boolean);
   --  Show or hide indentation guides.

   function GetIndentationGuides (Control : Scintilla_Type) return Boolean;
   --  Are the indentation guides visible?

   procedure SetHighlightGuide
     (Control : in out Scintilla_Type; column : Integer);
   --  Set the highlighted indentation guide column.
   --  0 = no highlighted guide.

   function GetHighlightGuide (Control : Scintilla_Type) return Integer;
   --  Get the highlighted indentation guide column.

   function GetLineEndPosition
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Get the Position after the last visible characters on a line.

   function GetCodePage (Control : Scintilla_Type) return Integer;
   --  Get the code page used to interpret the bytes of the document
   --  as characters.

   function GetCaretFore (Control : Scintilla_Type)
                         return GWindows.Colors.Color_Type;
   --  Get the foreground color of the caret.

   function GetUsePalette (Control : Scintilla_Type) return Boolean;
   --  In palette mode?

   function GetReadOnly (Control : Scintilla_Type) return Boolean;
   --  In read-only mode?

   procedure SetCurrentPos (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position of the caret.

   procedure SetSelectionStart
     (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that starts the selection - this becomes the anchor.

   function GetSelectionStart (Control : Scintilla_Type) return Position;
   --  Returns the Position at the start of the selection.

   procedure SetSelectionEnd (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that ends the selection - this becomes the
   --  currentPosition.

   function GetSelectionEnd (Control : Scintilla_Type) return Position;
   --  Returns the Position at the end of the selection.

   procedure SetPrintMagnification
     (Control : in out Scintilla_Type; magnification : Integer);
   --  Sets the printer magnification added to the poInteger size of
   --  each style for printing.

   function GetPrintMagnification (Control : Scintilla_Type) return Integer;
   --  Returns the printer magnification.

   SC_PRINT_NORMAL                 : constant := 0;
   SC_PRINT_INVERTLIGHT            : constant := 1;
   SC_PRINT_BLACKONWHITE           : constant := 2;
   SC_PRINT_COLORONWHITE           : constant := 3;
   SC_PRINT_COLORONWHITEDEFAULTBG  : constant := 4;

   procedure SetPrintColorMode
     (Control : in out Scintilla_Type; mode : Integer);
   --  Modify colors when printing for clearer printed text.

   function GetPrintColorMode (Control : Scintilla_Type) return Integer;
   --  Returns the printer color mode.

   SCFIND_WHOLEWORD               : constant := 2;
   SCFIND_MATCHCASE               : constant := 4;
   SCFIND_WORDSTART               : constant := 16#0010_0000#;
   SCFIND_REGEXP                  : constant := 16#0020_0000#;

   function FindText
     (Control : Scintilla_Type; flags : Integer; ft : Find_Text_Access)
     return Position;
   --  Find some text in the document.

   procedure FormatRange
     (Control : in out Scintilla_Type; draw : Boolean; fr : Text_Range_Type);
   --  On Windows will draw the document into a display context such
   --  as a printer.

   function GetFirstVisibleLine (Control : Scintilla_Type) return Integer;
   --  Retrieve the line at the top of the display.

   function GetLine
     (Control : in     Scintilla_Type;
      line    : in     Integer)
     return GString;
   --  Retrieve the contents of a line.

   function GetLineCount (Control : Scintilla_Type) return Integer;
   --  Returns the number of lines in the document. There is always at
   --  least one.

   procedure SetMarginLeft
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the size in pixels of the left margin.

   function GetMarginLeft (Control : Scintilla_Type) return Integer;
   --  Returns the size in pixels of the left margin.

   procedure SetMarginRight
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the size in pixels of the right margin.

   function GetMarginRight (Control : Scintilla_Type) return Integer;
   --  Returns the size in pixels of the right margin.

   function GetModify (Control : Scintilla_Type) return Boolean;
   --  Is the document different from when it was last saved?

   procedure SetSel
     (Control : in out Scintilla_Type; start : Position; endp : Position);
   --  Select a range of text.

   procedure GetSelText
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer);
   --  Retrieve the selected text.
   --  Return the length of the text.

   function GetTextRange
     (Control : Scintilla_Type;
      Min     : Integer;
      Max     : Integer)
     return GString;

   function GetTextRange
     (Control : Scintilla_Type; tr : Text_Range_Type) return Integer;
   --  Retrieve a range of text.
   --  Return the length of the text.

   procedure HideSelection
     (Control : in out Scintilla_Type; normal : Boolean);
   --  Draw the selection in normal style or with selection highlighted.

   function PointXFromPosition
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the x value of the poInteger in the window where a
   --  Position is displayed.

   function PointYFromPosition
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the y value of the poInteger in the window where a
   --  Position is displayed.

   function LineFromPosition
     (Control : Scintilla_Type; pos : Position) return Integer;
   --  Retrieve the line containing a Position.

   function PositionFromLine
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the Position at the start of a line.

   procedure LineScroll
     (Control : in out Scintilla_Type; columns : Integer; lines : Integer);
   --  Scroll horizontally and vertically.

   procedure ScrollCaret (Control : in out Scintilla_Type);
   --  Ensure the caret is visible.

   procedure ReplaceSel (Control : in out Scintilla_Type; text : GString);
   --  Replace the selected text with the argument text.

   procedure SetReadOnly
     (Control : in out Scintilla_Type; readOnly : Boolean);
   --  Set to read only or read write.

   function CanPaste (Control : Scintilla_Type) return Boolean;
   --  Will a paste succeed?

   function CanUndo (Control : Scintilla_Type) return Boolean;
   --  Are there any undoable actions in the undo history.

   procedure EmptyUndoBuffer (Control : in out Scintilla_Type);
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

   procedure SetText (Control : in out Scintilla_Type; text : GString);
   --  Replace the contents of the document with the argument text.

   procedure GetText
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer);
   --  Retrieve all the text in the document.
   --  Returns number of characters retrieved.

   function GetTextLength (Control : Scintilla_Type) return Integer;
   --  Retrieve the number of characters in the document.

   procedure SetOvertype (Control : in out Scintilla_Type; overtype : Boolean);
   --  Set to overtype (true) or insert mode.

   function GetOvertype (Control : Scintilla_Type) return Boolean;
   --  Returns true if overtype mode is active otherwise false is returned.

   procedure SetCaretWidth
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Set the width of the insert mode caret.

   function GetCaretWidth (Control : Scintilla_Type) return Integer;
   --  Returns the width of the insert mode caret

   procedure SetTargetStart (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that starts the target which is used for updating the
   --  document without affecting the scroll Position.

   function GetTargetStart (Control : Scintilla_Type) return Position;
   --  Get the Position that starts the target.

   procedure SetTargetEnd (Control : in out Scintilla_Type; pos : Position);
   --  Sets the Position that ends the target which is used for updating the
   --  document without affecting the scroll Position.

   function GetTargetEnd (Control : Scintilla_Type) return Position;
   --  Get the Position that ends the target.

   function ReplaceTarget
     (Control : Scintilla_Type; text : GString)
     return Integer;
   --  Replace the target text with the argument text.
   --  Returns the length of the replacement text.

   function ReplaceTargetRE
     (Control : Scintilla_Type; text : GString)
     return Integer;
   --  Replace the target text with the argument text after \d
   --  processing.  Text is counted so it can contain nulls.  Looks
   --  for \d where d is between 1 and 9 and replaces these with the
   --  GStrings matched in the last search operation which were
   --  surrounded by \( and \).  Returns the length of the replacement
   --  text including any change caused by processing the \d patterns.

   function SearchInTarget
     (Control : Scintilla_Type; text : GString)
     return Integer;
   --  Search for a counted GString in the target and set the target
   --  to the found range. Text is counted so it can contain nulls.
   --  Returns length of range or -1 for failure in which case target
   --  is not moved.

   procedure SetSearchFlags (Control : in out Scintilla_Type; flags : Integer);
   --  Set the search flags used by SearchInTarget

   function GetSearchFlags (Control : Scintilla_Type) return Integer;
   --  Get the search flags used by SearchInTarget

   procedure CallTipShow
     (Control : in out Scintilla_Type; pos : Position; definition : GString);
   --  Show a call tip containing a definition near Position pos.

   procedure CallTipCancel (Control : in out Scintilla_Type);
   --  Remove the call tip from the screen.

   function CallTipActive (Control : Scintilla_Type) return Boolean;
   --  Is there an active call tip?

   function CallTipPosStart (Control : Scintilla_Type) return Position;
   --  Retrieve the Position where the caret was before displaying the
   --  call tip.

   procedure CallTipSetHlt
     (Control : in out Scintilla_Type; start : Integer; endp : Integer);
   --  Highlight a segment of the definition.

   procedure CallTipSetBack
     (Control : in out Scintilla_Type; back : GWindows.Colors.Color_Type);
   --  Set the background color for the call tip.

   function VisibleFromDocLine
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Find the display line of a document line taking hidden lines
   --  into account.

   function DocLineFromVisible
     (Control : Scintilla_Type; lineDisplay : Integer) return Integer;
   --  Find the document line of a display line taking hidden lines
   --  into account.

   SC_FOLDLEVELBASE               : constant := 16#0400#;
   SC_FOLDLEVELWHITEFLAG          : constant := 16#1000#;
   SC_FOLDLEVELHEADERFLAG         : constant := 16#2000#;
   SC_FOLDLEVELNUMBERMASK         : constant := 16#0FFF#;

   procedure SetFoldLevel
     (Control : in out Scintilla_Type; line : Integer; level : Integer);
   --  Set the fold level of a line.
   --  This encodes an integer level along with flags indicating whether the
   --  line is a header and whether it is effectively white space.

   function GetFoldLevel
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Retrieve the fold level of a line.

   function GetLastChild
     (Control : Scintilla_Type; line : Integer; level : Integer)
     return Integer;
   --  Find the last child line of a header line.

   function GetFoldParent
     (Control : Scintilla_Type; line : Integer) return Integer;
   --  Find the parent line of a child line.

   procedure ShowLines
     (Control : in out Scintilla_Type; lineStart : Integer; lineEnd : Integer);
   --  Make a range of lines visible.

   procedure HideLines
     (Control : in out Scintilla_Type; lineStart : Integer; lineEnd : Integer);
   --  Make a range of lines invisible.

   function GetLineVisible
     (Control : Scintilla_Type; line : Integer) return Boolean;
   --  Is a line visible?

   procedure SetFoldExpanded
     (Control : in out Scintilla_Type; line : Integer; expanded : Boolean);
   --  Show the children of a header line.

   function GetFoldExpanded
     (Control : Scintilla_Type; line : Integer) return Boolean;
   --  Is a header line expanded?

   procedure ToggleFold (Control : in out Scintilla_Type; line : Integer);
   --  Switch a header line between expanded and contracted.

   procedure EnsureVisible
     (Control : in out Scintilla_Type; line : Integer);
   --  Ensure a particular line is visible by expanding any header
   --  line hiding it.

   procedure SetFoldFlags (Control : in out Scintilla_Type; Flags : Integer);
   --  Set some debugging options for folding

   procedure EnsureVisibleEnforcePolicy
     (Control : in out Scintilla_Type; line : Integer);
   --  Ensure a particular line is visible by expanding any header
   --  line hiding it.  Use the currently set visibility policy to
   --  determine which range to display.

   procedure SetTabIndents
     (Control : in out Scintilla_Type; tabIndents : Boolean);
   --  Sets whether a tab pressed when caret is within indentation indents

   function GetTabIndents (Control : Scintilla_Type) return Boolean;
   --  Does a tab pressed when caret is within indentation indent?

   procedure SetBackSpaceUnIndents
     (Control : in out Scintilla_Type; bsUnIndents : Boolean);
   --  Sets whether a backspace pressed when caret is within
   --  indentation unindents

   function GetBackSpaceUnIndents (Control : Scintilla_Type) return Boolean;
   --  Does a backspace pressed when caret is within indentation unindent?

   SC_TIME_FOREVER                : constant := 16#0098_9680#;

   procedure SetMouseDwellTime
     (Control : in out Scintilla_Type; periodMilliseconds : Integer);
   --  Sets the time the mouse must sit still to generate a mouse
   --  dwell event.  If set to SC_TIME_FOREVER, the default, no dwell
   --  events will be generated.

   function GetMouseDwellTime (Control : Scintilla_Type) return Integer;
   --  Retrieve the time the mouse must sit still to generate a mouse
   --  dwell event.

   function WordStartPosition
     (Control : Scintilla_Type; pos : Position; onlyWordCharacters : Boolean)
     return Integer;
   --  Get Position of start of word.

   function WordEndPosition
     (Control : Scintilla_Type; pos : Position; onlyWordCharacters : Boolean)
     return Integer;
   --  Get Position of end of word.

   SC_WRAP_NONE                   : constant := 0;
   SC_WRAP_WORD                   : constant := 1;

   procedure SetWrapMode (Control : in out Scintilla_Type; mode : Integer);
   --  Sets whether text is word wrapped.

   function GetWrapMode (Control : Scintilla_Type) return Integer;
   --  Retrieve whether text is word wrapped.

   SC_CACHE_NONE                  : constant := 0;
   SC_CACHE_CARET                 : constant := 1;
   SC_CACHE_PAGE                  : constant := 2;
   SC_CACHE_DOCUMENT              : constant := 3;

   procedure SetLayoutCache (Control : in out Scintilla_Type; mode : Integer);
   --  Sets the degree of caching of layout information.

   function GetLayoutCache (Control : Scintilla_Type) return Integer;
   --  Retrieve the degree of caching of layout information.

   procedure SetScrollWidth
     (Control : in out Scintilla_Type; pixelWidth : Integer);
   --  Sets the document width assumed for scrolling.

   function GetScrollWidth (Control : Scintilla_Type) return Integer;
   --  Retrieve the document width assumed for scrolling.

   function TextWidth
     (Control : Scintilla_Type; style : Integer; text : GString)
     return Integer;
   --  Measure the pixel width of some text in a particular style.
   --  Nul terminated text argument.
   --  Does not handle tab or control characters.

   procedure SetEndAtLastLine
     (Control : in out Scintilla_Type; endAtLastLine : Boolean);
   --  Sets the scroll range so that maximum scroll Position has
   --  the last line at the bottom of the view (default).
   --  Setting this to false allows scrolling one page below the last line.

   function GetEndAtLastLine (Control : Scintilla_Type) return Integer;
   --  Retrieve whether the maximum scroll Position has the last
   --  line at the bottom of the view.

   procedure LineDown (Control : in out Scintilla_Type);
   --  Start of key messages
   --  Move caret down one line.

   procedure LineDownExtend (Control : in out Scintilla_Type);
   --  Move caret down one line extending selection to new caret Position.

   procedure LineUp (Control : in out Scintilla_Type);
   --  Move caret up one line.

   procedure LineUpExtend (Control : in out Scintilla_Type);
   --  Move caret up one line extending selection to new caret Position.

   procedure CharLeft (Control : in out Scintilla_Type);
   --  Move caret left one character.

   procedure CharLeftExtend (Control : in out Scintilla_Type);
   --  Move caret left one character extending selection to new caret Position.

   procedure CharRight (Control : in out Scintilla_Type);
   --  Move caret right one character.

   procedure CharRightExtend (Control : in out Scintilla_Type);
   --  Move caret right one character extending selection to new caret
   --  Position.

   procedure WordLeft (Control : in out Scintilla_Type);
   --  Move caret left one word.

   procedure WordLeftExtend (Control : in out Scintilla_Type);
   --  Move caret left one word extending selection to new caret Position.

   procedure WordRight (Control : in out Scintilla_Type);
   --  Move caret right one word.

   procedure WordRightExtend (Control : in out Scintilla_Type);
   --  Move caret right one word extending selection to new caret Position.

   procedure Home (Control : in out Scintilla_Type);
   --  Move caret to first Position on line.

   procedure HomeExtend (Control : in out Scintilla_Type);
   --  Move caret to first Position on line extending selection to new
   --  caret Position.

   procedure LineEnd (Control : in out Scintilla_Type);
   --  Move caret to last Position on line.

   procedure LineEndExtend (Control : in out Scintilla_Type);
   --  Move caret to last Position on line extending selection to new
   --  caret Position.

   procedure DocumentStart (Control : in out Scintilla_Type);
   --  Move caret to first Position in document.

   procedure DocumentStartExtend (Control : in out Scintilla_Type);
   --  Move caret to first Position in document extending selection to
   --  new caret Position.

   procedure DocumentEnd (Control : in out Scintilla_Type);
   --  Move caret to last Position in document.

   procedure DocumentEndExtend (Control : in out Scintilla_Type);
   --  Move caret to last Position in document extending selection to
   --  new caret Position.

   procedure PageUp (Control : in out Scintilla_Type);
   --  Move caret one page up.

   procedure PageUpExtend (Control : in out Scintilla_Type);
   --  Move caret one page up extending selection to new caret Position.

   procedure PageDown (Control : in out Scintilla_Type);
   --  Move caret one page down.

   procedure PageDownExtend (Control : in out Scintilla_Type);
   --  Move caret one page down extending selection to new caret Position.

   procedure EditToggleOvertype (Control : in out Scintilla_Type);
   --  Switch from insert to overtype mode or the reverse.

   procedure Cancel (Control : in out Scintilla_Type);
   --  Cancel any modes such as call tip or auto-completion list display.

   procedure DeleteBack (Control : in out Scintilla_Type);
   --  Delete the selection or if no selection, the character before the caret.

   procedure Tab (Control : in out Scintilla_Type);
   --  If selection is empty or all on one line replace the selection
   --  with a tab character.  If more than one line selected, indent
   --  the lines.

   procedure BackTab (Control : in out Scintilla_Type);
   --  Dedent the selected lines.

   procedure NewLine (Control : in out Scintilla_Type);
   --  Insert a new line, may use a CRLF, CR or LF depending on EOL mode.

   procedure FormFeed (Control : in out Scintilla_Type);
   --  Insert a Form Feed character.

   procedure VCHome (Control : in out Scintilla_Type);
   --  Move caret to before first visible character on line.
   --  If already there move to first character on line.

   procedure VCHomeExtend (Control : in out Scintilla_Type);
   --  Like VCHome but extending selection to new caret Position.

   procedure ZoomIn (Control : in out Scintilla_Type);
   --  Magnify the displayed text by increasing the sizes by 1 point.

   procedure ZoomOut (Control : in out Scintilla_Type);
   --  Make the displayed text smaller by decreasing the sizes by 1 point.

   procedure DelWordLeft (Control : in out Scintilla_Type);
   --  Delete the word to the left of the caret.

   procedure DelWordRight (Control : in out Scintilla_Type);
   --  Delete the word to the right of the caret.

   procedure LineCut (Control : in out Scintilla_Type);
   --  Cut the line containing the caret.

   procedure LineDelete (Control : in out Scintilla_Type);
   --  Delete the line containing the caret.

   procedure LineTranspose (Control : in out Scintilla_Type);
   --  Switch the current line with the previous.

   procedure LowerCase (Control : in out Scintilla_Type);
   --  Transform the selection to lower case.

   procedure UpperCase (Control : in out Scintilla_Type);
   --  Transform the selection to upper case.

   procedure LineScrollDown (Control : in out Scintilla_Type);
   --  Scroll the document down, keeping the caret visible.

   procedure LineScrollUp (Control : in out Scintilla_Type);
   --  Scroll the document up, keeping the caret visible.

   procedure DeleteBackNotLine (Control : in out Scintilla_Type);
   --  Delete the selection or if no selection, the character before the caret.
   --  Will not delete the chraacter before at the start of a line.

   procedure MoveCaretInsideView (Control : in out Scintilla_Type);
   --  Move the caret inside current view if it's not there already

   function LineLength (Control : Scintilla_Type; line : Integer)
                       return Integer;
   --  How many characters are on a line, not including end of line characters.

   procedure BraceHighlight
     (Control : in out Scintilla_Type; pos1 : Position; ppos2 : Position);
   --  Highlight the characters at two Positions.

   procedure BraceBadLight (Control : in out Scintilla_Type; pos : Position);
   --  Highlight the character at a Position indicating there is no
   --  matching brace.

   function BraceMatch (Control : Scintilla_Type; pos : Position)
                       return Position;
   --  Find the Position of a matching brace or INVALID_POSITION if no match.

   function GetViewEOL (Control : Scintilla_Type) return Boolean;
   --  Are the end of line characters visible.

   procedure SetViewEOL (Control : in out Scintilla_Type; visible : Boolean);
   --  Make the end of line characters visible or invisible

   function GetDocPointer (Control : Scintilla_Type) return Integer;
   --  Retrieve a pointer to the document object.

   procedure SetDocPointer
     (Control : in out Scintilla_Type; pointer : Integer);
   --  Change the document object used.

   --  Notifications
   --  Type of modification and the action which caused the
   --  modification These are defined as a bit mask to make it easy to
   --  specify which notifications are wanted.  One bit is set from
   --  each of SC_MOD_* and SC_PERFORMED_*.

   SC_MOD_INSERTTEXT              : constant := 16#0001#;
   SC_MOD_DELETETEXT              : constant := 16#0002#;
   SC_MOD_CHANGESTYLE             : constant := 16#0004#;
   SC_MOD_CHANGEFOLD              : constant := 16#0008#;
   SC_PERFORMED_USER              : constant := 16#0010#;
   SC_PERFORMED_UNDO              : constant := 16#0020#;
   SC_PERFORMED_REDO              : constant := 16#0040#;
   SC_LASTSTEPINUNDOREDO          : constant := 16#0100#;
   SC_MOD_CHANGEMARKER            : constant := 16#0200#;
   SC_MOD_BEFOREINSERT            : constant := 16#0400#;
   SC_MOD_BEFOREDELETE            : constant := 16#0800#;
   SC_MODEVENTMASKALL             : constant := 16#0F77#;

   procedure SetModEventMask
     (Control : in out Scintilla_Type; mask : Interfaces.C.unsigned);
   --  Set which document modification events are sent to the container.

   EDGE_NONE                      : constant := 0;
   EDGE_LINE                      : constant := 1;
   EDGE_BACKGROUND                : constant := 2;

   function GetEdgeColumn (Control : Scintilla_Type) return Integer;
   --  Retrieve the column number which text should be kept within.

   procedure SetEdgeColumn (Control : in out Scintilla_Type; column : Integer);
   --  Set the column number of the edge.
   --  If text goes past the edge then it is highlighted.

   function GetEdgeMode (Control : Scintilla_Type) return Integer;
   --  Retrieve the edge highlight mode.

   procedure SetEdgeMode (Control : in out Scintilla_Type; mode : Integer);
   --  The edge may be displayed by a line (EDGE_LINE) or by highlighting
   --  text that goes beyond it (EDGE_BACKGROUND) or not displayed at all
   --  (EDGE_NONE).

   function GetEdgeColor (Control : Scintilla_Type)
                         return GWindows.Colors.Color_Type;
   --  Retrieve the color used in edge indication.

   procedure SetEdgeColor
     (Control   : in out Scintilla_Type;
      edgeColor : in     GWindows.Colors.Color_Type);
   --  Change the color used in edge indication.

   procedure SearchAnchor (Control : in out Scintilla_Type);
   --  Sets the current caret Position to be the search anchor.

   function SearchNext
     (Control : Scintilla_Type; flags : Integer; text : GString)
     return Integer;
   --  Find some text starting at the search anchor.
   --  Does not ensure the selection is visible.

   function SearchPrev
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

   procedure SetCaretPolicy
     (Control     : in out Scintilla_Type;
      caretPolicy : in     Integer;
      caretSlop   : in     Integer);
   --  Set the way the line the caret is on is kept visible.

   function LinesOnScreen (Control : Scintilla_Type) return Integer;
   --  Retrieves the number of lines completely visible.

   procedure UsePopUp (Control : in out Scintilla_Type; allowPopUp : Boolean);
   --  Set whether a pop up menu is displayed automatically when the
   --  user presses the wrong mouse button.

   function SelectionIsRectangle (Control : Scintilla_Type) return Boolean;
   --  Is the selection a rectangular. The alternative is the more
   --  common stream selection.

   procedure SetZoom (Control : in out Scintilla_Type; zoom : Integer);
   --  Set the zoom level. This number of points is added to the size
   --  of all fonts.  It may be positive to magnify or negative to
   --  reduce.

   function GetZoom (Control : Scintilla_Type) return Integer;
   --  Retrieve the zoom level.

   function CreateDocument (Control : Scintilla_Type) return Integer;
   --  Create a new document object.
   --  Starts with reference count of 1 and not selected into editor.

   procedure AddRefDocument (Control : in out Scintilla_Type; doc : Integer);
   --  Extend life of document.

   procedure ReleaseDocument (Control : in out Scintilla_Type; doc : Integer);
   --  Release a reference to the document, deleting document if it
   --  fades to black.

   function GetModEventMask (Control : Scintilla_Type) return Integer;
   --  Get which document modification events are sent to the container.

   procedure SetFocus (Control : in out Scintilla_Type; focus : Boolean);
   --  Change internal focus flag

   function GetFocus (Control : Scintilla_Type) return Boolean;
   --  Get internal focus flag

   procedure SetStatus (Control : in out Scintilla_Type; statusCode : Integer);
   --  Change error status - 0 = OK

   function GetStatus (Control : Scintilla_Type) return Integer;
   --  Get error status

   procedure SetMouseDownCaptures
     (Control : in out Scintilla_Type; captures : Boolean);
   --  Set whether the mouse is captured when its button is pressed

   function GetMouseDownCaptures (Control : Scintilla_Type) return Boolean;
   --  Get whether mouse gets captured

   SC_CURSORNORMAL                : constant := -1;
   SC_CURSORWAIT                  : constant := 3;

   procedure SetCursor (Control : in out Scintilla_Type; cursorType : Integer);
   --  Sets the cursor to one of the SC_CURSOR* values

   function GetCursor (Control : Scintilla_Type) return Integer;
   --  Get cursor type

   procedure SetControlCharSymbol
     (Control : in out Scintilla_Type; symbol : Integer);
   --  Change the way control characters are displayed:
   --  If symbol is < 32, keep the drawn way, else, use the given character

   function GetControlCharSymbol (Control : Scintilla_Type) return Integer;
   --  Get the way control characters are displayed

   procedure WordPartLeft (Control : in out Scintilla_Type);
   --  Move to the previous change in capitalisation

   procedure WordPartLeftExtend (Control : in out Scintilla_Type);
   --  Move to the previous change in capitalisation extending
   --  selection to new caret Position.

   procedure WordPartRight (Control : in out Scintilla_Type);
   --  Move to the change next in capitalistion

   procedure WordPartRightExtend (Control : in out Scintilla_Type);
   --  Move to the next change in capitalistion extending selection to
   --  new caret Position.

   VISIBLE_SLOP                   : constant := 16#0001#;
   VISIBLE_STRICT                 : constant := 16#0004#;

   procedure SetVisiblePolicy
     (Control       : in out Scintilla_Type;
      visiblePolicy : in     Integer;
      visibleSlop   : in     Integer);
   --  Set the way the display area is determined when a particular
   --  line is to be moved to.

   procedure DelLineLeft (Control : in out Scintilla_Type);
   --  Delete back from the current Position to the start of the line

   procedure DelLineRight (Control : in out Scintilla_Type);
   --  Delete forwards from the current Position to the end of the line

   procedure SetXOffset
     (Control : in out Scintilla_Type; newOffset : Integer);
   --  Get the xOffset (ie, horizontal scroll Position)

   function GetXOffset (Control : Scintilla_Type) return Integer;
   --  Set the xOffset (ie, horizontal scroll Position)

   procedure StartRecord (Control : in out Scintilla_Type);
   --  Start notifying the container of all key presses and commands.

   procedure StopRecord (Control : in out Scintilla_Type);
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

   procedure SetLexer (Control : in out Scintilla_Type; lexer : Integer);
   --  Set the lexing language of the document.

   function GetLexer (Control : Scintilla_Type) return Integer;
   --  Retrieve the lexing language of the document.

   procedure Colorise
     (Control : in out Scintilla_Type; start : Position; endp : Position);
   --  Colorise a segment of the document using the current lexing language.

   procedure SetProperty
     (Control : in out Scintilla_Type; key : GString; value : GString);
   --  Set up a value that may be used by a lexer for some optional feature.

   procedure SetKeyWords
     (Control    : in out Scintilla_Type;
      keywordSet : in     Integer;
      keyWords   : in     GString);
   --  Set up the key words used by the lexer.

   procedure SetLexerLanguage
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

   -------------------------------------------------------------------------
   --  Common_Control_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   type Position_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Pos     : in     Position);

   type Modified_Event is access procedure
     (Control             : in out GWindows.Base.Base_Window_Type'Class;
      Pos                 : in     Position;
      Modification_Type   : in     Integer;
      Text                : in     GString;
      Lines_Added         : in     Integer;
      Line                : in     Integer;
      Fold_Level_Now      : in     Integer;
      Fold_Level_Previous : in     Integer);

   type Macro_Read_Event is access
     procedure (Control : in out GWindows.Base.Base_Window_Type'Class;
                Message : in     Integer;
                wParam  : in     Integer;
                lParam  : in     Integer);

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
      Modification_Type   : in     Integer;
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
                                 wParam  : in     Integer;
                                 lParam  : in     Integer);

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
                          Modification_Type   : in     Integer;
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
   --  SetModEventMask function which sets which notification types
   --  are sent to the container. For example, a container may decide
   --  to see only notifications about changes to text and not styling
   --  changes by calling:

   --  SetModEventMask
   --     (Control, (SC_MOD_INSERTTEXT or SC_MOD_DELETETEXT))

   procedure On_Macro_Read (Control : in out Scintilla_Type;
                            Message : in     Integer;
                            wParam  : in     Integer;
                            lParam  : in     Integer);
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
