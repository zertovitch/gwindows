------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . E D I T _ B O X E S                    --
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

with GWindows.Base;

with GWindows.Types;
package GWindows.Edit_Boxes is

   -------------------------------------------------------------------------
   --  Edit_Box_Type
   -------------------------------------------------------------------------

   type Edit_Box_Type is new GWindows.Base.Base_Window_Type with private;
   type Edit_Box_Access is access all Edit_Box_Type;
   type Pointer_To_Edit_Box_Class is access all Edit_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Edit              : in out Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                            := 0;
      Top               : in     Integer                            := 0;
      Width             : in     Integer                            := 0;
      Height            : in     Integer                            := 0;
      Horizontal_Scroll : in     Boolean                            := True;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Edit Box

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Lower_Case_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True);
   function Lower_Case_Only (Window : in Edit_Box_Type) return Boolean;
   --  When on converts all characters to lower case

   procedure Upper_Case_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True);
   function Upper_Case_Only (Window : in Edit_Box_Type) return Boolean;
   --  When on converts all characters to upper case

   procedure Digits_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True);
   function Digits_Only (Window : in Edit_Box_Type) return Boolean;
   --  Only allow digits

   procedure Read_Only (Window : in out Edit_Box_Type;
                              State  : Boolean := True);
   function Read_Only (Window : in Edit_Box_Type) return Boolean;
   --  Read only

   Off : constant Character := Character'Val (0);
   procedure Password (Window        : in out Edit_Box_Type;
                       Password_Char : in     Character     := '*');
   function Password (Window : in Edit_Box_Type) return Boolean;
   --  Displays a Password_Char for each typed letter
   --  Use Password (Window, Off) to turn off password
   --  Does not work for Multi_Line_Edit_Boxes

   function Can_Undo (Edit : in Edit_Box_Type) return Boolean;
   --  Returns true if an undo operation is possible

   function First_Visible (Edit : in Edit_Box_Type) return Natural;
   --  Returns first visible character (if multi line first visible line)
   --  Always returns 0 for single line rich edit controls

   procedure Text_Limit (Edit : in out Edit_Box_Type;
                         Size : in     Natural);
   function Text_Limit (Edit : in Edit_Box_Type) return Natural;
   --  Maximum number of characters that can be entered

   procedure Modified (Edit  : in out Edit_Box_Type;
                       State : in     Boolean       := True);
   function Modified (Edit : in Edit_Box_Type) return Boolean;
   --  Modification flag

   procedure Get_Selection
     (Edit           : in out Edit_Box_Type;
      Start_Position :    out Natural;
      End_Position   :    out Natural);
   procedure Set_Selection
     (Edit           : in out Edit_Box_Type;
      Start_Position : in     Integer;
      End_Position   : in     Integer);
   --  Selection in edit box

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Methods
   -------------------------------------------------------------------------

   procedure Replace_Selection
     (Edit     : in out Edit_Box_Type;
      Text     : in     GString;
      Can_Undo : in     Boolean       := True);
   --  Selection in edit box

   procedure Cut (Edit : in out Edit_Box_Type);
   --  Send a cut command

   procedure Copy (Edit : in out Edit_Box_Type);
   --  Send a copy command

   procedure Paste (Edit : in out Edit_Box_Type);
   --  Send a paste command

   procedure Clear (Edit : in out Edit_Box_Type);
   --  Send a clear command

   procedure Undo (Edit : in out Edit_Box_Type);
   --  Send an undo command

   procedure Clear_Undo (Edit : in out Edit_Box_Type);
   --  Clears the undo buffer

   function Recommended_Size (Edit : in Edit_Box_Type)
                             return GWindows.Types.Size_Type;

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Focus_Handler (Edit    : in out Edit_Box_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Focus (Edit : in out Edit_Box_Type);

   procedure On_Lost_Focus_Handler
     (Edit    : in out Edit_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (Edit : in out Edit_Box_Type);

   procedure On_Change_Handler (Edit    : in out Edit_Box_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Change (Edit : in out Edit_Box_Type);

   procedure On_Max_Text_Handler (Edit    : in out Edit_Box_Type;
                                  Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Max_Text (Edit : in out Edit_Box_Type);

   procedure On_Horizontal_Scroll_Handler
     (Edit    : in out Edit_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Horizontal_Scroll (Edit : in out Edit_Box_Type);

   procedure On_Vertical_Scroll_Handler
     (Edit    : in out Edit_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Vertical_Scroll (Edit : in out Edit_Box_Type);

   procedure On_Update_Handler (Edit    : in out Edit_Box_Type;
                                Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Update (Edit : in out Edit_Box_Type);

   procedure On_Out_Of_Memory_Handler
     (Edit    : in out Edit_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Out_Of_Memory (Edit : in out Edit_Box_Type);

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Focus (Edit : in out Edit_Box_Type);
   --  Received focus

   procedure On_Lost_Focus (Edit : in out Edit_Box_Type);
   --  Lost focus

   procedure On_Change (Edit : in out Edit_Box_Type);
   --  Change made to edit box text

   procedure On_Max_Text (Edit : in out Edit_Box_Type);
   --  Reached max text for this control

   procedure On_Horizontal_Scroll (Edit : in out Edit_Box_Type);
   --  Horizontal Scroll

   procedure On_Vertical_Scroll (Edit : in out Edit_Box_Type);
   --  Vertical Scroll

   procedure On_Update (Edit : in out Edit_Box_Type);
   --  Sent before redrawing text in control

   procedure On_Out_Of_Memory (Edit : in out Edit_Box_Type);
   --  Control can not allocate more memory

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Command (Window  : in out Edit_Box_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Receives command messags from parent window

   -------------------------------------------------------------------------
   --  Mutli_Line_Edit_Box_Type
   -------------------------------------------------------------------------

   type Multi_Line_Edit_Box_Type is new Edit_Box_Type with private;
   type Multi_Line_Edit_Box_Access is access all Multi_Line_Edit_Box_Type;

   -------------------------------------------------------------------------
   --  Mutli_Line_Edit_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                            := 0;
      Top               : in     Integer                            := 0;
      Width             : in     Integer                            := 0;
      Height            : in     Integer                            := 0;
      Horizontal_Scroll : in     Boolean                            := False;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Multi Line Edit Box with vertical scroll bar and return capture

   procedure Create_Multi_Line
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                            := 0;
      Top               : in     Integer                            := 0;
      Width             : in     Integer                            := 0;
      Height            : in     Integer                            := 0;
      Horizontal_Scroll : in     Boolean                            := False;
      Vertical_Scroll   : in     Boolean                            := True;
      Capture_Return    : in     Boolean                            := True;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Multi Line Edit Box

   procedure Create
     (Edit              : in out Multi_Line_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer                            := 0;
      Top               : in     Integer                            := 0;
      Width             : in     Integer                            := 0;
      Height            : in     Integer                            := 0;
      Horizontal_Scroll : in     Boolean;
      Vertical_Scroll   : in     Boolean;
      Capture_Return    : in     Boolean;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Multi Line Edit Box

   -------------------------------------------------------------------------
   --  Mutli_Line_Edit_Box_Type - Properties
   -------------------------------------------------------------------------

   function Line_Text (Edit : Multi_Line_Edit_Box_Type;
                       Line : Positive)
                      return GString;
   --  Text at line

   function Line_Length (Edit : Multi_Line_Edit_Box_Type;
                         Line : Positive)
                        return Natural;
   --  Length of text at line

   function Line_Count (Edit : Multi_Line_Edit_Box_Type) return Natural;
   --  Number of lines

   function Scroll_Position (Edit : in Multi_Line_Edit_Box_Type)
                            return Integer;
   --  Scroll position

   -------------------------------------------------------------------------
   --  Mutli_Line_Edit_Box_Type - Methods
   -------------------------------------------------------------------------

   function Line_From_Position (Edit     : Multi_Line_Edit_Box_Type;
                                Position : Natural)
                               return Positive;
   --  Returns the line containing the character at position
   --  If Position = 0 then return line of selection or current line

   function Position_From_Line (Edit : Multi_Line_Edit_Box_Type;
                                Line : Natural)
                               return Positive;
   --  Returns the position of the first character on line
   --  If Line = 0 then return line of selection or current line

   type Scroll_Command_Type is (Line_Up, Line_Down, Page_Up, Page_Down);

   procedure Scroll (Edit    : in out Multi_Line_Edit_Box_Type;
                     Command : in     Scroll_Command_Type);
   --  Scroll box

   procedure Scroll_To_Caret (Edit : in out Multi_Line_Edit_Box_Type);
   --  Scroll box so that input caret is in view

   function Recommended_Size (Edit : in Multi_Line_Edit_Box_Type)
                             return GWindows.Types.Size_Type;

private
   type Edit_Box_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Focus_Event             : GWindows.Base.Action_Event := null;
         On_Lost_Focus_Event        : GWindows.Base.Action_Event := null;
         On_Change_Event            : GWindows.Base.Action_Event := null;
         On_Max_Text_Event          : GWindows.Base.Action_Event := null;
         On_Horizontal_Scroll_Event : GWindows.Base.Action_Event := null;
         On_Vertical_Scroll_Event   : GWindows.Base.Action_Event := null;
         On_Update_Event            : GWindows.Base.Action_Event := null;
         On_Out_Of_Memory_Event     : GWindows.Base.Action_Event := null;
      end record;

   type Multi_Line_Edit_Box_Type is new Edit_Box_Type with null record;

   ----------
   --  Visible for child packages

   --  Win32 Edit Box Styles

--     ES_LEFT                    : constant := 0;
--     ES_CENTER                  : constant := 1;
--     ES_RIGHT                   : constant := 2;
   ES_MULTILINE               : constant := 4;
   ES_UPPERCASE               : constant := 8;
   ES_LOWERCASE               : constant := 16;
--     ES_PASSWORD                : constant := 32;
   ES_AUTOVSCROLL             : constant := 64;
   ES_AUTOHSCROLL             : constant := 128;
--     ES_NOHIDESEL               : constant := 256;
--     ES_OEMCONVERT              : constant := 1024;
   ES_READONLY                : constant := 2048;
   ES_WANTRETURN              : constant := 4096;
   ES_NUMBER                  : constant := 8192;
   WS_VSCROLL                 : constant := 2097152;
   WS_HSCROLL                 : constant := 1048576;
   WS_TABSTOP                 : constant := 65536;

   --  Win32 Edit Box Messages

   EM_GETSEL                  : constant := 176;
   EM_SETSEL                  : constant := 177;
--     EM_GETRECT                 : constant := 178;
--     EM_SETRECT                 : constant := 179;
--     EM_SETRECTNP               : constant := 180;
--     EM_SCROLL                  : constant := 181;
--     EM_LINESCROLL              : constant := 182;
   EM_SCROLLCARET             : constant := 183;
   EM_GETMODIFY               : constant := 184;
   EM_SETMODIFY               : constant := 185;
   EM_GETLINECOUNT            : constant := 186;
   EM_LINEINDEX               : constant := 187;
--     EM_SETHANDLE               : constant := 188;
--     EM_GETHANDLE               : constant := 189;
   EM_GETTHUMB                : constant := 190;
   EM_LINELENGTH              : constant := 193;
   EM_REPLACESEL              : constant := 194;
   EM_GETLINE                 : constant := 196;
   EM_LIMITTEXT               : constant := 197;
   EM_CANUNDO                 : constant := 198;
   EM_UNDO                    : constant := 199;
--     EM_FMTLINES                : constant := 200;
   EM_LINEFROMCHAR            : constant := 201;
--     EM_SETTABSTOPS             : constant := 203;
   EM_SETPASSWORDCHAR         : constant := 204;
   EM_EMPTYUNDOBUFFER         : constant := 205;
   EM_GETFIRSTVISIBLELINE     : constant := 206;
   EM_SETREADONLY             : constant := 207;
--     EM_SETWORDBREAKPROC        : constant := 208;
--     EM_GETWORDBREAKPROC        : constant := 209;
   EM_GETPASSWORDCHAR         : constant := 210;
--     EM_SETMARGINS              : constant := 211;
--     EM_GETMARGINS              : constant := 212;
--     EM_SETLIMITTEXT            : constant := 197;
   EM_GETLIMITTEXT            : constant := 213;
--     EM_POSFROMCHAR             : constant := 214;
--     EM_CHARFROMPOS             : constant := 215;
   SB_LINEUP                  : constant := 0;
   SB_LINEDOWN                : constant := 1;
   SB_PAGEUP                  : constant := 2;
   SB_PAGEDOWN                : constant := 3;

   --  Win32 Edit Box Notifications

   EN_SETFOCUS                : constant := 256;
   EN_KILLFOCUS               : constant := 512;
   EN_CHANGE                  : constant := 768;
   EN_UPDATE                  : constant := 1024;
   EN_ERRSPACE                : constant := 1280;
   EN_MAXTEXT                 : constant := 1281;
   EN_HSCROLL                 : constant := 1537;
   EN_VSCROLL                 : constant := 1538;

   WM_CUT                     : constant := 768;
   WM_COPY                    : constant := 769;
   WM_PASTE                   : constant := 770;
   WM_CLEAR                   : constant := 771;

end GWindows.Edit_Boxes;
