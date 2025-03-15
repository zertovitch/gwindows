------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--              G W I N D O W S . E D I T _ B O X E S . R I C H             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with GNATCOM.Types;
with TOM.ITextDocument_Interface;
with GWindows.GStrings;

package body GWindows.Edit_Boxes.Rich is
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   ES_MULTILINE               : constant := 4;
   ES_AUTOVSCROLL             : constant := 64;
   ES_AUTOHSCROLL             : constant := 128;
   ES_WANTRETURN              : constant := 4096;
   WS_VSCROLL                 : constant := 2097152;
   WS_HSCROLL                 : constant := 1048576;
   WS_TABSTOP                 : constant := 65536;

   EM_GETOLEINTERFACE : constant := 1084;

   EM_SETEVENTMASK         : constant := 1093;
--   ENM_NONE                : constant := 0;
   ENM_CHANGE              : constant := 1;
   ENM_UPDATE              : constant := 2;
   ENM_SCROLL              : constant := 4;
--   ENM_KEYEVENTS           : constant := 65536;
--   ENM_MOUSEEVENTS         : constant := 131072;
--   ENM_REQUESTRESIZE       : constant := 262144;
--   ENM_SELCHANGE           : constant := 524288;
--   ENM_DROPFILES           : constant := 1048576;
--   ENM_PROTECTED           : constant := 2097152;
--   ENM_CORRECTTEXT         : constant := 4194304;
--   ENM_IMECHANGE           : constant := 8388608;
--   ENM_LANGCHANGE          : constant := 16777216;
--   ENM_OBJECTPOSITIONS     : constant := 33554432;
--   ENM_LINK                : constant := 67108864;

   procedure LoadLibrary (LibName : GString_C);
   pragma Import (StdCall, LoadLibrary, "LoadLibrary"
                    & Character_Mode_Identifier);

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean                            := True;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
      Styles : Interfaces.C.unsigned := WS_TABSTOP;
   begin
      if Horizontal_Scroll then
         Styles := Styles or ES_AUTOHSCROLL;
      end if;

      Create_Control (Edit,
                      Parent,
                      "RichEdit20A",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (Edit);

      Edit.Multi_Line := False;

      if Show then
         GWindows.Edit_Boxes.Rich.Show (Edit);
      end if;
   end Create;

   -----------------------
   -- Create_Multi_Line --
   -----------------------

   procedure Create_Multi_Line
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean                            := False;
      Vertical_Scroll   : in     Boolean                            := True;
      Capture_Return    : in     Boolean                            := True;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
      Styles : Interfaces.C.unsigned := ES_MULTILINE or WS_TABSTOP;
   begin
      if Capture_Return then
         Styles := Styles or ES_WANTRETURN;
      end if;

      if Vertical_Scroll then
         Styles := Styles or ES_AUTOVSCROLL or WS_VSCROLL;
      end if;

      if Horizontal_Scroll then
         Styles := Styles or ES_AUTOHSCROLL or WS_HSCROLL;
      end if;

      Create_Control (Edit,
                      Parent,
                      "RichEdit20A",
                      Text,
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (Edit);

      if Show then
         GWindows.Edit_Boxes.Rich.Show (Edit);
      end if;
   end Create_Multi_Line;

   ------------
   -- Create --
   ------------

   procedure Create
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean;
      Vertical_Scroll   : in     Boolean;
      Capture_Return    : in     Boolean;
      Read_Only         : in     Boolean                            := False;
      ID                : in     Integer;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
   begin
      Create_Multi_Line (Edit, Parent,
                         Text,
                         Left, Top, Width, Height,
                         Horizontal_Scroll, Vertical_Scroll,
                         Capture_Return, Read_Only, ID,
                         Show, Is_Dynamic);
   end Create;

   -----------------------
   -- Get_ITextDocument --
   -----------------------

   function Get_ITextDocument (Edit : in Rich_Edit_Box_Type)
                              return TOM.Pointer_To_ITextDocument
   is
      use GNATCOM.Types;
      use TOM.ITextDocument_Interface;

      RichEditOle : aliased Pointer_To_IUnknown;

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle      := Handle (Edit);
         uMsg   : Interfaces.C.int           := EM_GETOLEINTERFACE;
         wParam : GWindows.Types.Wparam      := 0;
         lParam : access Pointer_To_IUnknown := RichEditOle'Access);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);

      Document : ITextDocument_Type;

   begin
      SendMessage;
      Attach (Document, RichEditOle);
      AddRef (Document);
      return Pointer (Document);
   end Get_ITextDocument;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Edit : in out Rich_Edit_Box_Type) is
      use GWindows.Types;
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Edit);
         uMsg   : Interfaces.C.int      := EM_SETEVENTMASK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam :=
           ENM_CHANGE or ENM_SCROLL or ENM_UPDATE);
      pragma Import (StdCall, SendMessage, "SendMessage"
                       & Character_Mode_Identifier);
   begin
      SendMessage;
      On_Create (Edit_Box_Type (Edit));
   end On_Create;

   ---------------
   -- Line_Text --
   ---------------

   function Line_Text (Edit : Rich_Edit_Box_Type;
                       Line : Positive)
                      return GString
   is
      Return_String : constant GString :=
        Line_Text (Multi_Line_Edit_Box_Type (Edit), Line);
   begin
      if Return_String = "" then
         return "";
      else
         return Return_String (Return_String'First .. Return_String'Last - 1);
      end if;
   end Line_Text;

begin
   LoadLibrary (GWindows.GStrings.To_GString_C ("riched20.dll"));
end GWindows.Edit_Boxes.Rich;
