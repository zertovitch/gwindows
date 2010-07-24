------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--              G W I N D O W S . E D I T _ B O X E S . R I C H             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
      ID                : in     Integer;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False)
   is
   begin
      Create_Multi_Line (Edit, Parent,
                         Text,
                         Left, Top, Width, Height,
                         Horizontal_Scroll, Vertical_Scroll,
                         Capture_Return, ID,
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
      Return_String : GString :=
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
