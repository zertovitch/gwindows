------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C U R S O R S                      --
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

with GWindows.GStrings;
with GWindows.Internal;

package body GWindows.Cursors is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------------------
   -- Load_System_Cursor --
   ------------------------

   function Load_System_Cursor (Cursor : Integer) return Cursor_Type
   is
      function LoadCursor
        (hInst : Integer := 0;
         ID    : Integer := Cursor)
        return Cursor_Type;
      pragma Import (StdCall, LoadCursor,
                     "LoadCursor" & Character_Mode_Identifier);
   begin
      return LoadCursor;
   end Load_System_Cursor;

   ---------------------------
   -- Load_Cursor_From_File --
   ---------------------------

   function Load_Cursor_From_File (File_Name : GString) return Cursor_Type
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (File_Name);

      function LoadCursorFromFile
        (Name : access GChar_C := C_Text (C_Text'First)'Access)
        return Cursor_Type;
      pragma Import (StdCall, LoadCursorFromFile,
                     "LoadCursorFromFile" & Character_Mode_Identifier);
   begin
      return LoadCursorFromFile;
   end Load_Cursor_From_File;

   -----------------
   -- Load_Cursor --
   -----------------

   function Load_Cursor (Name : GString) return Cursor_Type
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Name);

      function LoadCursor
        (hInst : GWindows.Types.Handle := GWindows.Internal.Current_hInstance;
         Name  : access GChar_C        := C_Text (C_Text'First)'Access)
        return Cursor_Type;
      pragma Import (StdCall, LoadCursor, "LoadCursor"
                     & Character_Mode_Identifier);
   begin
      return LoadCursor;
   end Load_Cursor;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position (X, Y : Integer)
   is
      procedure SetCursorPos
        (Top  : Integer := X;
         Left : Integer := Y);
      pragma Import (StdCall, SetCursorPos, "SetCursorPos");
   begin
      SetCursorPos;
   end Set_Cursor_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   function Get_Cursor_Position return GWindows.Types.Point_Type
   is
      Point : GWindows.Types.Point_Type := (0, 0);

      procedure GetCursorPos (Where : out GWindows.Types.Point_Type);
      pragma Import (StdCall, GetCursorPos, "GetCursorPos");
   begin
      GetCursorPos (Where => Point);
      return Point;
   end Get_Cursor_Position;

   -----------------
   -- Show_Cursor --
   -----------------

   procedure Show_Cursor
   is
      procedure ShowCursor (bShow : Boolean := True);
      pragma Import (StdCall, ShowCursor, "ShowCursor");
   begin
      ShowCursor;
   end Show_Cursor;

   -----------------
   -- Hide_Cursor --
   -----------------

   procedure Hide_Cursor
   is
      procedure ShowCursor (bShow : Boolean := False);
      pragma Import (StdCall, ShowCursor, "ShowCursor");
   begin
      ShowCursor;
   end Hide_Cursor;

   -----------------
   -- Clip_Cursor --
   -----------------

   procedure Clip_Cursor (Area : GWindows.Types.Rectangle_Type)
   is
      NArea : constant GWindows.Types.Rectangle_Type := Area;

      procedure ClipCursor (Area : GWindows.Types.Rectangle_Type := NArea);
      pragma Import (StdCall, ClipCursor, "ClipCursor");
   begin
      ClipCursor;
   end Clip_Cursor;

   ---------------------
   -- Get_Cursor_Clip --
   ---------------------

   function Get_Cursor_Clip return GWindows.Types.Rectangle_Type
   is
      Result : GWindows.Types.Rectangle_Type := (0, 0, 0, 0);

      procedure GetClipCursor
        (Area : out GWindows.Types.Rectangle_Type);
      pragma Import (StdCall, GetClipCursor, "GetClipCursor");
   begin
      GetClipCursor (Area => Result);
      return Result;
   end Get_Cursor_Clip;

   --------------------
   -- Release_Cursor --
   --------------------

   procedure Release_Cursor
   is
      procedure ClipCursor (Area : Integer := 0);
      pragma Import (StdCall, ClipCursor, "ClipCursor");
   begin
      ClipCursor;
   end Release_Cursor;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Cursor : Cursor_Type)
   is
      procedure SetCursor (New_Cursor : Cursor_Type := Cursor);
      pragma Import (StdCall, SetCursor, "SetCursor");
   begin
      SetCursor;
   end Set_Cursor;

   -----------------------
   -- Start_Wait_Cursor --
   -----------------------

   procedure Start_Wait_Cursor
   is
      C : constant Cursor_Type := Load_System_Cursor (IDC_WAIT);
   begin
      Set_Cursor (C);
   end Start_Wait_Cursor;

   ---------------------
   -- End_Wait_Cursor --
   ---------------------

   procedure End_Wait_Cursor
   is
      C : constant Cursor_Type := Load_System_Cursor (IDC_ARROW);
   begin
      Set_Cursor (C);
   end End_Wait_Cursor;

end GWindows.Cursors;
