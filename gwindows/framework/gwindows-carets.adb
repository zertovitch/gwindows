------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C A R E T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

package body GWindows.Carets is
   use GWindows.Base;
   use GWindows.Drawing_Objects;

   ------------------------
   -- Create_Solid_Caret --
   ------------------------

   procedure Create_Solid_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer                              := 0;
      Height : in     Integer                              := 0)
   is
      procedure CreateCaret
        (HWND : GWindows.Types.Handle := Handle (Window);
         BMP  : Integer := 0;
         W    : Integer := Width;
         H    : Integer := Height);
      pragma Import (StdCall, CreateCaret, "CreateCaret");
   begin
      CreateCaret;
   end Create_Solid_Caret;

   -----------------------
   -- Create_Gray_Caret --
   -----------------------

   procedure Create_Gray_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer                              := 0;
      Height : in     Integer                              := 0)
   is
      procedure CreateCaret
        (HWND : GWindows.Types.Handle := Handle (Window);
         BMP  : Integer := 1;
         W    : Integer := Width;
         H    : Integer := Height);
      pragma Import (StdCall, CreateCaret, "CreateCaret");
   begin
      CreateCaret;
   end Create_Gray_Caret;

   -------------------------
   -- Create_Bitmap_Caret --
   -------------------------

   procedure Create_Bitmap_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Bitmap : in     GWindows.Drawing_Objects.Bitmap_Type)
   is
      procedure CreateCaret
        (HWND : GWindows.Types.Handle := Handle (Window);
         BMP  : GWindows.Types.Handle := Handle (Bitmap);
         W    : Integer := 0;
         H    : Integer := 0);
      pragma Import (StdCall, CreateCaret, "CreateCaret");
   begin
      CreateCaret;
   end Create_Bitmap_Caret;

   -------------------
   -- Destroy_Caret --
   -------------------

   procedure Destroy_Caret
   is
      procedure DestroyCaret;
      pragma Import (StdCall, DestroyCaret, "DestroyCaret");
   begin
      DestroyCaret;
   end Destroy_Caret;

   ----------------
   -- Show_Caret --
   ----------------

   procedure Show_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      procedure ShowCaret
        (HWND : GWindows.Types.Handle := Handle (Window));
      pragma Import (StdCall, ShowCaret, "ShowCaret");
   begin
      ShowCaret;
   end Show_Caret;

   ----------------
   -- Hide_Caret --
   ----------------

   procedure Hide_Caret
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      procedure HideCaret
        (HWND : GWindows.Types.Handle := Handle (Window));
      pragma Import (StdCall, HideCaret, "HideCaret");
   begin
      HideCaret;
   end Hide_Caret;

   ------------------------
   -- Set_Caret_Position --
   ------------------------

   procedure Set_Caret_Position (X, Y : Integer)
   is
      procedure SetCaretPos (X, Y : Integer);
      pragma Import (StdCall, SetCaretPos, "SetCaretPos");
   begin
      SetCaretPos (X, Y);
   end Set_Caret_Position;

   ------------------------
   -- Get_Caret_Position --
   ------------------------

   function Get_Caret_Position return GWindows.Types.Point_Type
   is
      Result : GWindows.Types.Point_Type := (0, 0);

      procedure GetCaretPos
        (Pos : in out GWindows.Types.Point_Type);
      pragma Import (StdCall, GetCaretPos, "GetCaretPos");
   begin
      GetCaretPos (Result);
      return Result;
   end Get_Caret_Position;

end GWindows.Carets;
