------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . I N T E R N A L                     --
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

with Ada.Unchecked_Deallocation;

package body GWindows.Internal is

   function GetDesktopWindow return GWindows.Types.Handle;
   pragma Import (StdCall, GetDesktopWindow, "GetDesktopWindow");

   procedure GetClientRect
     (hwnd            : in  GWindows.Types.Handle;
      Rect            : out GWindows.Types.Rectangle_Type);
   pragma Import (StdCall, GetClientRect, "GetClientRect");

   --------------------------
   -- Add_Keyboard_Control --
   --------------------------

   procedure Add_Keyboard_Control
     (Window : GWindows.Base.Pointer_To_Base_Window_Class)
   is
   begin
      if Top_Keyboard_Control = null then
         Top_Keyboard_Control := new Keyboard_Control'(Window, null);
         Bottom_Keyboard_Control := Top_Keyboard_Control;
      else
         Bottom_Keyboard_Control.Next := new Keyboard_Control'(Window, null);
         Bottom_Keyboard_Control := Bottom_Keyboard_Control.Next;
      end if;
   end Add_Keyboard_Control;

   -----------------------------
   -- Remove_Keyboard_Control --
   -----------------------------

   procedure Remove_Keyboard_Control
     (Window : GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use type GWindows.Base.Pointer_To_Base_Window_Class;

      procedure Free is
         new Ada.Unchecked_Deallocation (Keyboard_Control,
                                         Pointer_To_Keyboard_Control);

      Current_Keyboard_Control  : Pointer_To_Keyboard_Control :=
        Top_Keyboard_Control;
      Previous_Keyboard_Control : Pointer_To_Keyboard_Control := null;
   begin
      while Current_Keyboard_Control /= null loop
         if Current_Keyboard_Control.Window = Window then

            if Current_Keyboard_Control = Top_Keyboard_Control then
               Top_Keyboard_Control := Current_Keyboard_Control.Next;
            else
               Previous_Keyboard_Control.Next := Current_Keyboard_Control.Next;
            end if;

            if Current_Keyboard_Control = Bottom_Keyboard_Control then
               Bottom_Keyboard_Control := Previous_Keyboard_Control;
            end if;

            Free (Current_Keyboard_Control);
            Current_Keyboard_Control := null;
         else
            Previous_Keyboard_Control := Current_Keyboard_Control;
            Current_Keyboard_Control := Current_Keyboard_Control.Next;
         end if;
      end loop;
   end Remove_Keyboard_Control;

   --------------------
   -- Desktop_Height --
   --------------------

   function Desktop_Height return Natural is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetClientRect (GetDesktopWindow, Rect);
      return abs (Rect.Bottom - Rect.Top);
   end Desktop_Height;

   --------------------
   -- Desktop_Width --
   --------------------

   function Desktop_Width return Natural is
      Rect : GWindows.Types.Rectangle_Type;
   begin
      GetClientRect (GetDesktopWindow, Rect);
      return abs (Rect.Right - Rect.Left);
   end Desktop_Width;

begin
   declare
      function hInst (MName : Integer := 0) return GWindows.Types.Handle;
      pragma Import (StdCall, hInst, "GetModuleHandleA");
      --  Load instance handle via Win32 API

      --        function hInst return Interfaces.C.long;
      --        pragma Import (C, hInst, "rts_get_hInstance");
      --  Load Instance information from Ada Windows run time

      function GetCurrentThreadId
        return Interfaces.C.unsigned_long;
      pragma Import (StdCall, GetCurrentThreadId, "GetCurrentThreadId");
   begin
      Current_hInstance := hInst;

      Main_Thread_ID := GetCurrentThreadId;
   end;
end GWindows.Internal;
