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
