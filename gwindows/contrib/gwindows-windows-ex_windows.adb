------------------------------------------------------------------------------
--                                                                          --
--                      gwindows.windows.ex_windows                         --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion; use Ada;
with Interfaces.C; use Interfaces.C;
with GWindows.GStrings; use GWindows.GStrings;

package body GWindows.Windows.Ex_Windows is

--   WM_USER             : constant := 16#400#;
--   TB_GETTOOLTIPS      : constant := WM_USER + 35;

   type NMTTDISPINFO_Type is
      record
         hdr       : GWindows.Base.Notification;
         lptstr    : Interfaces.C.int;
         SzText    : char_array (1 .. 80);
         Hinst     : Interfaces.C.long;
         UFlags    : Interfaces.C.int;
         Lparam    : Integer;
      end record;

   type NMTTDISPINFO_Pointer is access all NMTTDISPINFO_Type;

   function Message_To_NMTTDISPINFO_Pointer is
      new Unchecked_Conversion (Pointer_To_Notification,
                                NMTTDISPINFO_Pointer);

   ----------------------
   -- activate_tooltip --
   ----------------------

   procedure Activate_Tooltip (Window  : in out Ex_Window_Type;
                               Toolbar : in out Ex_Toolbar_Control_Type) is

   begin
      Window.Toolbar := Toolbar'Unchecked_Access;
   end Activate_Tooltip;

   --------------------
   -- event-handling --
   --------------------

   ---------------
   -- on_notify --
   ---------------

   procedure On_Notify (Window       : in out Ex_Window_Type;
                        Message      : in     Pointer_To_Notification;
                        Control      : in     Pointer_To_Base_Window_Class;
                        Return_Value : in out Interfaces.C.long)
   is
      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      Dispinfo_Ptr  : NMTTDISPINFO_Pointer :=
        Message_To_NMTTDISPINFO_Pointer (Message);
      Last : size_t;
   begin
      if Message.Code = -520 then
         To_C (Item   => To_String (Get_Tooltip (Window.Toolbar.all,
                                                 Integer(Dispinfo_Ptr.hdr.ID))),
               Target => Dispinfo_Ptr.SzText,
               Count => Last);
      end if;
   exception
      when others =>
         null;
   end On_Notify;

end GWindows.Windows.Ex_Windows;
