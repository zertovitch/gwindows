------------------------------------------------------------------------------
--                                                                          --
--                      gwindows.windows.ex_windows                         --
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
