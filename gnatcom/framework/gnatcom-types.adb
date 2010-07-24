------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . T Y P E S                         --
--                                                                          --
--                                B o d y                                   --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Interfaces.C.Pointers;

package body GNATCOM.Types is

   package wchar_array_pointer is new Interfaces.C.Pointers
     (index              => Interfaces.C.size_t,
      element            => Interfaces.C.wchar_t,
      element_array      => Interfaces.C.wchar_array,
      default_terminator => Interfaces.C.wide_nul);

   --------------------
   -- To_wchar_array --
   --------------------

   function To_C (From : access Interfaces.C.wchar_t)
     return Interfaces.C.wchar_array
   is
      WC_Array : constant Interfaces.C.wchar_array :=
        wchar_array_pointer.Value (wchar_array_pointer.Pointer (From));
   begin
      return WC_Array;
   end To_C;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (From : access Interfaces.C.wchar_t) return String is
   begin
      --  To_String Should be replaced with a function that better
      --  handles internationalization.

      return Ada.Characters.Handling.To_String (To_Ada (From));
   end To_Ada;

   function To_Ada (From : access Interfaces.C.wchar_t) return Wide_String is
   begin
      return Interfaces.C.To_Ada (To_C (From));
   end To_Ada;

end GNATCOM.Types;
