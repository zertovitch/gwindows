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
