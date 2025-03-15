------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                    G W I N D O W S . U T I L I T I E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2018 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body GWindows.Utilities is
   use GWindows.Types;

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   type SDouble_Word is
      record
         Low  : Interfaces.C.short;
         High : Interfaces.C.short;
      end record;

   function To_SDWORD is new
     Ada.Unchecked_Conversion (Interfaces.C.unsigned, SDouble_Word);

   type Double_Word is
      record
         Low  : Interfaces.C.unsigned_short;
         High : Interfaces.C.unsigned_short;
      end record;

   function To_DWORD is new
     Ada.Unchecked_Conversion (Interfaces.C.unsigned, Double_Word);

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ---------------
   -- Make_Long --
   ---------------

   function Make_Long (Low  : in Interfaces.C.short;
                       High : in Interfaces.C.short)
                       return GWindows.Types.Lparam is
   begin
      return Lparam (Low) + 16#10000# * Lparam (High);
   end Make_Long;

   --------------
   -- Low_Word --
   --------------

   function Low_Word (L : GWindows.Types.Lparam) return Integer is
      Result : constant SDouble_Word :=
         To_SDWORD (Interfaces.C.unsigned (L and 16#FFFF_FFFF#));
   begin
      return Integer (Result.Low);
   end Low_Word;

   function Low_Word (L : GWindows.Types.Wparam) return Integer is
      Result : constant SDouble_Word :=
         To_SDWORD (Interfaces.C.unsigned (L and 16#FFFF_FFFF#));
   begin
      return Integer (Result.Low);
   end Low_Word;

   ---------------
   -- High_Word --
   ---------------

   function High_Word (L : GWindows.Types.Lparam) return Integer is
      Result : constant SDouble_Word :=
         To_SDWORD (Interfaces.C.unsigned (L and 16#FFFF_FFFF#));
   begin
      return Integer (Result.High);
   end High_Word;

   function High_Word (L : GWindows.Types.Wparam) return Integer is
      Result : constant SDouble_Word :=
         To_SDWORD (Interfaces.C.unsigned (L and 16#FFFF_FFFF#));
   begin
      return Integer (Result.High);
   end High_Word;

   -----------------------
   -- Unsigned_Low_Word --
   -----------------------

   function Unsigned_Low_Word (W : GWindows.Types.Wparam)
                              return Interfaces.C.unsigned is
      Result : constant Double_Word :=
         To_DWORD (Interfaces.C.unsigned (W and 16#FFFF_FFFF#));
   begin
      return Interfaces.C.unsigned (Result.Low);
   end Unsigned_Low_Word;

   ------------------------
   -- Unsigned_High_Word --
   ------------------------

   function Unsigned_High_Word (W : GWindows.Types.Wparam)
                               return Interfaces.C.unsigned is
      Result : constant Double_Word :=
         To_DWORD (Interfaces.C.unsigned (W and 16#FFFF_FFFF#));
   begin
      return Interfaces.C.unsigned (Result.High);
   end Unsigned_High_Word;

end GWindows.Utilities;
