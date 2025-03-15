------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                    G W I N D O W S . U T I L I T I E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2022 David Botton                   --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Types;

package GWindows.Utilities is

   function Low_Word (L : GWindows.Types.Lparam) return Integer;
   function Low_Word (L : GWindows.Types.Wparam) return Integer;
   function Unsigned_Low_Word (W : GWindows.Types.Wparam)
                              return Interfaces.C.unsigned;
   --  Return the low word of a DWORD

   function High_Word (L : GWindows.Types.Lparam) return Integer;
   function High_Word (L : GWindows.Types.Wparam) return Integer;
   function Unsigned_High_Word (W : GWindows.Types.Wparam)
                               return Interfaces.C.unsigned;
   --  Return the high word of a DWORD

   function Make_Long (Low  : in Interfaces.C.short;
                       High : in Interfaces.C.short)
                       return GWindows.Types.Lparam;

   type ANSI_Unicode_Choice is array (Character_Mode_Type) of Interfaces.C.int;

end GWindows.Utilities;
