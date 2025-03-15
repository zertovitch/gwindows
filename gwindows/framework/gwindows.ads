------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--                             G W I N D O W S                              --
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
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Unbounded;
with Interfaces.C;

package GWindows is
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("-lgdi32");

   type Character_Mode_Type is (ANSI, Unicode);

   --  Bind to UNICODE version of the Windows API.
   --
   --  For setting the character mode of GWindows (concerns
   --  this file and 5 others), please use the ansi.cmd or
   --  unicode.cmd scripts.

   Character_Mode : constant Character_Mode_Type := Unicode;
   Character_Mode_Identifier : constant String := "W";

   subtype GCharacter is Wide_Character;
   subtype GString is Wide_String;
   subtype GString_Unbounded is
      Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Null_GString_Unbounded : constant GString_Unbounded :=
      Ada.Strings.Wide_Unbounded.Null_Unbounded_Wide_String;

   subtype GChar_C is Interfaces.C.wchar_t;
   subtype GString_C is Interfaces.C.wchar_array;
   GString_C_Null : constant GChar_C := Interfaces.C.wide_nul;

   --  Universal

   type Pointer_To_GCharacter is access all GCharacter;
   type Pointer_To_GString is access all GString;
   type Pointer_To_GChar_C is access all GChar_C;
   type Pointer_To_GString_C is access all GString_C;

end GWindows;
