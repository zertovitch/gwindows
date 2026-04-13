------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                  G W I N D O W S . C L I P B O A R D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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

with GWindows.Base;

with Ada.Strings.Unbounded;

package GWindows.Clipboard is

   type Copy_Format_Sequence is
      (Single_Format, First_Format, Intermediate_Format, Last_Format);
   --  It is possible to copy contents in multiple formats.
   --  In that case, the first format sent should use `First`,
   --  the formats between the first and the last should use `Intermediate`,
   --  and the last format should use `Last`.

   ----------------------------------------------------
   --  Transfer of TEXTS to, or from, the clipboard  --
   ----------------------------------------------------

   ------------
   --  Copy  --
   ------------

   procedure Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in GString;
       Format_Position : in Copy_Format_Sequence := Single_Format);

   procedure Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in GString_Unbounded;
       Format_Position : in Copy_Format_Sequence := Single_Format);

   -------------
   --  Paste  --
   -------------

   function Clipboard_Text
      (Owner : in Base.Base_Window_Type'Class)
      return GString;

   function Is_Clipboard_Text_Available
      return Boolean;

   --  The following subprograms are older versions
   --  (pre-Aug-2018), supporting only ANSI strings:

   ------------
   --  Copy  --
   ------------

   procedure Set_Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in String;
       Format_Position : in Copy_Format_Sequence := Single_Format);
   pragma Obsolescent (Set_Clipboard_Text);

   procedure Set_Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in Ada.Strings.Unbounded.Unbounded_String;
       Format_Position : in Copy_Format_Sequence := Single_Format);
   pragma Obsolescent (Set_Clipboard_Text);

   -------------
   --  Paste  --
   -------------

   function Get_Clipboard_Text
      (Owner : in Base.Base_Window_Type'Class)
      return String;
   pragma Obsolescent (Get_Clipboard_Text);

   function Is_Clipboard_Text
      return Boolean;
   pragma Obsolescent (Is_Clipboard_Text);

   ---------------------------------------------------------------------------
   --  Transfer of HTML contents (can be a code fragment) to the clipboard  --
   ---------------------------------------------------------------------------

   ------------
   --  Copy  --
   ------------

   procedure Clipboard_HTML
      (Owner           : in Base.Base_Window_Type'Class;
       HTML            : in GString;
       Format_Position : in Copy_Format_Sequence := Single_Format);

   procedure Clipboard_HTML
      (Owner           : in Base.Base_Window_Type'Class;
       HTML            : in GString_Unbounded;
       Format_Position : in Copy_Format_Sequence := Single_Format);

end GWindows.Clipboard;
