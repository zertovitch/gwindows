------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . G S T R I N G S . I O                  --
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

--  Simple IO for GStrings based on GNAT.IO

with GNAT.IO;

package body GWindows.GStrings.IO is

   ---------
   -- Get --
   ---------

   procedure Get (X : out Integer) is
   begin
      GNAT.IO.Get (X);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (C : out GCharacter) is
      Value : Character;
   begin
      GNAT.IO.Get (Value);

      declare
         Convert_String   : constant String (1 .. 2) := Value & " ";
         Converted_String : constant GString :=
           To_GString_From_String (Convert_String);
      begin
         C := Converted_String (1);
      end;
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return GString is
      Value : String (1 .. 1024);
      Last  : Natural;
   begin
      GNAT.IO.Get_Line (Value, Last);
      return To_GString_From_String (Value (1 .. Last));
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Spacing : Positive := 1) is
   begin
      GNAT.IO.New_Line (Spacing);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is
   begin
      GNAT.IO.Put (X);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (C : GCharacter) is
      Convert_String   : constant GString := C & " ";
      Converted_String : constant String (1 .. 2) :=
        To_String (Convert_String);
   begin
      GNAT.IO.Put (Converted_String (1));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (S : GString) is
   begin
      GNAT.IO.Put (To_String (S));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : GString) is
   begin
      GNAT.IO.Put_Line (To_String (S));
   end Put_Line;

   ---------------------
   -- Put_Line_String --
   ---------------------

   procedure Put_Line_String (S : String)
   is
   begin
      GNAT.IO.Put_Line (S);
   end Put_Line_String;

   --------------------------
   -- Put_Line_Wide_String --
   --------------------------

   procedure Put_Line_Wide_String (S : Wide_String)
   is
   begin
      Put_Line (To_GString_From_Wide_String (S));
   end Put_Line_Wide_String;

end GWindows.GStrings.IO;
