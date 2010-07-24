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
