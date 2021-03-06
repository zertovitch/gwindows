------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id: templates_parser-debug.adb,v 1.1 2004/11/15 13:54:58 dbotton Exp $

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Templates_Parser.Debug is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   -----------
   -- Print --
   -----------

   procedure Print (T : in Tag) is

      procedure Print (T : in Tag; K : Natural);
      --  Print tag T, K is the indent level

      -----------
      -- Print --
      -----------

      procedure Print (T : in Tag; K : Natural) is
         Indent : constant String := K * ' ';
         N      : Tag_Node_Access := T.Data.Head;
      begin
         Put (Indent);
         Put_Line
           ("(N=" & Natural'Image (T.Data.Count)
            & ", Min=" & Natural'Image (T.Data.Min)
            & ", Max=" & Natural'Image (T.Data.Max)
            & ", Nested_Level=" & Natural'Image (T.Data.Nested_Level));

         while N /= null loop
            if N.Kind = Value then
               Put_Line (Indent & Indent & To_String (N.V));
            else
               Print (N.VS.all, K + 1);
            end if;
            N := N.Next;
         end loop;

         Put_Line (Indent & ")");
      end Print;

   begin
      Print (T, 1);
   end Print;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (Filename : in String) is
   begin
      Templates_Parser.Print_Tree (Filename);
   end Print_Tree;

end Templates_Parser.Debug;
