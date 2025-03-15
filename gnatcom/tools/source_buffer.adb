------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        S O U R C E _ B U F F E R                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--                  Copyright (C) 1999-2004 David Botton                    --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Source_Buffer is
   use Ada.Strings.Unbounded;

   procedure Free is
      new Ada.Unchecked_Deallocation (Buf_Record,
                                      Pointer_To_Buf_Record);
   -- Clear --

   procedure Clear (Buffer : in out Source_Buffer_Type) is
      This : Pointer_To_Buf_Record := Buffer.Top;
      That : Pointer_To_Buf_Record;
   begin
      if Buffer.Top = null then
         This := Buffer.Current;
      end if;

      while This /= null loop
         That := This.Next;
         Free (This);
         This := That;
      end loop;

      Buffer.Top := new Buf_Record;
      Buffer.Current := Buffer.Top;
   end Clear;

   -- Decrease_Indent --

   procedure Decrease_Indent (In_Buffer : in out Source_Buffer_Type) is
   begin
      In_Buffer.Indent_Level := In_Buffer.Indent_Level - 1;
   end Decrease_Indent;

   -- Increase_Indent --

   procedure Increase_Indent (In_Buffer : in out Source_Buffer_Type) is
   begin
      In_Buffer.Indent_Level := In_Buffer.Indent_Level + 1;
   end Increase_Indent;

   -- Indent --

   procedure Indent (In_Buffer : in out Source_Buffer_Type) is
      use Ada.Strings.Fixed;
   begin
      Put (In_Buffer, In_Buffer.Indent_Size * ' ');
   end Indent;

   -- Line_Indent --

   procedure Line_Indent (In_Buffer : in out Source_Buffer_Type) is
      use Ada.Strings.Fixed;
   begin
      In_Buffer.Line_Start := False;
      Put (In_Buffer, (In_Buffer.Indent_Level * In_Buffer.Indent_Size) * ' ');
   end Line_Indent;

   -- New_Line --

   procedure New_Line (In_Buffer : in out Source_Buffer_Type) is
   begin
      if In_Buffer.Top = null then
         In_Buffer.Top := In_Buffer.Current;
      end if;

      In_Buffer.Current.Next := new Buf_Record;
      In_Buffer.Current := In_Buffer.Current.Next;

      In_Buffer.Line_Start := True;
   end New_Line;

   -- Put --

   procedure Put
     (In_Buffer : in out Source_Buffer_Type;
      Text      : in String)
   is
   begin
      if In_Buffer.Line_Start then
         Line_Indent (In_Buffer);
      end if;

      Append (Source   => In_Buffer.Current.Line,
              New_Item => Text);
   end Put;

   -- Put_Line --

   procedure Put_Line
     (In_Buffer : in out Source_Buffer_Type;
      Text      : in String)
   is
   begin
      Put (In_Buffer, Text);
      New_Line (In_Buffer);
   end Put_Line;

   -- Put_Comment --

   procedure Put_Comment
     (In_Buffer : in out Source_Buffer_Type;
      Text      : in String)
   is
      Max_Len : constant Natural :=
        76 - (In_Buffer.Indent_Level * In_Buffer.Indent_Size);
      Doc     : Unbounded_String := To_Unbounded_String (Text);
      Section : Unbounded_String;
      Pos     : Natural;
   begin
      while Doc /= "" loop
         if Length (Doc) < Max_Len then
            Put_Line (In_Buffer, "--  " & To_String (Doc));
            Doc := To_Unbounded_String ("");
         else
            Section := To_Unbounded_String (Slice (Doc, 1, Max_Len));
            Pos := Index (Section, " ", Ada.Strings.Backward);
            if Pos = 0 then
               Put_Line (In_Buffer, "--  " & To_String (Section));
               Pos := 60;
            else
               Put_Line (In_Buffer,
                         "--  " & Slice (Section, 1, Pos - 1));
            end if;
            Delete (Doc, 1, Pos);
         end if;
      end loop;
   end Put_Comment;

   -- Set_Indent --

   procedure Set_Indent
     (In_Buffer : in out Source_Buffer_Type;
      Spaces    : in     Natural            := 3)
   is
   begin
      In_Buffer.Indent_Size := Spaces;
   end Set_Indent;

   -- Read --

   procedure Read (From_File : in     Ada.Text_IO.File_Type;
                   To_Buffer : in out Source_Buffer_Type)
   is
      use Ada.Text_IO;
   begin
      while not End_Of_File (From_File) loop
         declare
            Line : String (1 .. 256);
            Last : Natural;
         begin
            Get_Line (From_File, Line, Last);
            if Last > 0 then
               Put_Line (To_Buffer, Line (1 .. Last));
            end if;
         end;
      end loop;
   end Read;

   -- Write --

   procedure Write
     (From_Buffer : in     Source_Buffer_Type;
      To_Buffer   : in out Source_Buffer_Type)
   is
      This : Pointer_To_Buf_Record := From_Buffer.Top;
   begin
      while This /= null loop
         Put_Line (To_Buffer, To_String (This.Line));
         This := This.Next;
      end loop;
   end Write;

   -- Write --

   procedure Write
     (From_Buffer : in     Source_Buffer_Type;
      To_Output   : in out Ada.Text_IO.File_Type)
   is
      This : Pointer_To_Buf_Record := From_Buffer.Top;
   begin
      while This /= null loop
         Ada.Text_IO.Put_Line (To_Output, To_String (This.Line));
         This := This.Next;
      end loop;
   end Write;

   -- Write --

   procedure Write (From_Buffer : in Source_Buffer_Type) is
      This : Pointer_To_Buf_Record := From_Buffer.Top;
   begin
      while This /= null loop
         Ada.Text_IO.Put_Line (To_String (This.Line));
         This := This.Next;
      end loop;
   end Write;

   -- Write_Buffer --

   procedure Write_Buffer (From_Buffer : in Source_Buffer_Type)
   is
      This : Pointer_To_Buf_Record := From_Buffer.Top;
   begin
      while This /= null loop
         Write_Line (To_String (This.Line));
         This := This.Next;
      end loop;
   end Write_Buffer;

end Source_Buffer;
