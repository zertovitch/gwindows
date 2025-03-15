------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        S O U R C E _ B U F F E R                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Source_Buffer is

   type Source_Buffer_Type is tagged private;

   procedure Put (In_Buffer : in out Source_Buffer_Type;
                  Text      : in     String);
   --  Add text to the source buffer

   procedure Put_Line (In_Buffer : in out Source_Buffer_Type;
                       Text      : in     String);
   --  Add text to the source buffer plus a new line

   procedure Put_Comment (In_Buffer : in out Source_Buffer_Type;
                          Text      : in     String);
   --  Adds a comment to the source buffer and breaks it in to multiple
   --  lines as needed.

   procedure New_Line (In_Buffer : in out Source_Buffer_Type);
   --  Add a newline to the source buffer

   procedure Indent (In_Buffer : in out Source_Buffer_Type);
   --  Add indent to source buffer

   procedure Line_Indent (In_Buffer : in out Source_Buffer_Type);
   --  Indent line in source buffer

   procedure Set_Indent (In_Buffer : in out Source_Buffer_Type;
                         Spaces    : in     Natural            := 3);
   --  Sets the number of spaces representing an indent level

   procedure Increase_Indent (In_Buffer : in out Source_Buffer_Type);
   procedure Decrease_Indent (In_Buffer : in out Source_Buffer_Type);
   --  Adjust indentation of text added to source buffer

   procedure Read (From_File : in     Ada.Text_IO.File_Type;
                   To_Buffer : in out Source_Buffer_Type);
   --  Loads a buffer from a file

   procedure Write (From_Buffer : in     Source_Buffer_Type;
                    To_Buffer   : in out Source_Buffer_Type);
   --  Output buffer on to another buffer

   procedure Write (From_Buffer : in     Source_Buffer_Type;
                    To_Output   : in out Ada.Text_IO.File_Type);
   --  Output buffer to file

   procedure Write (From_Buffer : in Source_Buffer_Type);
   --  Output buffer to stdout

   procedure Clear (Buffer : in out Source_Buffer_Type);
   --  Clear buffer

   generic
      with procedure Write_Line (S : String);
   procedure Write_Buffer (From_Buffer : in Source_Buffer_Type);

private
   type Buf_Record;
   type Pointer_To_Buf_Record is access all Buf_Record;

   type Buf_Record is
      record
         Line : Ada.Strings.Unbounded.Unbounded_String;
         Next : Pointer_To_Buf_Record := null;
      end record;

   type Source_Buffer_Type is tagged
      record
         Top          : Pointer_To_Buf_Record := null;
         Current      : Pointer_To_Buf_Record := new Buf_Record;
         Indent_Level : Natural := 0;
         Indent_Size  : Natural := 3;
         Line_Start   : Boolean := True;
      end record;

end Source_Buffer;
