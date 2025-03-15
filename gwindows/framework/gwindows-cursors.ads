------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . C U R S O R S                      --
--                                                                          --
--                                 S p e c                                  --
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

with Interfaces.C;

with GWindows.Types;

package GWindows.Cursors is

   type Cursor_Type is new Interfaces.C.long;

   --  System cursor constants

   IDC_ARROW                  : constant := 32512;
   --  Standard cursor: Arrow
   IDC_IBEAM                  : constant := 32513;
   --  Standard cursor: I-Beam
   IDC_WAIT                   : constant := 32514;
   --  Standard cursor: Hourglass
   IDC_CROSS                  : constant := 32515;
   --  Standard cursor: Cross
   IDC_UPARROW                : constant := 32516;
   --  Standard cursor: Up arrow
   IDC_SIZE                   : constant := 32640;
   --  Standard cursor: Resize
   IDC_ICON                   : constant := 32641;
   --  Standard cursor: Icon
   IDC_SIZENWSE               : constant := 32642;
   --  Standard cursor: Northwest-southeast size
   IDC_SIZENESW               : constant := 32643;
   --  Standard cursor: Northeast-southwest size
   IDC_SIZEWE                 : constant := 32644;
   --  Standard cursor: West-east size
   IDC_SIZENS                 : constant := 32645;
   --  Standard cursor: North-south size
   IDC_SIZEALL                : constant := 32646;
   --  Standard cursor: Size all
   IDC_NO                     : constant := 32648;
   --  Standard cursor: Slashed circle
   IDC_HAND                   : constant := 32649;
   --  Standard cursor: Hand with pointing finger
   IDC_APPSTARTING            : constant := 32650;
   --  Standard cursor: App starting
   IDC_HELP                   : constant := 32651;
   --  Standard cursor: Help

   function Load_System_Cursor (Cursor : Integer) return Cursor_Type;

   function Load_Cursor_From_File (File_Name : GString) return Cursor_Type;

   function Load_Cursor (Name : GString) return Cursor_Type;
   --  Load cursor resource
   --  To specify a numeric resource use #XXXX where XXXX is the resource ID

   procedure Set_Cursor_Position (X, Y : Integer);
   --  Sets cursor at position x, y

   function Get_Cursor_Position return GWindows.Types.Point_Type;

   procedure Show_Cursor;

   procedure Hide_Cursor;

   procedure Clip_Cursor (Area : GWindows.Types.Rectangle_Type);
   --  Restrict cursor to area

   function Get_Cursor_Clip return GWindows.Types.Rectangle_Type;
   --  Returns area cursor is restricted to

   procedure Release_Cursor;
   --  Release cursor from clip restriction

   procedure Set_Cursor (Cursor : Cursor_Type);
   --  Set cursor to cursor

   procedure Start_Wait_Cursor;
   --  Set the cursor to the wait cursor

   procedure End_Wait_Cursor;
   --  Restore the cursor to the arrow cursor

end GWindows.Cursors;
