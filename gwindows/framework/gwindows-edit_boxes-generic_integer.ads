------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . E D I T _ B O X E S .G E N E R I C _ I N T E G E R     --
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

generic
   type Integer_Type is (<>);
package GWindows.Edit_Boxes.Generic_Integer is

   -------------------------------------------------------------------------
   --  Edit_Box_Type
   -------------------------------------------------------------------------

   type Edit_Box_Type is new GWindows.Edit_Boxes.Edit_Box_Type
     with private;
   type Edit_Box_Access is access all Edit_Box_Type;
   type Pointer_To_Edit_Box_Class is access all Edit_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Edit       : in out Edit_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Initial    : in     Integer_Type
      := Integer_Type'First;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Edit Box

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Override Event Methods
   -------------------------------------------------------------------------

   procedure On_Update (Edit : in out Edit_Box_Type);
   --  Validate user's entry

   -------------------------------------------------------------------------
   --  New Edit_Box_Type operations
   -------------------------------------------------------------------------

   function Current (Edit : in Edit_Box_Type) return Integer_Type;

   procedure Current (Edit : in out Edit_Box_Type; Item  : in Integer_Type);

private
   type Edit_Box_Type is new GWindows.Edit_Boxes.Edit_Box_Type with record
      Value : Integer_Type;
   end record;

end GWindows.Edit_Boxes.Generic_Integer;
