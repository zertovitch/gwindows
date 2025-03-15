------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . C O M B O _ B O X E S .G E N E R I C _ E N U M E R A L --
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
   type Enumeral_Type is (<>);
package GWindows.Combo_Boxes.Generic_Enumeral is

   -------------------------------------------------------------------------
   --  Combo_Box_Type
   -------------------------------------------------------------------------

   type Combo_Box_Type is new GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type
     with null record;
   type Combo_Box_Access is access all Combo_Box_Type;
   type Pointer_To_Combo_Box_Class is access all Combo_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Combo_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Combo      : in out Combo_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Combo Box

   ------------
   --  New Combo_Box_Type operations

   function Current (Combo : in Combo_Box_Type) return Enumeral_Type;

   procedure Current
     (Combo : in Combo_Box_Type;
      Item  : in Enumeral_Type);

end GWindows.Combo_Boxes.Generic_Enumeral;
