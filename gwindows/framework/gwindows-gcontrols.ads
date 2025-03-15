------------------------------------------------------------------------------
--                                                                          --
--                   GWINDOWS - Ada 95 RAD GUI Framework                    --
--                                                                          --
--                   G W I N D O W S . G C O N T R O L S                    --
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

with GWindows.Windows;
with GWindows.Base;

package GWindows.GControls is

   -------------------------------------------------------------------------
   --  GControl_Type
   -------------------------------------------------------------------------
   --  GControl_Type is the parent Class of all GWindows GUI Widgets that
   --  are completely based on GWindows and therefore platform independant.
   --  The base GControl implements nothing but a blank panel and is a
   --  child of the Window_Type class. The Text parameter sets the text
   --  property for the control, but there is no GUI representation of that
   --  text property.

   type GControl_Type is new GWindows.Windows.Window_Type with private;
   type GControl_Access is access all GControl_Type;
   type Pointer_To_GControl_Class is access all GControl_Type'Class;

   -------------------------------------------------------------------------
   --  GControl_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out GControl_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      All_Keys   : in     Boolean;
      Container  : in     Boolean;
      Show       : in     Boolean;
      Is_Dynamic : in     Boolean);
   --  Create GControl

   -------------------------------------------------------------------------
   --  GControl_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   --  GControl_Type is a child of GWindow_Type for that reason the
   --  create methods of GWindow_Type are also available to
   --  GControl_Types. These create methods should not be used even
   --  though they may be functional. The same behavior can be created
   --  by creating the GControl_Type or any of its children as a child
   --  of a window and setting the Dock property of the control to
   --  Fill.

private

   type GControl_Type is new GWindows.Windows.Window_Type with null record;

end GWindows.GControls;
