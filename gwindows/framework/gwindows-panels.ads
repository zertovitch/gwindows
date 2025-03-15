------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . P A N E L S                       --
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

with GWindows.Base;
with GWindows.Types;
with GWindows.Drawing;
with GWindows.Windows;

package GWindows.Panels is

   -------------------------------------------------------------------------
   --  Panel_Type
   -------------------------------------------------------------------------

   type Panel_Type is new GWindows.Windows.Window_Type with private;
   type Panel_Access is access all Panel_Type;
   type Pointer_To_Panel_Class is access all Panel_Type'Class;

   -------------------------------------------------------------------------
   --  Panel_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create panel control

   -------------------------------------------------------------------------
   --  Panel_Type - Properties
   -------------------------------------------------------------------------

   type Panel_Border_Type is (None, Sunken, Raised);

   procedure Border_Type (Panel  : in out Panel_Type;
                          Border : in     Panel_Border_Type);
   function Border_Type (Panel : in Panel_Type) return Panel_Border_Type;

   procedure Border_Thickness (Panel     : in out Panel_Type;
                               Thickness : in     Positive);
   function Border_Thickness (Panel : in Panel_Type) return Positive;

   -------------------------------------------------------------------------
   --  Panel_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Paint (Window : in out Panel_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);
   --  Handles drawing border

private

   type Panel_Type is new GWindows.Windows.Window_Type with
      record
         Panel_Border           : Panel_Border_Type := None;
         Panel_Border_Thickness : Positive := 1;
      end record;

end GWindows.Panels;
