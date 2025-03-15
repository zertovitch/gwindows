------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . P A N E L S                       --
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

package body GWindows.Panels is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   -----------------
   -- Border_Type --
   -----------------

   procedure Border_Type (Panel  : in out Panel_Type;
                          Border : in     Panel_Border_Type)
   is
   begin
      Panel.Panel_Border := Border;
   end Border_Type;

   function Border_Type (Panel : in Panel_Type) return Panel_Border_Type
   is
   begin
      return Panel.Panel_Border;
   end Border_Type;

   ----------------------
   -- Border_Thickness --
   ----------------------

   procedure Border_Thickness (Panel     : in out Panel_Type;
                               Thickness : in     Positive)
   is
   begin
      Panel.Panel_Border_Thickness := Thickness;
   end Border_Thickness;

   function Border_Thickness (Panel : in Panel_Type) return Positive
   is
   begin
      return Panel.Panel_Border_Thickness;
   end Border_Thickness;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint (Window : in out Panel_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Drawing;

      pragma Warnings (Off, Area);
   begin
      case Window.Panel_Border is
         when None =>
            null;
         when Sunken =>
            Rectangle_3D (Canvas, 0, 0,
                          Client_Area_Width (Window),
                          Client_Area_Height (Window),
                          Sunken,
                          Window.Panel_Border_Thickness);
         when Raised =>
            Rectangle_3D (Canvas, 0, 0,
                          Client_Area_Width (Window),
                          Client_Area_Height (Window),
                          Raised,
                          Window.Panel_Border_Thickness);
      end case;

      GWindows.Windows.On_Paint (GWindows.Windows.Window_Type (Window),
                                 Canvas, Area);
   end On_Paint;

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_As_Control (Window, Parent, "", Left, Top, Width, Height,
                         Show => Show,
                         Is_Dynamic => Is_Dynamic);
   end Create;

end GWindows.Panels;
