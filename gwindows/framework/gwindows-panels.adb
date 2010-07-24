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
