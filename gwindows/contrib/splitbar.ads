------------------------------------------------------------------------------
--                                                                          --
--                                splitbar                                  --
--                                                                          --
--                                 S p e c                                  --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

--  A splitbar is a pair of windows with a draggable bar between them.


with GWindows.Windows; use GWindows.Windows;
with GWindows.Types;
with GWindows.Drawing;
with GWindows.Base; use GWindows.Base;
with GWindows.Drawing_Panels; use GWindows.Drawing_Panels;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Colors; use GWindows.Colors;

package Splitbar is

   type Splitbar_Type is new Window_Type with private;

   type Direction_Type is (Hor, Ver);

   procedure Create (Splitbar        : in out Splitbar_Type;
                     Parent          : in out Base_Window_Type'class;
                     First_Window    : in out Window_Type;
                     Second_Window   : in out Window_Type;
                     Splitbar_Pos    : in     Integer;
                     Width           :        Natural                := 5;
                     Refresh_At_End  :        Boolean                := False;
                     Direction       :        Direction_Type         := Hor;
                     Move_Line_Color :        Color_Type             := Black;
                     Move_Line_Width :        Natural                := 1);
   --  Create a splitbar

   --------------------
   -- Event-Handling --
   --------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Mouse_Move
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Paint (Window : in out Splitbar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);

   procedure On_Move
     (Window : in out Splitbar_Type;
      Left   : in     Integer;
      Top    : in     Integer);

   procedure On_Change_Cursor (Window : in out Splitbar_Type);

private

   type Brush_Access is access all Brush_Type;

   type Splitbar_Type is new Window_Type with
      record
         Bar_Width            : Natural;
         Move_Line_Brush      : Brush_Access;
         First_Window_Handle  : GWindows.Types.Handle;
         Second_Window_Handle : GWindows.Types.Handle;
         Parent_Window_Handle : GWindows.Types.Handle;
         Move_Line_Window     : Drawing_Panel_Type;
         Canvas               : Drawing_Canvas_Type;
         Is_Activated         : Boolean := False;
         Old_Mouse_Point      : GWindows.Types.Point_Type;
         Refresh_At_End       : Boolean := False;
         Direction            : Direction_Type;
      end record;

end Splitbar;
