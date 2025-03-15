------------------------------------------------------------------------------
--                                                                          --
--                                splitbar                                  --
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
