------------------------------------------------------------------------------
--                                                                          --
--                                splitbar                                  --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Cursors;

package body Splitbar is

   procedure Move_Windows (Splitbar : in out Splitbar_Type);

   function Is_Valid (Splitbar : in Splitbar_Type;
                      Point    : in GWindows.Types.Point_Type)
                     return Boolean;

   ------------
   -- Create --
   ------------

   procedure Create (Splitbar        : in out Splitbar_Type;
                     Parent          : in out Base_Window_Type'class;
                     First_Window    : in out Window_Type;
                     Second_Window   : in out Window_Type;
                     Splitbar_Pos    : in     Integer;
                     Width           :        Natural                := 5;
                     Refresh_At_End  :        Boolean                := False;
                     Direction       :        Direction_Type         := Hor;
                     Move_Line_Color :        Color_Type             := Black;
                     Move_Line_Width :        Natural                := 1)
   is

   begin
      Splitbar.First_Window_Handle := Handle (First_Window);
      Splitbar.Second_Window_Handle := Handle (Second_Window);
      Splitbar.Parent_Window_Handle := Handle (Parent);
      Splitbar.Refresh_At_End := Refresh_At_End;
      Splitbar.Direction := Direction;
      Splitbar.Move_Line_Brush := new Brush_Type;
      Splitbar.Bar_Width := Width;

      --  splitbar create
      if Direction = Hor then
         Create_As_Control (Splitbar,
                            Parent,
                            Left   => Splitbar_Pos,
                            Top    => 0,
                            Width  => Splitbar.Bar_Width,
                            Height => Height (Parent));

         --  move_line_window create
         Create_As_Control (Splitbar.Move_Line_Window,
                            Parent,
                            Left   => Splitbar_Pos,
                            Top    => 0,
                            Width  => Move_Line_Width,
                            Height => Height (Parent));
      else
         Create_As_Control (Splitbar,
                            Parent,
                            Left   => 0,
                            Top    => Splitbar_Pos,
                            Width  => GWindows.Base.Width (Parent),
                            Height => Splitbar.Bar_Width);

         --  move_line_window create
         Create_As_Control (Splitbar.Move_Line_Window,
                            Parent,
                            Left   => 0,
                            Top    => Splitbar_Pos,
                            Width  => GWindows.Base.Width (Parent),
                            Height => Move_Line_Width);
      end if;

      --  move_line create
      Visible (Splitbar.Move_Line_Window, False);
      GWindows.Drawing_Panels.Get_Canvas (Splitbar.Move_Line_Window,
                                          Splitbar.Canvas);
      Resize_Canvas (Splitbar.Move_Line_Window, True);
      Create_Solid_Brush (Splitbar.Move_Line_Brush.all, Move_Line_Color);
      Fill_Rectangle
        (Splitbar.Canvas,
         (0,
          0,
          GWindows.Base.Width (Base_Window_Type (Splitbar)),
          Height (Splitbar)),
         Splitbar.Move_Line_Brush.all);

      Move_Windows (Splitbar);
   end Create;

   --------------------
   -- event-handling --
   --------------------

   -------------------------------
   -- On_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      pragma Warnings (Off, Keys);

      Point : GWindows.Types.Point_Type := (X => X, Y => Y);
   begin
      Point := Point_To_Desktop (Window, Point);
      Window.Old_Mouse_Point := Point;
      Window.Is_Activated := True;
      Capture_Mouse (Window);
      On_Change_Cursor (Window);

      if Window.Refresh_At_End then

         --  redraw move_line
         Resize_Canvas (Window.Move_Line_Window,
                        Width (Window),
                        Height (Window));
         Get_Canvas (Window.Move_Line_Window, Window.Canvas);
         Fill_Rectangle (Window.Canvas,
                         (0, 0, Width (Window), Height (Window)),
                         Window.Move_Line_Brush.all);
         Visible (Window.Move_Line_Window, True);
      end if;
   end On_Left_Mouse_Button_Down;

   -----------------------------
   -- On_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_Left_Mouse_Button_Up
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      pragma Warnings (Off, Keys);

      Mouse_Point : GWindows.Types.Point_Type := (X => X, Y => Y);
   begin
      Window.Is_Activated := False;
      Release_Mouse;

      if Window.Refresh_At_End then
         Visible (Window.Move_Line_Window, False);
         Mouse_Point := Point_To_Desktop (Window, Mouse_Point);

         if Is_Valid (Window, Mouse_Point) = False then
            return;
         end if;

         Mouse_Point :=
           Point_To_Client (Window_From_Handle
                            (Window.Parent_Window_Handle).all,
                             Mouse_Point);

         if Window.Direction = Hor then
            Left (Window, Mouse_Point.X);
         else
            Top (Window, Mouse_Point.Y);
         end if;

         Window.Old_Mouse_Point := Mouse_Point;
         Move_Windows (Window);
      end if;
   end On_Left_Mouse_Button_Up;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move
     (Window : in out Splitbar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      pragma Warnings (Off, Keys);

      Mouse_Point : GWindows.Types.Point_Type := (X => X, Y => Y);
      use GWindows.Types;
   begin
      On_Change_Cursor (Window);

      Mouse_Point := Point_To_Desktop (Window,
                                       Mouse_Point);

      if Is_Valid (Window, Mouse_Point) = False then
         return;
      end if;

      if Window.Direction = Hor then
         if Window.Refresh_At_End then
            Mouse_Point :=
              Point_To_Client (Window_From_Handle
                               (Window.Parent_Window_Handle).all,
                               Mouse_Point);
            Left (Window.Move_Line_Window, Mouse_Point.X);
         else
            if
              Window.Is_Activated
              and
              Window.Old_Mouse_Point /= Mouse_Point
            then
               Left (Window,
                     Left (Window) +
                      (Mouse_Point.X - Window.Old_Mouse_Point.X));
               Window.Old_Mouse_Point := Mouse_Point;
            end if;
         end if;
      else
         if Window.Refresh_At_End then
            Mouse_Point :=
              Point_To_Client
              (Window_From_Handle (Window.Parent_Window_Handle).all,
               Mouse_Point);
            Top (Window.Move_Line_Window, Mouse_Point.Y);
         else
            if
              Window.Is_Activated and Window.Old_Mouse_Point /= Mouse_Point
            then
               Top (Window,
                    Top (Window) + (Mouse_Point.Y - Window.Old_Mouse_Point.Y));
               Window.Old_Mouse_Point := Mouse_Point;
            end if;
         end if;
      end if;

      Move_Windows (Window);
   end On_Mouse_Move;

   --------------
   -- on_paint --
   --------------

   procedure On_Paint (Window : in out Splitbar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Warnings (Off, Canvas);
      pragma Warnings (Off, Area);
   begin
      if Window.Direction = Hor then
         Top (Window, 0);
         Height (Window,
                 Height (Window_From_Handle
                         (Window.Parent_Window_Handle).all));
         Top (Window.Move_Line_Window, 0);
         Height (Window.Move_Line_Window,
                Height (Window_From_Handle (Window.Parent_Window_Handle).all));
      else
         Left (Window, 0);
         Width (Window,
                Width (Window_From_Handle (Window.Parent_Window_Handle).all));
         Left (Window.Move_Line_Window, 0);
         Width (Window.Move_Line_Window,
                Width (Window_From_Handle (Window.Parent_Window_Handle).all));
      end if;

      Move_Windows (Window);
   end On_Paint;

   -------------
   -- On_Move --
   -------------

   procedure On_Move
     (Window : in out Splitbar_Type;
      Left   : in     Integer;
      Top    : in     Integer)
   is
      pragma Warnings (Off, Left);
      pragma Warnings (Off, Top);
   begin
      Move_Windows (Window);
   exception
      when others =>
         null;
   end On_Move;

   -------------------
   -- on_change_cursor --
   -------------------

   procedure On_Change_Cursor (Window : in out Splitbar_Type) is
      use GWindows.Cursors;
   begin
      if Window.Direction = Hor then
         Set_Cursor (Load_System_Cursor (IDC_SIZEWE));
      else
         Set_Cursor (Load_System_Cursor (IDC_SIZENS));
      end if;
   end On_Change_Cursor;

   --------------------
   -- body functions --
   --------------------

   --------------
   -- is_valid --
   --------------

   function Is_Valid (Splitbar : in Splitbar_Type;
                      Point    : in GWindows.Types.Point_Type)
                    return Boolean
   is
      use GWindows.Types;
      Mouse_Point : Point_Type := Point; -- desktop-coordinates
   begin
      Mouse_Point := Point_To_Client (Window_From_Handle
                                      (Splitbar.Parent_Window_Handle).all,
                                      Mouse_Point);

      --  is the mouse_point within parent-window?
      if
        Mouse_Point.X < Splitbar.Bar_Width + 10
        or
        Mouse_Point.Y < Splitbar.Bar_Width + 10
      then
         return False;
      end if;

      if
        Mouse_Point.X >
        Client_Area_Width
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all) -
        Splitbar.Bar_Width - 10
      then
         return False;
      end if;

      if
        Mouse_Point.Y >
        Client_Area_Height
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all) -
        Splitbar.Bar_Width - 10
      then
         return False;
      end if;

      return True;
   end Is_Valid;

   ------------------
   -- Move_Windows --
   ------------------

   procedure Move_Windows (Splitbar : in out Splitbar_Type) is
      Point, Split_Point : GWindows.Types.Point_Type;
   begin
      if Splitbar.Direction = Hor then

         --  Window-positions correct for first_window
         Point.X :=
           Left (Window_From_Handle (Splitbar.First_Window_Handle).all) +
           Width (Window_From_Handle (Splitbar.First_Window_Handle).all);

         Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all, Point);

         Split_Point.X := Left (Splitbar);

         Split_Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all,
            Split_Point);

         Width (Window_From_Handle (Splitbar.First_Window_Handle).all,
                Width (Window_From_Handle (Splitbar.First_Window_Handle).all) +
                (Split_Point.X - Point.X));

         --  ... second_window
         Point.X :=
           Left (Window_From_Handle (Splitbar.Second_Window_Handle).all);

         Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all, Point);

         Split_Point.X := Left (Splitbar) + Splitbar.Bar_Width;

         Split_Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all,
            Split_Point);

         Width (Window_From_Handle (Splitbar.Second_Window_Handle).all,
                Width (Window_From_Handle
                       (Splitbar.Second_Window_Handle).all) +
                (Point.X - Split_Point.X));
      else
         --  Window-position correct for first_window
         Point.Y :=
           Top (Window_From_Handle (Splitbar.First_Window_Handle).all) +
           Height (Window_From_Handle (Splitbar.First_Window_Handle).all);
         Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all, Point);

         Split_Point.Y := Top (Splitbar);

         Split_Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all,
            Split_Point);

         Height
           (Window_From_Handle (Splitbar.First_Window_Handle).all,
            Height (Window_From_Handle (Splitbar.First_Window_Handle).all) +
            (Split_Point.Y - Point.Y));

         --  ... second_window
         Point.Y :=
           Top (Window_From_Handle (Splitbar.Second_Window_Handle).all);

         Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all, Point);

         Split_Point.Y := Top (Splitbar) + Splitbar.Bar_Width;

         Split_Point := Point_To_Desktop
           (Window_From_Handle (Splitbar.Parent_Window_Handle).all,
            Split_Point);

         Height
           (Window_From_Handle (Splitbar.Second_Window_Handle).all,
            Height (Window_From_Handle (Splitbar.Second_Window_Handle).all) +
            (Point.Y - Split_Point.Y));
      end if;

      Dock_Children
        (Window_Type (Window_From_Handle (Splitbar.Parent_Window_Handle).all));

   exception
      when others =>
         null;
   end Move_Windows;

end Splitbar;
