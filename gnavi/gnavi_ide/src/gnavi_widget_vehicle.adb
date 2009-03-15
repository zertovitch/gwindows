------------------------------------------------------------------------------
--                                                                          --
--         GNAVI - The GWINDOWS Rapid Application Development Tool          --
--                                                                          --
--                G N A V I _ W I D G E T _ V E H I C L E                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 1999, 2000, 2001 David Botton                 --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Key_States;
with GWindows.Colors;
with GWindows.Drawing_Objects;

package body GNAVI_Widget_Vehicle is

   procedure Widget_Resize
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer;
      Height : in     Integer);
     --  Capture internal resizing of a widget to resize its vehicle

   procedure Widget_Resize
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Width  : in     Integer;
      Height : in     Integer)
   is
      use GWindows.Base;

      Vehicle : GNAVI_Widget_Vehicle_Type renames
        GNAVI_Widget_Vehicle_Type (Parent (Window).all);
   begin
      if Vehicle.Show_Handles = False then
         Size (Parent (Window).all, Size (Window));
      else
         Size (Parent (Window).all,
               GWindows.Base.Width (Window) + Handle_Size * 2,
               GWindows.Base.Height (Window) + Handle_Size * 2);
      end if;
   end Widget_Resize;


   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Window : in out GNAVI_Widget_Vehicle_Type) is
   begin
      if Window.Show_Handles = False then
         Window.Show_Handles := True;

         Move (Window, Left (Window) - Handle_Size,
               Top (Window) - Handle_Size);
         GWindows.Base.Move (Window.Widget.all, Handle_Size, Handle_Size);
         Move (Window.Blanket.all, Handle_Size, Handle_Size);

         Size (Window, Width (Window) + Handle_Size * 2,
               Height (Window) + Handle_Size * 2);

         Redraw (Window, False);
      end if;
   end On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Window : in out GNAVI_Widget_Vehicle_Type) is
   begin
      if Window.Show_Handles = True and not Window.Inner_Edit then
         Window.Show_Handles := False;

         Move (Window, Left (Window) + Handle_Size,
               Top (Window) + Handle_Size);
         GWindows.Base.Move (Window.Widget.all, 0, 0);
         Move (Window.Blanket.all, 0, 0);

         Size (Window, Width (Window) - Handle_Size * 2,
               Height (Window) - Handle_Size * 2);

         GWindows.Base.Redraw (Parent (Window).all, False);
      end if;
   end On_Lost_Focus;

   ----------------
   -- Set_Widget --
   ----------------

   procedure Set_Widget
     (Vehicle : in out GNAVI_Widget_Vehicle_Type;
      Widget  : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use GWindows.Windows;
   begin
      Vehicle.Widget := Widget;

      if Widget.all in Window_Type'Class then
        On_Size_Handler (Window_Type (Widget.all), Widget_Resize'Access);
      end if;

      GWindows.Base.Move (Widget.all, 0, 0);
      GWindows.Base.Size (Widget.all,
                          Width (Vehicle), Height (Vehicle));

      Vehicle.Blanket := new GNAVI_Widget_Blanket_Type;

      Vehicle.Blanket.Vehicle := Vehicle'Unchecked_Access;

      Create_As_Control (Vehicle.Blanket.all, Vehicle, "",
                         0, 0,
                         Client_Area_Width (Vehicle),
                         Client_Area_Height (Vehicle),
                         Is_Dynamic => True);
      Order (Vehicle.Blanket.all, GWindows.Base.Top);
   end Set_Widget;

   -------------------------
   -- On_Erase_Background --
   -------------------------

   procedure On_Erase_Background
     (Window : in out GNAVI_Widget_Vehicle_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   begin
      null;
   end On_Erase_Background;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint
     (Window : in out GNAVI_Widget_Vehicle_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Drawing;
      use GWindows.Drawing_Objects;

      Mid_Y : Integer := Client_Area_Height (Window) / 2;
      Mid_X : Integer := Client_Area_Width (Window) / 2;

      Handle_Brush : Brush_Type;
   begin
      if Window.Show_Handles then
         if Window.Inner_Edit then
            Create_Solid_Brush (Handle_Brush, GWindows.Colors.Red);
            Select_Object (Canvas, Handle_Brush);

            --  Top_Left
            Ellipse (Canvas,
                     0,
                     0,
                     Handle_Size,
                     Handle_Size);

            --  Bottom_Left
            Ellipse (Canvas,
                     0,
                     Client_Area_Height (Window) - Handle_Size,
                     Handle_Size,
                     Client_Area_Height (Window));

            --  Top_Right
            Ellipse (Canvas,
                     Client_Area_Width (Window) - Handle_Size,
                     0,
                     Client_Area_Width (Window),
                     Handle_Size);

            --  Bottom_Right
            Ellipse (Canvas,
                     Client_Area_Width (Window) - Handle_Size,
                     Client_Area_Height (Window) - Handle_Size,
                     Client_Area_Width (Window),
                     Client_Area_Height (Window));

            --  Left
            Ellipse (Canvas,
                     0,
                     Mid_Y - Handle_Size / 2,
                     Handle_Size,
                     Mid_Y + Handle_Size / 2);

            --  Right
            Ellipse (Canvas,
                     Client_Area_Width (Window) - Handle_Size,
                     Mid_Y - Handle_Size / 2,
                     Client_Area_Width (Window),
                     Mid_Y + Handle_Size / 2);

            --  Top
            Ellipse
              (Canvas,
               Client_Area_Width (Window) - Mid_X - Handle_Size / 2,
               0,
               Client_Area_Width (Window) - Mid_X + Handle_Size / 2,
               Handle_Size);

            --  Bottom
            Ellipse
              (Canvas,
               Client_Area_Width (Window) - Mid_X - Handle_Size / 2,
               Client_Area_Height (Window) - Handle_Size,
               Client_Area_Width (Window) - Mid_X + Handle_Size / 2,
               Client_Area_Height (Window));
         else
            --  Top_Left
            Rectangle (Canvas,
                       0,
                       0,
                       Handle_Size,
                       Handle_Size);

            --  Bottom_Left
            Rectangle (Canvas,
                       0,
                       Client_Area_Height (Window) - Handle_Size,
                       Handle_Size,
                       Client_Area_Height (Window));

            --  Top_Right
            Rectangle (Canvas,
                       Client_Area_Width (Window) - Handle_Size,
                       0,
                       Client_Area_Width (Window),
                       Handle_Size);

            --  Bottom_Right
            Rectangle (Canvas,
                       Client_Area_Width (Window) - Handle_Size,
                       Client_Area_Height (Window) - Handle_Size,
                       Client_Area_Width (Window),
                       Client_Area_Height (Window));

            --  Left
            Rectangle (Canvas,
                       0,
                       Mid_Y - Handle_Size / 2,
                       Handle_Size,
                       Mid_Y + Handle_Size / 2);

            --  Right
            Rectangle (Canvas,
                       Client_Area_Width (Window) - Handle_Size,
                       Mid_Y - Handle_Size / 2,
                       Client_Area_Width (Window),
                       Mid_Y + Handle_Size / 2);

            --  Top
            Rectangle
              (Canvas,
               Client_Area_Width (Window) - Mid_X - Handle_Size / 2,
               0,
               Client_Area_Width (Window) - Mid_X + Handle_Size / 2,
               Handle_Size);

            --  Bottom
            Rectangle
              (Canvas,
               Client_Area_Width (Window) - Mid_X - Handle_Size / 2,
               Client_Area_Height (Window) - Handle_Size,
               Client_Area_Width (Window) - Mid_X + Handle_Size / 2,
               Client_Area_Height (Window));
         end if;
      end if;
   end On_Paint;

   -----------------------
   -- On_Character_Down --
   -----------------------

   procedure On_Character_Down
     (Window      : in out GNAVI_Widget_Vehicle_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
      use GWindows.Windows;
      use GWindows.Key_States;

   begin
      case Special_Key is

         when Up_Key =>
            if not Is_Key_Down (VK_SHIFT) then
               Top (Window, Top (Window) - 1);
            else
               declare
                  Fixed_Y : Integer := Height (Window) - 1;
               begin
                  Height (Window, Fixed_Y);

                  GWindows.Base.Height (Window.Widget.all,
                                        Fixed_Y - Handle_Size * 2);
                  Height (Window.Blanket.all,
                          Fixed_Y - Handle_Size * 2);
               end;
            end if;

            GWindows.Base.Redraw (Parent (Window).all, True);

         when Down_Key =>
            if not Is_Key_Down (VK_SHIFT) then
               Top (Window, Top (Window) + 1);
            else
               declare
                  Fixed_Y : Integer := Height (Window) + 1;
               begin
                  Height (Window, Fixed_Y);

                  GWindows.Base.Height (Window.Widget.all,
                                        Fixed_Y - Handle_Size * 2);
                  Height (Window.Blanket.all,
                          Fixed_Y - Handle_Size * 2);
               end;
            end if;

            GWindows.Base.Redraw (Parent (Window).all, True);

         when Left_Key =>
            if not Is_Key_Down (VK_SHIFT) then
               Left (Window, Left (Window) - 1);
            else
               declare
                  Fixed_X : Integer := Width (Window) - 1;
               begin
                  Width (Window, Fixed_X);
                  GWindows.Base.Width (Window.Widget.all,
                                       Fixed_X - Handle_Size * 2);
                  Width (Window.Blanket.all,
                         Fixed_X - Handle_Size * 2);
               end;
            end if;

            GWindows.Base.Redraw (Parent (Window).all, True);
         when Right_Key =>
            if not Is_Key_Down (VK_SHIFT) then
               Left (Window, Left (Window) + 1);
            else
               declare
                  Fixed_X : Integer := Width (Window) + 1;
               begin
                  Width (Window, Fixed_X);
                  GWindows.Base.Width (Window.Widget.all,
                                       Fixed_X - Handle_Size * 2);
                  Width (Window.Blanket.all,
                         Fixed_X - Handle_Size * 2);
               end;
            end if;

            GWindows.Base.Redraw (Parent (Window).all, True);
         when None =>
            if Value = GWindows.GCharacter'Val (9) then
               declare
                  use GWindows.Base;

                  Next_Win : GWindows.Base.Pointer_To_Base_Window_Class;
               begin
                  if Is_Key_Down (VK_SHIFT) then
                     Next_Win := Previous_Window (Window);
                  else
                     Next_Win := Next_Window (Window);
                  end if;

                  if Next_Win = null then
                     if Is_Key_Down (VK_SHIFT) then
                        Next_Win := Last_Window (Window);
                     else
                        Next_Win := First_Window (Window);
                     end if;
                  end if;


                  if Next_Win /= null then
                     GWindows.Base.Focus (Next_Win.all);
                  end if;
               end;
            end if;
         when others =>
            null;
      end case;
   end On_Character_Down;

   -------------------------------
   -- On_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Window.Current_Handle := None;

      Window.Size_Bounds := (Left (Window), Top (Window),
                             Left (Window) +  Width (Window),
                             Top (Window) + Height (Window));

      if X <= Extended_Handle_Size then
         if Y <= Extended_Handle_Size then
            Window.Current_Handle := Top_Left;
            Window.X_Offset := X;
            Window.Y_Offset := Y;
         elsif Y >= Height (Window) - Extended_Handle_Size then
            Window.Current_Handle := Bottom_Left;
            Window.X_Offset := X;
            Window.Y_Offset := Height (Window) - Y;
         elsif
           Y >= Height (Window) / 2 - Extended_Handle_Size
           and
           Y <= Height (Window) / 2 + Extended_Handle_Size
         then
            Window.Current_Handle := Left;
            Window.X_Offset := X;
            Window.Y_Offset := Y;
         end if;
      elsif X > Width (Window) - Extended_Handle_Size then
         if Y <= Extended_Handle_Size then
            Window.Current_Handle := Top_Right;
            Window.X_Offset := Width (Window) - X;
            Window.Y_Offset := Y;
         elsif Y >= Height (Window) - Extended_Handle_Size then
            Window.Current_Handle := Bottom_Right;
            Window.X_Offset := Width (Window) - X;
            Window.Y_Offset := Height (Window) - Y;
         elsif
           Y >= Height (Window) / 2 - Extended_Handle_Size
           and
           Y <= Height (Window) / 2 + Extended_Handle_Size
         then
            Window.Current_Handle := Right;
            Window.X_Offset := Width (Window) - X;
            Window.Y_Offset := Y;
         end if;
      elsif
        X >= Width (Window) / 2 - Extended_Handle_Size
        and
        X <= Width (Window) / 2 + Extended_Handle_Size
      then
         if Y <= Extended_Handle_Size then
            Window.Current_Handle := Top;
            Window.X_Offset := X;
            Window.Y_Offset := Y;
         elsif Y >= Height (Window) - Extended_Handle_Size then
            Window.Current_Handle := Bottom;
            Window.X_Offset := X;
            Window.Y_Offset := Height (Window) - Y;
         end if;
      end if;

      if Window.Current_Handle /= None then
         Capture_Mouse (Window);
      end if;
   end On_Left_Mouse_Button_Down;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      use GWindows.Base;
      Fixed_X : Integer := X + Window.X_Offset;
      Fixed_Y : Integer := Y + Window.Y_Offset;
      New_X   : Integer := Left (Window) + X - Window.X_Offset;
      New_Y   : Integer := Top (Window) + Y - Window.Y_Offset;
   begin
      if Window.Current_Handle /= None then
         if Fixed_X < Minimum_Width + Handle_Size * 2 then
            Fixed_X := Minimum_Width + Handle_Size * 2;
         end if;

         if Fixed_Y < Minimum_Height + Handle_Size * 2 then
            Fixed_Y := Minimum_Height + Handle_Size * 2;
         end if;

         if
           Window.Size_Bounds.Right - New_X <
           Minimum_Width + Handle_Size * 2
         then
            New_X := Window.Size_Bounds.Right -
              Minimum_Width - Handle_Size * 2;
         end if;

         if
           Window.Size_Bounds.Bottom - New_Y <
           Minimum_Height + Handle_Size * 2
         then
            New_Y := Window.Size_Bounds.Bottom -
              Minimum_Height - Handle_Size * 2;
         end if;

      end if;

      case Window.Current_Handle is

         when Top_Left =>
            Move (Window, New_X, New_Y);

            Size (Window,
                  Window.Size_Bounds.Right - Left (Window),
                  Window.Size_Bounds.Bottom - Top (Window));
            Size (Window.Widget.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);

            Redraw (Window, True);

         when Top =>
            Top (Window, New_Y);

            Size (Window,
                  Window.Size_Bounds.Right - Left (Window),
                  Window.Size_Bounds.Bottom - Top (Window));
            Size (Window.Widget.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);

            Redraw (Window, True);

         when Left =>
            Left (Window, New_X);

            Size (Window,
                  Window.Size_Bounds.Right - Left (Window),
                  Window.Size_Bounds.Bottom - Top (Window));
            Size (Window.Widget.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);

            Redraw (Window, True);

         when Bottom_Left =>
            Left (Window, New_X);

            Size (Window,
                  Window.Size_Bounds.Right - Left (Window),
                  Fixed_Y);
            Size (Window.Widget.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Fixed_Y - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Window.Size_Bounds.Right - Left (Window) - Handle_Size * 2,
                  Fixed_Y - Handle_Size * 2);

            Redraw (Window, True);

         when Top_Right =>
            Top (Window, New_Y);

            Size (Window,
                  Fixed_X,
                  Window.Size_Bounds.Bottom - Top (Window));
            Size (Window.Widget.all,
                  Fixed_X - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Fixed_X - Handle_Size * 2,
                  Window.Size_Bounds.Bottom - Top (Window) - Handle_Size * 2);

            Redraw (Window, True);

         when Right =>
            Width (Window, Fixed_X);
            Width (Window.Widget.all,
                  Fixed_X - Handle_Size * 2);
            Width (Window.Blanket.all,
                  Fixed_X - Handle_Size * 2);

            Redraw (Window, True);

         when Bottom_Right =>
            Size (Window, Fixed_X, Fixed_Y);

            Size (Window.Widget.all,
                  Fixed_X - Handle_Size * 2,
                  Fixed_Y - Handle_Size * 2);
            Size (Window.Blanket.all,
                  Fixed_X - Handle_Size * 2,
                  Fixed_Y - Handle_Size * 2);

            Redraw (Window, True);

         when Bottom =>
            Height (Window, Fixed_Y);

            Height (Window.Widget.all,
                    Fixed_Y - Handle_Size * 2);
            Height (Window.Blanket.all,
                    Fixed_Y - Handle_Size * 2);

            Redraw (Window, True);

         when others =>
            null;
      end case;
   end On_Mouse_Move;

   -----------------------------
   -- On_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_Left_Mouse_Button_Up
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      if Window.Current_Handle /= None then
         GWindows.Base.Release_Mouse;
      end if;

      Window.Current_Handle := None;
   end On_Left_Mouse_Button_Up;


   -------------------------
   -- On_Erase_Background --
   -------------------------

   procedure On_Erase_Background
     (Window : in out GNAVI_Widget_Blanket_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   begin
      null;
   end On_Erase_Background;


   -------------------------------
   -- On_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Focus (Window.Vehicle.all);
      Capture_Mouse (Window);
      Window.Track_Mouse := True;
      Window.X_Offset := X;
      Window.Y_Offset := Y;
   end On_Left_Mouse_Button_Down;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      use GWindows.Windows;
   begin
      if Window.Track_Mouse then
         Move (Window.Vehicle.all,
               Left (Window.Vehicle.all) + X - Window.X_Offset,
               Top (Window.Vehicle.all) + Y - Window.Y_Offset);
         GWindows.Base.Redraw (Parent (Window.Vehicle.all).all, True);
      end if;
   end On_Mouse_Move;

   -----------------------------
   -- On_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_Left_Mouse_Button_Up
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Window.Track_Mouse := False;
      GWindows.Base.Release_Mouse;
   end On_Left_Mouse_Button_Up;

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Window.Vehicle.Inner_Edit := True;
      GWindows.Base.Order (Window.Vehicle.Widget.all, GWindows.Base.Top);
      Redraw (Window.Vehicle.all, True);
   end On_Left_Mouse_Button_Double_Click;

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      Window.Inner_Edit := False;
      Order (Window.Blanket.all, GWindows.Base.Top);
      Focus (Window);
      Redraw (Window, True);
   end On_Left_Mouse_Button_Double_Click;

   -----------------
   -- Widget_Size --
   -----------------

   function Widget_Size (Vehicle : in GNAVI_Widget_Vehicle_Type)
                        return GWindows.Types.Size_Type
   is
   begin
      return GWindows.Base.Size (Vehicle.Widget.all);
   end Widget_Size;

   ---------------------
   -- Widget_Location --
   ---------------------

   function Widget_Location (Vehicle : in GNAVI_Widget_Vehicle_Type)
                            return GWindows.Types.Point_Type
   is
      use GWindows.Base;
      use GWindows.Types;

      Loc : GWindows.Types.Point_Type := Location (Vehicle);
   begin
      if Vehicle.Show_Handles then
         Loc := Loc + Size_Type'(Handle_Size, Handle_Size);
      end if;

      return Loc;
   end Widget_Location;

end GNAVI_Widget_Vehicle;
