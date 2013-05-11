------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--          G W I N D O W S . G C O N T R O L S . S I Z E _ B A R S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2013 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Cursors;
with GWindows.Drawing_Objects;

package body GWindows.GControls.GSize_Bars is

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out GSize_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Location   : in     GWindows.Base.Dock_Type;
      Text       : in     GString                              := "";
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 3;
      Height     : in     Integer                              := 3;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      use type GWindows.Base.Dock_Type;
   begin
      GWindows.GControls.Create
        (GWindows.GControls.GControl_Type (Window),
         Parent     => Parent,
         Text       => Text,
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         All_Keys   => True,
         Container  => True,
         Show       => Show,
         Is_Dynamic => Is_Dynamic);

      Dock (Window, Location);

      if
        Location = GWindows.Base.At_Top
        or
        Location = GWindows.Base.At_Bottom
      then
         Default_Standard_Cursor (Window, GWindows.Cursors.IDC_SIZENS);
         Window.Minimum := Height;
      else
         Default_Standard_Cursor (Window, GWindows.Cursors.IDC_SIZEWE);
         Window.Minimum := Width;
      end if;

      Window.Last_Size := Window.Minimum;
   end Create;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint (Window : in out GSize_Bar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Warnings (Off, Window);
      pragma Warnings (Off, Area);

      use GWindows.Drawing_Objects;
      use GWindows.Drawing;
      use GWindows.Colors;
      use GWindows.Base;

      F  : Font_Type;
      M  : Font_Metrics_Type;
      A  : Integer := 0;
      TS : GWindows.Types.Size_Type;
   begin
      if Text (Window) /= "" then
         Background_Mode (Canvas, Transparent);
         Text_Color (Canvas, Window.Text_Color);

         Get_Font (Window, F);

         Select_Object (Canvas, F);
         M := Current_Font_Metrics (Canvas);

         if Dock (Window) = At_Left then
            A := 90 * 10;
         elsif Dock (Window) = At_Right then
            A := 270 * 10;
         end if;

         if not Current_Font_Is_True_Type (Canvas) then
            Create_Font (F,
                         "Arial",
                         M.Height,
                         Angle => A);
            Select_Object (Canvas, F);
         elsif A > 0 then
            Create_Font (F,
                         Current_Font_Name (Canvas),
                         M.Height,
                         Angle => A);
            Select_Object (Canvas, F);
         end if;

         TS := Text_Output_Size (Canvas, Text (Window));

         if Dock (Window) = At_Left then
            Put (Canvas,
                 Width (Window) / 2 - TS.Height / 2, 3 + TS.Width,
                 Text (Window));
         elsif Dock (Window) = At_Right then
            Put (Canvas,
                 Width (Window) / 2 + TS.Height / 2, 3,
                 Text (Window));
         else
            Put (Canvas,
                 3, Height (Window) / 2 - TS.Height / 2,
                 Text (Window));
         end if;
      end if;
   end On_Paint;

   -------------------------------
   -- On_Left_Mouse_Button_Down --
   -------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Keys);

      use GWindows.Base;
      use GWindows.Types;

      P  : constant Pointer_To_Base_Window_Class :=
        Controlling_Parent (Window);
      G  : constant Pointer_To_Base_Window_Class :=
        Parent (Parent (Window).all);
      W  : constant Point_Type := Point_To_Desktop (Window, (Left (Window),
                                                             Top (Window)));
      O  : constant Point_Type := Point_To_Client (P.all, W);
   begin
      Window.In_Size := True;

      if G /= null and not Window.Max_Set then
         if
           Dock (Window) = GWindows.Base.At_Top
           or
           Dock (Window) = GWindows.Base.At_Bottom
         then
            Window.Maximum := Client_Area_Height (G.all);
         else
            Window.Maximum := Client_Area_Width (G.all);
         end if;
      end if;

      if not Window.Live_Resize then
         Window.Bar := new Move_Bar_Type;

         Window.Bar.GSize_Bar_Parent := Window'Unchecked_Access;

         if
           Dock (Window) = GWindows.Base.At_Top
           or
           Dock (Window) = GWindows.Base.At_Bottom
         then
            Create (Window.Bar.all, P.all, "", O.X, O.Y,
                    Width (Window),
                    Window.Bar_Size,
                    All_Keys   => False,
                    Container  => False,
                    Show       => False,
                    Is_Dynamic => True);
         else
            Create (Window.Bar.all, P.all, "", O.X, O.Y,
                    Window.Bar_Size,
                    Height (Window),
                    All_Keys   => False,
                    Container  => False,
                    Show       => False,
                    Is_Dynamic => True);
         end if;

      end if;

      Capture_Mouse (Window);
   end On_Left_Mouse_Button_Down;

   -----------------------------
   -- On_Left_Mouse_Button_Up --
   -----------------------------

   procedure On_Left_Mouse_Button_Up
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   begin
      if Window.In_Size then

         if not Window.Live_Resize then
            if Window.Bar /= null then
               Close (Window.Bar.all);
            end if;

            Window.Live_Resize := True;
            On_Mouse_Move (Window, X, Y, Keys);
            Window.Live_Resize := False;
            On_Bar_Moved (GSize_Bar_Type'Class (Window));
         end if;

         Window.In_Size := False;

         GWindows.Base.Release_Mouse;
      end if;
   end On_Left_Mouse_Button_Up;

   -------------------
   -- On_Mouse_Move --
   -------------------

   procedure On_Mouse_Move (Window : in out GSize_Bar_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, Keys);

      use GWindows.Base;
   begin
      if Window.In_Size and (X /= 0 and Y /= 0) then
         declare
            use GWindows.Windows;
            use GWindows.Types;

            P   : GWindows.Windows.Window_Type renames
              GWindows.Windows.Window_Type (Parent (Window).all);
            W   : constant Natural := Width (P);
            H   : constant Natural := Height (P);
            NS  : Integer;
            PC  : constant Pointer_To_Base_Window_Class :=
              Controlling_Parent (Window);
            WC  : constant Point_Type := Point_To_Desktop (Window, (X, Y));
            OC  : Point_Type := Point_To_Client (PC.all, WC);
         begin
            if Dock (Window) = At_Right then
               NS := W + X;

               if Window.Live_Resize then
                  if NS < Window.Minimum then
                     NS := Window.Minimum;
                  elsif NS > Window.Maximum then
                     NS := Window.Maximum;
                  end if;

                  if NS /= W then
                     Width (P, NS);
                     On_Bar_Moved (GSize_Bar_Type'Class (Window));
                  end if;
               else
                  if NS < Window.Minimum then
                     OC := Point_To_Desktop (P, (Window.Minimum, 0));
                     OC.X := OC.X - Width (Window);
                     OC := Point_To_Client (PC.all, OC);
                  elsif NS > Window.Maximum then
                     OC := Point_To_Desktop (P, (Window.Maximum, 0));
                     OC.X := OC.X - Width (Window);
                     OC := Point_To_Client (PC.all, OC);
                  end if;

                  Left (Window.Bar.all, OC.X);
               end if;

            elsif Dock (Window) = At_Left then
               NS := W - X;

               if Window.Live_Resize then
                  if NS < Window.Minimum then
                     NS := Window.Minimum;
                  elsif NS > Window.Maximum then
                     NS := Window.Maximum;
                  end if;

                  if NS /= W then
                     Width (P, NS);
                     On_Bar_Moved (GSize_Bar_Type'Class (Window));
                  end if;
               else
                  if NS < Window.Minimum then
                     OC := Point_To_Desktop (Window, (Left (Window), 0));
                     OC.X := OC.X + W - Window.Minimum;
                     OC := Point_To_Client (PC.all, OC);
                  elsif NS > Window.Maximum then
                     OC := Point_To_Desktop (Window, (Left (Window), 0));
                     OC.X := OC.X + W - Window.Maximum;
                     OC := Point_To_Client (PC.all, OC);
                  end if;

                  Left (Window.Bar.all, OC.X);
               end if;
            elsif Dock (Window) = At_Bottom then
               NS := H + Y;

               if Window.Live_Resize then
                  if NS < Window.Minimum then
                     NS := Window.Minimum;
                  elsif NS > Window.Maximum then
                     NS := Window.Maximum;
                  end if;

                  if NS /= H then
                     Height (P, NS);
                     On_Bar_Moved (GSize_Bar_Type'Class (Window));
                  end if;
               else
                  if NS < Window.Minimum then
                     OC := Point_To_Desktop (Window, (0, Window.Minimum));
                     OC.Y := OC.Y - H - Height (Window);
                     OC := Point_To_Client (PC.all, OC);
                  elsif NS > Window.Maximum then
                     OC := Point_To_Desktop (Window, (0, Window.Maximum));
                     OC.Y := OC.Y - H - Height (Window);
                     OC := Point_To_Client (PC.all, OC);
                  end if;

                  Top (Window.Bar.all, OC.Y);
               end if;

            elsif Dock (Window) = At_Top then
               NS := H - Y;

               if Window.Live_Resize then
                  if NS < Window.Minimum then
                     NS := Window.Minimum;
                  elsif NS > Window.Maximum then
                     NS := Window.Maximum;
                  end if;

                  if NS /= H then
                     Height (P, NS);
                     On_Bar_Moved (GSize_Bar_Type'Class (Window));
                  end if;
               else
                  if NS < Window.Minimum then
                     OC := Point_To_Desktop (Window, (0, 0));
                     OC.Y := OC.Y + H - Window.Minimum;
                     OC := Point_To_Client (PC.all, OC);
                  elsif NS > Window.Maximum then
                     OC := Point_To_Desktop (Window, (0, 0));
                     OC.Y := OC.Y + H - Window.Maximum;
                     OC := Point_To_Client (PC.all, OC);
                  end if;

                  Top (Window.Bar.all, OC.Y);
               end if;
            end if;
         end;

         if not Window.Live_Resize then
            Visible (Window.Bar.all);
         end if;
      end if;
   end On_Mouse_Move;

   ---------------------------------------
   -- On_Left_Mouse_Button_Double_Click --
   ---------------------------------------

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Keys);

      use GWindows.Base;
      use GWindows.Windows;

      P  : GWindows.Windows.Window_Type renames
        GWindows.Windows.Window_Type (Parent (Window).all);
      W  : constant Natural := Width (P);
      H  : constant Natural := Height (P);
   begin
      if Dock (Window) = At_Left or Dock (Window) = At_Right then
         if W <= Window.Minimum then
            Width (P, Window.Last_Size);
         else
            Window.Last_Size := W;
            Width (P, Window.Minimum);
         end if;
      else
         if H <= Window.Minimum then
            Height (P, Window.Last_Size);
         else
            Window.Last_Size := H;
            Height (P, Window.Minimum);
         end if;
      end if;

      On_Bar_Moved (GSize_Bar_Type'Class (Window));
   end On_Left_Mouse_Button_Double_Click;

   ------------------
   -- Maximum_Size --
   ------------------

   procedure Maximum_Size (Window : in out GSize_Bar_Type; Value : in Natural)
   is
   begin
      Window.Max_Set := True;
      Window.Maximum := Value;
   end Maximum_Size;

   function Maximum_Size (Window : in GSize_Bar_Type) return Natural
   is
   begin
      return Window.Maximum;
   end Maximum_Size;

   ------------------
   -- Minimum_Size --
   ------------------

   procedure Minimum_Size (Window : in out GSize_Bar_Type; Value : in Natural)
   is
   begin
      Window.Minimum := Value;
   end Minimum_Size;

   function Minimum_Size (Window : in GSize_Bar_Type) return Natural
   is
   begin
      return Window.Minimum;
   end Minimum_Size;

   -------------------
   --  On_Bar_Moved --
   -------------------

   procedure On_Bar_Moved (Window : in out GSize_Bar_Type)
   is
      use GWindows.Base;
      use GWindows.Windows;

      Grand_Parent : constant Pointer_To_Base_Window_Class :=
        Parent (Parent (Window).all);

   begin
      Fire_On_Bar_Moved (Window);

      if Grand_Parent /= null then
         if Grand_Parent.all in Window_Type'Class then
            On_Size (Pointer_To_Window_Class (Grand_Parent).all,
                     Width (Pointer_To_Window_Class (Grand_Parent).all),
                     Height (Pointer_To_Window_Class (Grand_Parent).all));
         end if;
      end if;
   end On_Bar_Moved;

   procedure On_Bar_Moved_Handler (Window  : in out GSize_Bar_Type;
                                   Handler : in GWindows.Base.Action_Event)
   is
   begin
      Window.On_Bar_Moved_Event := Handler;
   end On_Bar_Moved_Handler;

   procedure Fire_On_Bar_Moved (Window : in out GSize_Bar_Type)
   is
      use GWindows.Base;
   begin
      if Window.On_Bar_Moved_Event /= null then
         Window.On_Bar_Moved_Event (Base_Window_Type'Class (Window));
      end if;
   end Fire_On_Bar_Moved;

   -----------------
   -- Live_Resize --
   -----------------

   procedure Live_Resize (Window : in out GSize_Bar_Type;
                          Value  : in Boolean := True)
   is
   begin
      Window.Live_Resize := Value;
   end Live_Resize;

   function Live_Resize (Window : in GSize_Bar_Type) return Boolean
   is
   begin
      return Window.Live_Resize;
   end Live_Resize;

   -------------------
   -- Move_Bar_Size --
   -------------------

   procedure Move_Bar_Size (Window : in out GSize_Bar_Type; Value : in Natural)
   is
   begin
      Window.Bar_Size := Value;
   end Move_Bar_Size;

   function Move_Bar_Size (Window : in GSize_Bar_Type) return Natural
   is
   begin
      return Window.Bar_Size;
   end Move_Bar_Size;

   --------------------
   -- Move_Bar_Color --
   --------------------

   procedure Move_Bar_Color (Window : in out GSize_Bar_Type;
                             Value  : in GWindows.Colors.Color_Type)
   is
   begin
      Window.Bar_Color := Value;
   end Move_Bar_Color;

   function Move_Bar_Color (Window : in GSize_Bar_Type)
                           return GWindows.Colors.Color_Type
   is
   begin
      return Window.Bar_Color;
   end Move_Bar_Color;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint (Window : in out Move_Bar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Warnings (Off, Window);
      pragma Warnings (Off, Area);

      use GWindows.Drawing_Objects;
      use GWindows.Drawing;

      Brush : Brush_Type;
   begin
      Create_Solid_Brush (Brush, Move_Bar_Color (Window.GSize_Bar_Parent.all));
      Fill_Rectangle (Canvas, (0, 0, Width (Window), Height (Window)), Brush);
   end On_Paint;

   ----------------
   -- Text_Color --
   ----------------

   procedure Text_Color (Window : in out GSize_Bar_Type;
                         Color  : in     GWindows.Colors.Color_Type)
   is
   begin
      Window.Text_Color := Color;
   end Text_Color;

   function Text_Color (Window : in GSize_Bar_Type)
                       return GWindows.Colors.Color_Type
   is
   begin
      return Window.Text_Color;
   end Text_Color;

end GWindows.GControls.GSize_Bars;
