------------------------------------------------------------------------------
--                                                                          --
--                   GWINDOWS - Ada 95 RAD GUI Framework                    --
--                                                                          --
--         G W I N D O W S . G C O N T R O L S . D U R A T I O N            --
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

with GWindows.GStrings;
with GWindows.Drawing_Objects;
with GWindows.Colors;

package body GWindows.GControls.Duration is

   --------------
   --  Create  --
   --------------

   procedure Create
     (Window           : in out Duration_Type;
      Parent           : in out GWindows.Base.Base_Window_Type'Class;
      Initial_Duration : in     Ada.Calendar.Day_Duration             := 0.0;
      Left             : in     Integer                               := 0;
      Top              : in     Integer                               := 0;
      Width            : in     Integer                               := 0;
      Height           : in     Integer                               := 0;
      Show             : in     Boolean                               := True;
      Is_Dynamic       : in     Boolean                               := False)
   is begin
      Window.Data := Initial_Duration;

      GWindows.GControls.Create
        (Window     => GWindows.GControls.GControl_Type (Window),
         Parent     => Parent,
         Text       => "",
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         All_Keys   => False,
         Container  => False,
         Show       => Show,
         Is_Dynamic => Is_Dynamic);

      Background_Color (Window, GWindows.Colors.White);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Window);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Window, Width => New_Size.Width, Height => New_Size.Height);
         end;
      end if;

   end Create;

   ----------------
   --  Duration  --
   ----------------

   procedure Duration (Window : in out Duration_Type;
                       Value  : in     Ada.Calendar.Day_Duration)
   is begin
      Window.Data := Value;
      Format_Data (Window); --  FIXME: Possibly Dispatch to Duration_Edit?
      Redraw (Window, Erase => True);
   end Duration;

   function Duration (Window : in Duration_Type)
                     return Ada.Calendar.Day_Duration
   is begin
      return Window.Data;
   end Duration;

   -------------------
   --  Format_Data  --
   -------------------

   procedure Format_Data (Window : in out Duration_Type)
   is begin
      Window.Formatted_Data := Hours_Minutes (Window.Data);
   end Format_Data;

   ---------------------
   --  Hours_Minutes  --
   ---------------------

   function Hours_Minutes (Item : in Ada.Calendar.Day_Duration) return GString
   is
      --  'Floor is not defined for fixed point types, so we need to
      --  convert to floating point. To retain millisecond precision
      --  over 24 hours, we need a range of 8.64e+7, or at least 8
      --  digits.
      type Temp_Float is digits 8;
      Seconds : Temp_Float := Temp_Float (Item);
      Hours   : Temp_Float;
      Minutes : Temp_Float;
      Result  : String (1 .. 5);

   begin
      Hours   := Temp_Float'Floor (Seconds / 3600.0);
      Seconds := Seconds - Hours * 3600.0;

      if Hours < 10.0 then
         Result (1) := '0';
         Result (2) :=
           Standard.Duration'Image (Standard.Duration (Hours)) (2);
      else
         Result (1 .. 2) :=
           Standard.Duration'Image (Standard.Duration (Hours)) (2 .. 3);
      end if;

      Result (3) := ':';

      Minutes := Temp_Float'Floor (Seconds / 60.0);
      Seconds := Seconds - Minutes * 60.0;

      if Minutes < 10.0 then
         Result (4) := '0';
         Result (5) :=
           Standard.Duration'Image (Standard.Duration (Minutes)) (2);
      else
         Result (4 .. 5) :=
           Standard.Duration'Image (Standard.Duration (Minutes)) (2 .. 3);
      end if;

      return GWindows.GStrings.To_GString_From_String (Result);
   end Hours_Minutes;

   -----------------
   --  On_Create  --
   -----------------

   procedure On_Create (Window : in out Duration_Type)
   is begin
      --  Called here rather than in Create, so formatted string is
      --  available in Parent_Notify_Create_Destroy message handler.
      Format_Data (Window);
      GWindows.GControls.On_Create (GWindows.GControls.GControl_Type (Window));
   end On_Create;

   ----------------
   --  On_Paint  --
   ----------------

   procedure On_Paint (Window : in out Duration_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Unreferenced (Area);
      Current_Font : GWindows.Drawing_Objects.Font_Type;
   begin
      Get_Font (Window, Current_Font);
      GWindows.Drawing.Background_Color (Canvas, Background_Color (Window));
      GWindows.Drawing.Select_Object (Canvas, Current_Font);
      GWindows.Drawing.Put (Canvas, Text => Window.Formatted_Data);
   end On_Paint;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Window : in Duration_Type)
                             return GWindows.Types.Size_Type
   is
      use type GWindows.Types.Size_Type;

      Largest_Duration : constant Standard.Duration :=
        23.0 * 3600.0 + 59.0 * 60;

      Formatted_Duration : constant GString :=
        Hours_Minutes (Largest_Duration);

      Canvas : GWindows.Drawing.Canvas_Type;
      Font : GWindows.Drawing_Objects.Font_Type;
   begin
      Get_Canvas (Window, Canvas);
      Get_Font (Window, Font);
      GWindows.Drawing.Select_Object (Canvas, Font);
      return Calculate_New_Window_Size
        (Window,
         (6, 6) + GWindows.Drawing.Text_Output_Size
           (Canvas, Formatted_Duration));
   end Recommended_Size;

   ------------
   --  Text  --
   ------------

   function Text (Window : in Duration_Type) return GString
   is begin
      return Window.Formatted_Data;
   end Text;

end GWindows.GControls.Duration;
