------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--      G W I N D O W S . G C O N T R O L S . D U R A T I O N . E D I T     --
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

with GWindows.Colors;
with GWindows.Key_States;
with GWindows.Drawing_Objects;

package body GWindows.GControls.Duration.Edit is

   --------------
   --  Local operations

   procedure Increment_Field
     (Window : in out Duration_Edit_Type;
      Up     : in     Boolean);

   procedure Increment_Field
     (Window : in out Duration_Edit_Type;
      Up     : in     Boolean)
   is begin
      --  Explicit limit tests, because user may compile release
      --  version with constraint tests disabled.

      --  FIXME: handle Page_Up/Down; increment by +/- 15 minutes

      case Window.Current_Field is
      when Hour =>
         if Up then
            if Window.Data <= Ada.Calendar.Day_Duration'Last - 3600.0 then
               Window.Data := Window.Data + 3600.0;
            else
               return;
            end if;
         else
            if Window.Data >= 3600.0 then
               Window.Data := Window.Data - 3600.0;
            else
               return;
            end if;
         end if;

      when Minute =>
         if Up then
            if Window.Data <= Ada.Calendar.Day_Duration'Last - 60.0 then
               Window.Data := Window.Data + 60.0;
            else
               return;
            end if;
         else
            if Window.Data >= 60.0 then
               Window.Data := Window.Data - 60.0;
            else
               return;
            end if;
         end if;

      end case;

      Format_Data (Window);

      Redraw (Window, Erase => True);

      Fire_On_Value_Changed (Window);

   end Increment_Field;

   procedure Next_Field
     (Window : in out Duration_Edit_Type'Class;
      Right  : in     Boolean);
   --  Move to next field (left or right). If at end field, don't move.

   procedure Next_Field
     (Window : in out Duration_Edit_Type'Class;
      Right  : in     Boolean)
   is begin
      Find_Field :
      loop
         if Right then
            if Window.Current_Field = Field_Label_Type'Last then
               return;
            else
               Window.Current_Field :=
                 Field_Label_Type'Succ (Window.Current_Field);
            end if;
         else
            if Window.Current_Field = Field_Label_Type'First then
               return;
            else
               Window.Current_Field :=
                 Field_Label_Type'Pred (Window.Current_Field);
            end if;
         end if;

         if Window.Field_Text_Bounds (Window.Current_Field).First /= 0 then
            exit Find_Field;
         end if;
      end loop Find_Field;

      Redraw (Window, Erase => True); --  Redisplay highlighted field
   end Next_Field;

   ----------
   --  Public operations, alphabetical order

   --------------
   --  Create  --
   --------------

   procedure Create
     (Window           : in out Duration_Edit_Type;
      Parent           : in out GWindows.Base.Base_Window_Type'Class;
      Initial_Duration : in     Ada.Calendar.Day_Duration            := 0.0;
      Left             : in     Integer                              := 0;
      Top              : in     Integer                              := 0;
      Width            : in     Integer                              := 0;
      Height           : in     Integer                              := 0;
      Show             : in     Boolean                              := True;
      Is_Dynamic       : in     Boolean                              := False)
   is begin
      Window.Data            := Initial_Duration;
      Window.Current_Field   := Field_Label_Type'First;

      Window.Field_Text_Bounds (Hour).First    := 1;
      Window.Field_Text_Bounds (Hour).Last     := 2;
      Window.Field_Text_Bounds (Minute).First  := 4;
      Window.Field_Text_Bounds (Minute).Last   := 5;

      --  Field_Rects set in Format_Data, since we are probably
      --  using a proportional font.

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

      Format_Data (Window);

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

   -----------------------------
   --  Fire_On_Value_Changed  --
   -----------------------------

   procedure Fire_On_Value_Changed (Window : in out Duration_Edit_Type) is
      use type GWindows.Base.Action_Event;
   begin
      if Window.On_Value_Changed_Handler /= null then
         Window.On_Value_Changed_Handler (Window);
      end if;
   end Fire_On_Value_Changed;

   -------------------
   --  Format_Data  --
   -------------------

   procedure Format_Data (Window : in out Duration_Edit_Type) is
      use GWindows.Types;
      use GWindows.Drawing;
      Canvas : GWindows.Drawing.Canvas_Type;
   begin

      GWindows.GControls.Duration.Format_Data
         (GWindows.GControls.Duration.Duration_Type (Window));

      Get_Canvas (Window, Canvas);

      for I in Window.Field_Rects'Range loop
         if Window.Field_Text_Bounds (I).First = 1 then
            Window.Field_Rects (I).Left := 0;
            Window.Field_Rects (I).Top  := 0;
         else
            Window.Field_Rects (I).Left := Text_Output_Size
              (Canvas, Window.Formatted_Data
                 (Window.Formatted_Data'First ..
                    Window.Field_Text_Bounds (I).First - 1)).Width;

            Window.Field_Rects (I).Top := 0;
         end if;

         Right_Bottom
           (Window.Field_Rects (I),
            Left_Top (Window.Field_Rects (I)) + Text_Output_Size
              (Canvas, Window.Formatted_Data
                 (Window.Field_Text_Bounds (I).First ..
                    Window.Field_Text_Bounds (I).Last)));
      end loop;
   end Format_Data;

   -----------------
   --  On_Create  --
   -----------------

   procedure On_Create (Window : in out Duration_Type) is
   begin
      --  Called here rather than in Create, so formatted string is
      --  available in Parent_Notify_Create_Destroy message handler.
      Format_Data (Window);
   end On_Create;

   -------------------------
   --  On_Character_Down  --
   -------------------------

   procedure On_Character_Down
     (Window      : in out Duration_Edit_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter)
   is
      use GWindows.Windows;
      use GWindows.Key_States;
   begin
      case Special_Key is
         when Down_Key =>
            Increment_Field (Window, Up => False);

         when Left_Key =>
            Next_Field (Window, Right => False);

         when Right_Key =>
            Next_Field (Window, Right => True);

         when Up_Key =>
            Increment_Field (Window, Up => True);

         when None =>
            case Value is
               when GCharacter'Val (9) => --  Tab
                  Next_Field
                  (Window,
                   Right => not Is_Key_Down (VK_SHIFT));
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end On_Character_Down;

   ----------------
   --  On_Focus  --
   ----------------

   procedure On_Focus (Window : in out Duration_Edit_Type) is
   begin
      Window.Current_Field := Hour;
      Redraw (Window, Erase => True); --  Redisplay highlighted field
      Fire_On_Focus (Window);
   end On_Focus;

   ---------------------
   --  On_Lost_Focus  --
   ---------------------

   procedure On_Lost_Focus (Window : in out Duration_Edit_Type) is
   begin
      Redraw (Window, Erase => True); --  Redisplay un-highlighted field
      Fire_On_Lost_Focus (Window);
   end On_Lost_Focus;

   ---------------------------------
   --  On_Left_Mouse_Button_Down  --
   ---------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out Duration_Edit_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, Keys);
      use GWindows.Types;
      Point : constant Point_Type := (X, Y);
   begin
      Focus (Window);

      Find_Field :
      for I in Window.Field_Rects'Range loop
         if Inside (Point, Window.Field_Rects (I)) then
            Window.Current_Field := I;
            Redraw (Window, Erase => True); --  Redisplay highlighted field
            exit Find_Field;
         end if;
      end loop Find_Field;

   end On_Left_Mouse_Button_Down;

   ----------------
   --  On_Paint  --
   ----------------

   procedure On_Paint
     (Window : in out Duration_Edit_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Warnings (Off, Area);
      use GWindows.Drawing;
      use GWindows.Colors;
      use type GWindows.Types.Handle;
      use type Interfaces.C.long;
      Current_Font : GWindows.Drawing_Objects.Font_Type;
   begin
      Get_Font (Window, Current_Font);
      GWindows.Drawing.Background_Color (Canvas, Background_Color (Window));
      GWindows.Drawing.Select_Object (Canvas, Current_Font);

      Horizontal_Text_Alignment (Canvas, Left, Use_Current_Position => True);
      Vertical_Text_Alignment (Canvas, Top, Use_Current_Position => True);

      if Handle (Window) /= GWindows.Base.Handle (GWindows.Base.Focus.all) then
         Put (Canvas, Text => Window.Formatted_Data);
      else
         --  Highlight current field
         case Window.Current_Field is
            when Hour =>
               Text_Color (Canvas, System_Color (COLOR_HIGHLIGHTTEXT));
               Background_Color (Canvas, System_Color (COLOR_HIGHLIGHT));

               Put
                 (Canvas,
                  Text => Window.Formatted_Data
                    (Window.Field_Text_Bounds (Hour).First ..
                       Window.Field_Text_Bounds (Hour).Last));

               Text_Color (Canvas, System_Color (COLOR_WINDOWTEXT));
               Background_Color (Canvas, System_Color (COLOR_WINDOW));

               Put
                 (Canvas,
                  Text => Window.Formatted_Data
                    (Window.Field_Text_Bounds (Hour).Last + 1 ..
                       Window.Formatted_Data'Last));

            when Minute =>
               Text_Color (Canvas, System_Color (COLOR_WINDOWTEXT));
               Background_Color (Canvas, System_Color (COLOR_WINDOW));
               Put
                 (Canvas,
                  Text => Window.Formatted_Data
                    (Window.Formatted_Data'First ..
                       Window.Field_Text_Bounds (Minute).First - 1));

               Text_Color (Canvas, System_Color (COLOR_HIGHLIGHTTEXT));

               Background_Color
                 (Canvas, System_Color (COLOR_HIGHLIGHT));

               Put
                 (Canvas,
                  Text => Window.Formatted_Data
                    (Window.Field_Text_Bounds (Minute).First ..
                          Window.Field_Text_Bounds (Minute).Last));

         end case;
      end if;

   end On_Paint;

   --------------------------------
   --  On_Value_Changed_Handler  --
   --------------------------------

   procedure On_Value_Changed_Handler
     (Window  : in out Duration_Edit_Type;
      Handler : in GWindows.Base.Action_Event)
   is begin
      Window.On_Value_Changed_Handler := Handler;
   end On_Value_Changed_Handler;

end GWindows.GControls.Duration.Edit;
