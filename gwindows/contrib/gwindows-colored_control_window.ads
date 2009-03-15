--  Contribued package from Wiljan Derks - wiljan.derks@philips.com

with GWindows.Windows;          use GWindows.Windows;
with GWindows.Colors;           use GWindows.Colors;
with GWindows.Drawing_Objects;  use GWindows.Drawing_Objects;
with GWindows.Edit_Boxes;       use GWindows.Edit_Boxes;
with GWindows.Scroll_Bars;      use GWindows.Scroll_Bars;
with GWindows.Static_Controls;  use GWindows.Static_Controls;
with Interfaces.C;              use Interfaces.C;

package GWindows.Colored_Control_Window is

   type Colored_Control_Window_Type is new Window_Type with private;

   procedure On_Message (Window       : in out Colored_Control_Window_Type;
                         message      : in     Interfaces.C.unsigned;
                         wParam       : in     Interfaces.C.int;
                         lParam       : in     Interfaces.C.int;
                         Return_Value : in out Interfaces.C.long);

   procedure Color (Window : in out Colored_Control_Window_Type;
                    Color : Color_Type);
   procedure Background
     (Window : in out Colored_Control_Window_Type;
      Bitmap : GString);
   procedure Initialize (Window : in out Colored_Control_Window_Type);

   type Colored_Label_Type is new Label_Type with private;
   type Colored_Multi_Line_Edit_Box_Type is new
     Multi_Line_Edit_Box_Type with private;
   type Colored_Scroll_Bar_Type is new
     GWindows.Scroll_Bars.Scroll_Bar_Type with private;

   procedure Initialize (Label  : in out Colored_Label_Type);

   procedure Color (Label : in out Colored_Label_Type;
                    Color : Color_Type);

   procedure Initialize (Edit : in out Colored_Multi_Line_Edit_Box_Type);

   procedure Color (Edit : in out Colored_Multi_Line_Edit_Box_Type;
                    Color : Color_Type);

   procedure Initialize (Scroll_Bar : in out Colored_Scroll_Bar_Type);

   procedure Color (Scroll_Bar : in out Colored_Scroll_Bar_Type;
                    Color : Color_Type);

private

   type Colored_Control_Window_Type is new Window_Type with
      record
         Color    : Color_Type := System_Color (COLOR_BTNFACE);
         Bitmap   : GWindows.Drawing_Objects.Bitmap_Type;
         Stretched : GWindows.Drawing_Objects.Bitmap_Type;
         Width, Height : Integer := 0;
         Brush    : Brush_Type;
      end record;

   type Colored_Label_Type is new Label_Type with
      record
         Color : Color_Type := System_Color (COLOR_BTNFACE);
         Brush : Brush_Type;
      end record;

   type Colored_Multi_Line_Edit_Box_Type is new Multi_Line_Edit_Box_Type with
      record
         Color : Color_Type := System_Color (COLOR_BTNFACE);
         Brush : Brush_Type;
      end record;

   type Colored_Scroll_Bar_Type is new
     GWindows.Scroll_Bars.Scroll_Bar_Type with
      record
         Color : Color_Type := System_Color (COLOR_SCROLLBAR);
         Brush : Brush_Type;
      end record;

end GWindows.Colored_Control_Window;
