--  Contribued package from Wiljan Derks - wiljan.derks@philips.com

with GWindows.Base;             use GWindows.Base;
with GWindows.Buttons;          use GWindows.Buttons;
with GWindows.Colors;           use GWindows.Colors;
with GWindows.Combo_Boxes;      use GWindows.Combo_Boxes;
with GWindows.Drawing_Objects;  use GWindows.Drawing_Objects;
with GWindows.Edit_Boxes;       use GWindows.Edit_Boxes;
with GWindows.Types;            use GWindows.Types;
with GWindows.Scroll_Bars;      use GWindows.Scroll_Bars;
with GWindows.Static_Controls;  use GWindows.Static_Controls;
with GWindows.Windows;          use GWindows.Windows;
with Interfaces.C;              use Interfaces.C;

package GWindows.Colored_Control_Window is

   type Colored_Control_Window_Type is new Window_Type with private;

   overriding
   procedure On_Message (Window       : in out Colored_Control_Window_Type;
                         message      : in     Interfaces.C.unsigned;
                         wParam       : in     GWindows.Types.Wparam;
                         lParam       : in     GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult);

   procedure Color (Window : in out Colored_Control_Window_Type;
                    Color : Color_Type);
   procedure Background
     (Window : in out Colored_Control_Window_Type;
      Bitmap : GString);
   procedure Initialize (Window : in out Colored_Control_Window_Type);

   type Colored_Label_Type is new Label_Type with private;
   type Colored_Edit_Box_Type is new Edit_Box_Type with private;
   type Colored_Multi_Line_Edit_Box_Type is new
     Multi_Line_Edit_Box_Type with private;
   type Colored_Scroll_Bar_Type is new
     GWindows.Scroll_Bars.Scroll_Bar_Type with private;
   type Colored_Combo_Box_Type is new
     GWindows.Combo_Boxes.Combo_Box_Type with private;
   type Colored_Drop_Down_Combo_Box_Type is new
     GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type with private;
   type Colored_Check_Box_Type is new
     GWindows.Buttons.Check_Box_Type with private;

   procedure Initialize (Label  : in out Colored_Label_Type);

   procedure Color (Label : in out Colored_Label_Type;
                    Color :        Color_Type);
   procedure Text_Color (Label : in out Colored_Label_Type;
                         Color :        Color_Type);

   procedure Initialize (Edit  : in out Colored_Edit_Box_Type);

   procedure Color (Edit  : in out Colored_Edit_Box_Type;
                    Color :        Color_Type);
   procedure Text_Color (Edit  : in out Colored_Edit_Box_Type;
                         Color :        Color_Type);

   procedure Default_Color (Edit : in out Colored_Edit_Box_Type);

   procedure Read_Only (Edit  : in out Colored_Edit_Box_Type;
                        State :        Boolean := True);

   procedure Initialize (Edit : in out Colored_Multi_Line_Edit_Box_Type);

   procedure Color (Edit  : in out Colored_Multi_Line_Edit_Box_Type;
                    Color :        Color_Type);
   procedure Text_Color (Edit  : in out Colored_Multi_Line_Edit_Box_Type;
                         Color :        Color_Type);

   procedure Initialize (Scroll_Bar : in out Colored_Scroll_Bar_Type);

   procedure Color (Scroll_Bar : in out Colored_Scroll_Bar_Type;
                    Color      :        Color_Type);

   procedure Initialize (Box : in out Colored_Combo_Box_Type);

   procedure Color (Box   : in out Colored_Combo_Box_Type;
                    Color :        Color_Type);
   procedure Text_Color (Box   : in out Colored_Combo_Box_Type;
                         Color :        Color_Type);
   function Color (Box   : Colored_Combo_Box_Type)
                   return Color_Type;
   function Text_Color (Box   : Colored_Combo_Box_Type)
                        return Color_Type;
   function Color_Brush (Box   : Colored_Combo_Box_Type)
                         return GWindows.Types.Handle;

   procedure Default_Color (Box : in out Colored_Combo_Box_Type);

   procedure Initialize (Box : in out Colored_Drop_Down_Combo_Box_Type);

   procedure Color (Box   : in out Colored_Drop_Down_Combo_Box_Type;
                    Color :        Color_Type);
   procedure Text_Color (Box   : in out Colored_Drop_Down_Combo_Box_Type;
                         Color :        Color_Type);
   function Color (Box   : Colored_Drop_Down_Combo_Box_Type)
                   return Color_Type;
   function Text_Color (Box   : Colored_Drop_Down_Combo_Box_Type)
                        return Color_Type;
   function Color_Brush (Box   : Colored_Drop_Down_Combo_Box_Type)
                         return GWindows.Types.Handle;

   procedure Default_Color (Box : in out Colored_Drop_Down_Combo_Box_Type);

   procedure Initialize (Box : in out Colored_Check_Box_Type);

   procedure Color (Box   : in out Colored_Check_Box_Type;
                    Color :        Color_Type);
   function Color (Box   : Colored_Check_Box_Type) return Color_Type;
   function Color_Brush (Box   : Colored_Check_Box_Type)
                         return GWindows.Types.Handle;

   procedure Default_Color (Box : in out Colored_Check_Box_Type);

   --  When a check box is disabled to prevent a user from modifying it,
   --  the tooltip does not work anymore. Therefor this Read_Only
   --  procedure has been implemented.
   procedure Read_Only (Box   : in out Colored_Check_Box_Type;
                        State :        Boolean := True);

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
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

   type Colored_Edit_Box_Type is new Edit_Box_Type with
      record
         Color : Color_Type := White;
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

   type Colored_Multi_Line_Edit_Box_Type is new Multi_Line_Edit_Box_Type with
      record
         Color : Color_Type := System_Color (COLOR_BTNFACE);
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

   type Colored_Scroll_Bar_Type is new
     GWindows.Scroll_Bars.Scroll_Bar_Type with
      record
         Color : Color_Type := System_Color (COLOR_SCROLLBAR);
         Brush : Brush_Type;
      end record;

   type Colored_Combo_Box_Type is new
     GWindows.Combo_Boxes.Combo_Box_Type with
      record
         Color : Color_Type := System_Color (COLOR_SCROLLBAR);
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

   type Colored_Drop_Down_Combo_Box_Type is new
     GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type with
      record
         Color : Color_Type := System_Color (COLOR_SCROLLBAR);
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

   type Colored_Check_Box_Type is new GWindows.Buttons.Check_Box_Type with
      record
         Color     : Color_Type := System_Color (COLOR_BTNFACE);
         Brush     : Brush_Type;
         Read_Only : Boolean := False;
      end record;

   procedure On_Command
     (Box     : in out Colored_Check_Box_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     Pointer_To_Base_Window_Class);

end GWindows.Colored_Control_Window;
