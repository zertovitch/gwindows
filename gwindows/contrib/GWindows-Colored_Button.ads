with GWindows.Base;             use GWindows.Base;
with GWindows.Buttons;          use GWindows.Buttons;
with GWindows.Colors;           use GWindows.Colors;
with GWindows.Drawing;          use GWindows.Drawing;
with GWindows.Drawing_Objects;  use GWindows.Drawing_Objects;
with GWindows.Types;            use GWindows.Types;

package GWindows.Colored_Button is

   type Colored_Button_Type is new GWindows.Buttons.Button_Type with private;

   procedure On_Create (Button : in out Colored_Button_Type);

   procedure Color (Button : in out Colored_Button_Type;
                    Color : Color_Type);

   procedure Text_Color (Button : in out Colored_Button_Type;
                         Color : Color_Type);

private

   procedure On_Draw_Item
      (Window          : in out Colored_Button_Type;
       Canvas          : in out GWindows.Drawing.Canvas_Type;
       Item_ID         : in     Integer;
       Item_Action     : in     Interfaces.C.unsigned;
       Item_State      : in     Interfaces.C.unsigned;
       Item_Rect       : in     GWindows.Types.Rectangle_Type;
       Item_Data       : in     Integer;
       Control         : in     Pointer_To_Base_Window_Class);

   type Colored_Button_Type is new GWindows.Buttons.Button_Type with
      record
         Color : Color_Type := System_Color (COLOR_BTNFACE);
         Text  : Color_Type := Black;
         Brush : Brush_Type;
      end record;

end GWindows.Colored_Button;
