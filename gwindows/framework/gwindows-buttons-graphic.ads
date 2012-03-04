--  Inspired by GWindows.Buttons.Ex_buttons, but with less features
--  especially: no tasking (and related stack issues in GNAT)

with GWindows.Drawing_Objects;

package GWindows.Buttons.Graphic is

   ----------------------
   -- Icon_Button_Type --
   ----------------------

   type Icon_Button_Type is new Button_Type with null record;

   procedure On_Create (Window : in out Icon_Button_Type);

   procedure Set_Icon (
      Window : in out Icon_Button_Type;
      Icon   : GWindows.Drawing_Objects.Icon_Type
   );

   ------------------------
   -- Bitmap_Button_Type --
   ------------------------

   type Bitmap_Button_Type is new Button_Type with null record;

   procedure On_Create (Window : in out Bitmap_Button_Type);

   procedure Set_Bitmap (
      Window : in out Bitmap_Button_Type;
      Bitmap : GWindows.Drawing_Objects.Bitmap_Type
   );

end GWindows.Buttons.Graphic;
