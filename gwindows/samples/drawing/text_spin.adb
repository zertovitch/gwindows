--  This demo is drawing a text at regular time intervals
--  using an Ada task.

with GWindows.Windows; use GWindows.Windows;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Colors; use GWindows.Colors;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Base;
with GWindows.Application;

procedure Text_Spin is
   Main   : Window_Type;
   Canvas : Canvas_Type;

   task Spin_It is
      entry Start;
      entry Stop;
   end Spin_It;

   task body Spin_It is
      type Angle_Type is mod 360;

      Angle : Angle_Type := 1;
   begin
      accept Start;

      loop
         select
            accept Stop;
            exit;
         or
            delay 0.001;
            Angle := Angle - 1;
            Text (Main, " Angle : " &
                  To_GString_From_String (Angle_Type'Image (Angle)));

            declare
               F : Font_Type;
            begin
               Create_Font (F, "Arial", 20, Angle => Integer (Angle) * 10);
               Select_Object (Canvas, F);
               Put (Canvas,
                    Width (Main) / 2, Height (Main) / 2,
                    "I love GWindows!");
            end;

         end select;
      end loop;
   end Spin_It;

   procedure Do_Close (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      Spin_It.Stop;
      GWindows.Application.End_Loop;
   end Do_Close;

begin
   On_Destroy_Handler (Main, Do_Close'Unrestricted_Access);
   Create (Main);
   Visible (Main);

   Get_Canvas (Main, Canvas);
   Background_Color (Canvas, System_Color (COLOR_BTNFACE));

   Spin_It.Start;

   GWindows.Application.Message_Loop;
end Text_Spin;
