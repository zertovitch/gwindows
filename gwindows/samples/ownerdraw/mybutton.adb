with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Types;
with GWindows.Base;
with GWindows.Colors;
with GWindows.Message_Boxes;
with GWindows.Buttons;

with Owner_Drawn_Button;

procedure mybutton is
   use GWindows.Windows.Main;
   use GWindows.Buttons;
   use Owner_Drawn_Button;

   procedure Do_Paint (Window   : in out GWindows.Base.Base_Window_Type'Class;
                       Canvas   : in out GWindows.Drawing.Canvas_Type;
                       Area     : in     GWindows.Types.Rectangle_Type;
                       Selected : in     Boolean;
                       Focused  : in     Boolean)
   is
      use GWindows.Drawing;
      use GWindows.Drawing_Objects;

      Back_Brush : Brush_Type;
   begin
      Create_System_Color_Brush (Back_Brush, GWindows.Colors.COLOR_BTNFACE);

      Fill_Rectangle (Canvas, Area, Back_Brush);

      Put (Canvas, 3, 3, GWindows.Base.Text (Window), Area);

      if Selected then
         Invert_Rectangle (Canvas, Area);
      end if;

      if Focused then
         declare
            Brush : Brush_Type;
         begin
            Create_Stock_Brush (Brush, Black_Brush);
            Frame_Rectangle (Canvas, Area, Brush);
         end;
      end if;
   end Do_Paint;

   procedure Do_Click (Window   : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Message_Boxes.Message_Box (Window,
                                          "Hello_World",
                                          "Hello World!");
   end Do_Click;

   Top     : Main_Window_Type;
   Button  : Owner_Drawn_Button_Type;
   Button2 : Button_Type;
begin
   Create (Top, "Hello World");
   Size (Top, 300, 100);
   Keyboard_Support (Top);
   Visible (Top);

   Create (Button, Top, "Test", 10, 10, 50, 25);
   On_Paint_Handler (Button, Do_Paint'Unrestricted_Access);
   On_Click_Handler (Button, Do_Click'Unrestricted_Access);
   Focus (Button);

   Create (Button2, Top, "Test2", 70, 10, 50, 25);
   On_Click_Handler (Button2, Do_Click'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end mybutton;
