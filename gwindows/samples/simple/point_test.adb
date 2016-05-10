--  with GWindows.Message_Boxes;
with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Types;
with GWindows.Base;

with GWindows.GStrings.IO; use GWindows.GStrings.IO;
with GWindows.GStrings; use GWindows.GStrings;

procedure Point_Test is
   use GWindows.Windows.Main;

   Top : GWindows.Windows.Main.Main_Window_Type;

   procedure Do_Mouse_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   pragma Unreferenced (Keys);
      New_Point : GWindows.Types.Point_Type := (X, Y);
   begin
      Put_Line ("Click relative to window :" &
                  Image (X) & " x" & Image (Y));
      New_Point := GWindows.Base.Point_To_Desktop (Window, New_Point);
      Put_Line ("Click relative to desktop :" &
                  Image (New_Point.X) & " x" & Image (New_Point.Y));

   end Do_Mouse_Click;
begin
   Create (Top, "Hello World");
   Size (Top, 300, 100);
   On_Left_Mouse_Button_Down_Handler (Top, Do_Mouse_Click'Unrestricted_Access);
   Visible (Top);
   GWindows.Application.Message_Loop;
end Point_Test;
