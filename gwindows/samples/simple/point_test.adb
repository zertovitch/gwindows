with GWindows.Application,
     GWindows.Base,
     GWindows.GStrings.IO,
     GWindows.Windows.Main,
     GWindows.Types;

procedure Point_Test is

   Top : GWindows.Windows.Main.Main_Window_Type;

   procedure Do_Mouse_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
   pragma Unreferenced (Keys);
      use GWindows.Types;
      Relative_Point : constant Point_Type := (X, Y);
      Absolute_Point : constant Point_Type :=
                                   Window.Point_To_Desktop (Relative_Point);

      Monitor_Count, Monitor_With_Point : Natural := 0;

      procedure Check_Point_In_Monitor (Rectangle : Rectangle_Type) is
      begin
         Monitor_Count := Monitor_Count + 1;
         if Absolute_Point.X in Rectangle.Left .. Rectangle.Right
            and then Absolute_Point.Y in Rectangle.Top .. Rectangle.Bottom
         then
            Monitor_With_Point := Monitor_Count;
         end if;
      end Check_Point_In_Monitor;

      use GWindows.Application, GWindows.GStrings, GWindows.GStrings.IO;

   begin
      Put_Line ("Click relative to Window :" &
                  Image (X) & " ," & Image (Y) &
                  "  ---  Window dimensions: " &
                  Image (Window.Client_Area_Width) & " ," &
                  Image (Window.Client_Area_Height));
      Put_Line ("Click relative to Desktop :" &
                  Image (Absolute_Point.X) & " ," & Image (Absolute_Point.Y) &
                  "  ---  Desktop dimensions: " &
                  Image (Desktop_Width) & " ," &
                  Image (Desktop_Height));
      Enumerate_Display_Monitors (Check_Point_In_Monitor'Unrestricted_Access);
      Put_Line ("Point is visible on monitor #" & Image (Monitor_With_Point));
   end Do_Mouse_Click;

   Monitor_Count : Natural := 0;

   procedure List_Monitor_Dimensions (Rectangle : GWindows.Types.Rectangle_Type) is
      use GWindows.GStrings, GWindows.GStrings.IO;
   begin
      Monitor_Count := Monitor_Count + 1;
      Put_Line ("Monitor #" & Image (Monitor_Count) & "  -----------");
      Put_Line ("               Left:" & Image (Rectangle.Left));
      Put_Line ("                Top:" & Image (Rectangle.Top));
      Put_Line ("              Right:" & Image (Rectangle.Right));
      Put_Line ("             Bottom:" & Image (Rectangle.Bottom));
      New_Line;
   end List_Monitor_Dimensions;

begin
   Top.Create ("Click somewhere on this window's client area!");
   Top.Size (500, 100);
   Top.On_Left_Mouse_Button_Down_Handler (Do_Mouse_Click'Unrestricted_Access);
   Top.Visible;
   --
   GWindows.Application.Enumerate_Display_Monitors
     (List_Monitor_Dimensions'Unrestricted_Access);
   --
   GWindows.Application.Message_Loop;
end Point_Test;
