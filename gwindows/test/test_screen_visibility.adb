with GWindows.Application,
     GWindows.Base,
     GWindows.GStrings,
     GWindows.Windows.Main;

procedure Test_Screen_Visibility is
   pragma Linker_Options ("-mwindows");  --  Don't display the console window
   --
   use GWindows.Application;
   Main_Window : GWindows.Windows.Main.Main_Window_Type;
   --
   procedure Moved (Window : in out GWindows.Base.Base_Window_Type'Class;
                    Left   : in     Integer;
                    Top    : in     Integer)
   is
      use GWindows.GStrings;
   begin
      Window.Text (
        To_GString_From_String (
          Screen_Visibility_Type'Image (Screen_Visibility ((Left, Top)))
        ) & " = corner visibility; " & Image (Left) & ',' & Image (Top)
      );
   end Moved;
   --
begin
   Main_Window.Create ("Move me!");
   Main_Window.Width (500);
   Main_Window.Height (200);
   Main_Window.On_Move_Handler (Moved'Unrestricted_Access);
   Main_Window.Show;
   Message_Loop;  --  Just wait till the window is closed
end Test_Screen_Visibility;
