with Interfaces.C;

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Application;
with GWindows.Base;
with GWindows.Types;

procedure Splash_Screen is
   pragma Linker_Options ("-mwindows");
   pragma Linker_Options ("splash_screen.coff");

   Main_Window : GWindows.Windows.Main.Main_Window_Type;
   Splash      : GWindows.Drawing_Objects.Bitmap_Type;
   Full_Canvas : GWindows.Drawing.Canvas_Type;

begin
   Create (Main_Window, "Splash Screen", Width => 500, Height => 346);

   Load_Bitmap (Splash, "#100");

   Center (Main_Window);

   Visible (Main_Window);

   Get_Full_Window_Canvas (Main_Window, Full_Canvas);

   Paint_Bitmap (Full_Canvas, Splash,
                 0, 0, Width (Main_Window), Height (Main_Window));

   delay 5.0;

   Close (Main_Window);

end Splash_Screen;
