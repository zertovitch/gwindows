with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Drawing;
with GWindows.Colors;
with GWindows.Types;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial8 is
   pragma Linker_Options ("-mwindows");

   procedure Do_Paint
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
     use GWindows.Drawing;
     use GWindows.Colors;
     pragma Unreferenced (Window);
     pragma Unreferenced (Area);
   begin
      for N in 1 .. 100 loop
         Fill_Rectangle (Canvas,
                         (N * 2, N * 2, N + 20, N + 20),
                         COLOR_3DHILIGHT);
         Fill_Rectangle (Canvas,
                         (N * 2 + 200, N * 2 + 200, N + 180, N + 180),
                         COLOR_DESKTOP);
      end loop;
   end Do_Paint;

   Main_Window : GWindows.Windows.Main.Main_Window_Type;
begin
   Create (Main_Window, "On_Paint Drawing Window",
      Width => 200, Height => 200);
   Visible (Main_Window, True);
   On_Paint_Handler (Main_Window, Do_Paint'Unrestricted_Access);

   GWindows.Application.Message_Loop;
end Tutorial8;
