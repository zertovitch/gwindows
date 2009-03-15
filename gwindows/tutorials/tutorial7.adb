with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Drawing_Panels; use GWindows.Drawing_Panels;
with GWindows.Scroll_Panels; use GWindows.Scroll_Panels;
with GWindows.Drawing;
with GWindows.Colors; use GWindows.Colors;
with GWindows.Events;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial7 is
   pragma Linker_Options ("-mwindows");

   use GWindows.Drawing;
   use GWindows.Drawing_Panels;

   procedure Draw_Something
     (Canvas : in out GWindows.Drawing.Canvas_Type'Class)
   is
   begin
      for N in 1 .. 100 loop
         Fill_Rectangle (Canvas,
                         (N * 2, N * 2, N + 20, N + 20),
                         COLOR_3DHILIGHT);
         Fill_Rectangle (Canvas,
                         (N * 2 + 200, N * 2 + 200, N + 180, N + 180),
                         COLOR_DESKTOP);
      end loop;
   end Draw_Something;

begin
   --  Using a Drawing Panel as a Window

   declare
      Main_Window : GWindows.Drawing_Panels.Drawing_Panel_Type;
      Canvas      : GWindows.Drawing_Panels.Drawing_Canvas_Type;
   begin
      Create (Main_Window, "Drawing Window", Width => 200, Height => 125);
      Visible (Main_Window, True);
      On_Destroy_Handler (Main_Window,
                          GWindows.Events.Do_End_Application'Access);
      --  Since Drawing_Panel_Type is not derived from Main_Window_Type
      --  it will not automaticly close the application when the window
      --  is destroyed. This handler will do that for us.

      Auto_Resize (Main_Window, False);
      Resize_Canvas (Main_Window,
                     GWindows.Application.Desktop_Width,
                     GWindows.Application.Desktop_Height);
      --  By turning off auto resize and setting canvas to the size of
      --  of the desktop contents will be saved no matter how we
      --  resize the window

      Get_Canvas (Main_Window, Canvas);

      Draw_Something (Canvas);

      GWindows.Application.Message_Loop;
   end;

   --  Using a Drawing Panel as a Control

   declare
      Main_Window  : GWindows.Windows.Main.Main_Window_Type;
      Draw_Control : GWindows.Drawing_Panels.Drawing_Panel_Type;
      Canvas       : GWindows.Drawing_Panels.Drawing_Canvas_Type;
   begin
      Create (Main_Window, "Drawing Window", Width => 400, Height => 400);
      Visible (Main_Window, True);

      Create_As_Control (Draw_Control, Main_Window,
                         Top    => 20,
                         Left   => 20,
                         Width  => 300,
                         Height => 300);

      Get_Canvas (Draw_Control, Canvas);

      Draw_Something (Canvas);

      GWindows.Application.Message_Loop;
   end;

   --  Using a Drawing Panel in a Scroll Panel

   declare
      Main_Window  : GWindows.Windows.Main.Main_Window_Type;
      Scroll_Panel : GWindows.Scroll_Panels.Scroll_Panel_Type;
      Draw_Control : GWindows.Drawing_Panels.Drawing_Panel_Type;
      Canvas       : GWindows.Drawing_Panels.Drawing_Canvas_Type;
   begin
      Create (Main_Window, "Drawing Window", Width => 400, Height => 400);
      Visible (Main_Window, True);

      Create_As_Control (Scroll_Panel, Main_Window,
                         Top    => 20,
                         Left   => 20,
                         Width  => 300,
                         Height => 300);

      Panel_Size (Scroll_Panel, 500, 500);

      Create_As_Control (Draw_Control, Scroll_Panel.Panel,
                         Top    => 0,
                         Left   => 0,
                         Width  => 0,
                         Height => 0);
      Dock (Draw_Control, GWindows.Base.Fill);
      Dock_Children (Scroll_Panel.Panel);

      Get_Canvas (Draw_Control, Canvas);

      Draw_Something (Canvas);

      GWindows.Application.Message_Loop;
   end;

end Tutorial7;
