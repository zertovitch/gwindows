with GWindows.Drawing_Panels; use GWindows.Drawing_Panels;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Colors; use GWindows.Colors;
with GWindows.Events;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial9 is
   pragma Linker_Options ("-mwindows");

   Main_Window : GWindows.Drawing_Panels.Drawing_Panel_Type;
   Canvas      : GWindows.Drawing_Panels.Drawing_Canvas_Type;
   Brush       : GWindows.Drawing_Objects.Brush_Type;
   Pen         : GWindows.Drawing_Objects.Pen_Type;
begin
   Create (Main_Window, "On_Paint Drawing Window",
           Width => 200, Height => 200);
   Visible (Main_Window, True);
   On_Destroy_Handler (Main_Window,
                       GWindows.Events.Do_End_Application'Access);
   Auto_Resize (Main_Window, False);
   Resize_Canvas (Main_Window,
                  GWindows.Application.Desktop_Width,
                  GWindows.Application.Desktop_Height,
                  False);

   Get_Canvas (Main_Window, Canvas);

   Create_Pen (Pen,
               Style => Solid,
               Width => 3,
               Color => Blue);
   Select_Object (Canvas, Pen);

   Create_Solid_Brush (Brush,
                       Color => Orange);
   Select_Object (Canvas, Brush);

   Ellipse (Canvas,
            10, 10,
            Client_Area_Width (Main_Window) - 10,
            Client_Area_Height (Main_Window) - 10);

   Create_Hatch_Brush (Brush,
                       Style => Cross,
                       Color => Red);
   Select_Object (Canvas, Brush);

   Create_Stock_Pen (Pen, Null_Pen);
   Select_Object (Canvas, Pen);

   Rectangle (Canvas,
              25, 25,
              Client_Area_Width (Main_Window) - 25,
              Client_Area_Height (Main_Window) - 25);

   Redraw (Main_Window);
   --  Tell windows that a redraw of the panel is needed.

   GWindows.Application.Message_Loop;
end Tutorial9;
