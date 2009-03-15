with GTest;

with GWindows.Drawing_Panels; use GWindows.Drawing_Panels;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Colors; use GWindows.Colors;
with GWindows.Events;
with Ada.Text_Io;
with GWindows.Application;
with GWindows.GStrings.IO; use GWindows.GStrings.IO;

procedure Test_Resources is
begin
   GTest.Start_Test ("Resources");

   Ada.Text_Io.Put_Line (Ada.Text_Io.Standard_Error, "Creating 10_000 windows; be patient ...");
   for N in 1 .. 10000 loop
      if 0 = N mod 100 then
         Ada.Text_Io.Put (Ada.Text_Io.Standard_Error, '.');
      elsif 0 = N mod 1000 then
         Ada.Text_Io.Put_Line (Ada.Text_Io.Standard_Error, Integer'Image (N) & "done");
      end if;

      declare
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

         Redraw (Main_Window, False);
      end;
   end loop;

   Ada.Text_Io.Put_Line (Ada.Text_Io.Standard_Error, "Check Resources in Task Manager.");

   GTest.End_Test;
end Test_Resources;
