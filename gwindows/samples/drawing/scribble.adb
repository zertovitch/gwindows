with Ada.Exceptions;
with GNAT.OS_Lib;

with GWindows.Application; use GWindows.Application;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;
with GWindows.Constants; use GWindows.Constants;
with GWindows.Base; use GWindows.Base;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels; use GWindows.Drawing_Panels;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Colors;
with GWindows.Types;

--  Drawing using a drawing panel

procedure Scribble is
   pragma Linker_Options ("-mwindows");

   Main         : Main_Window_Type;
   Draw_Win     : Drawing_Panel_Type;
   Done_Button  : Cancel_Button_Type;
   Window_Font  : Font_Type;
   NB           : Brush_Type;
   Drawing_Area : Drawing_Canvas_Type;

   procedure Do_Mouse_Move
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (Window);
   begin
      if Keys (Left_Button) then
         Ellipse (Drawing_Area, X, Y, X + 50, Y + 50);
         Put (Drawing_Area, X + 15, Y + 25, "Moo");
         Redraw (Draw_Win, False);
      end if;
   end Do_Mouse_Move;

   procedure Do_Erase_Background
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   begin
      null; --  Don't paint main windows background to avoid flicker
            --  when resizing window
   end Do_Erase_Background;

begin
   Create (Main, "Test Input",
           Width => 300, HEIGHT => 300);
   On_Erase_Background_Handler
     (Main, Do_Erase_Background'Unrestricted_Access);

   Keyboard_Support (Main, True); --  Allow ESC key for Done_Button

   Create_As_Control (Draw_Win, Main, "", 0, 0, 100, 100);
   Dock (Draw_Win, Fill);

   On_Left_Mouse_Button_Down_Handler (Draw_Win,
                                      Do_Mouse_Move'Unrestricted_Access);
   On_Mouse_Move_Handler (Draw_Win,
                          Do_Mouse_Move'Unrestricted_Access);

   Create (Done_Button, Main, "Done Scribblin'",
           0, 0, 60, 25,
           Id => IDCANCEL);

   --  Use Standard Windows GUI font instead of system font
   Create_Stock_Font (Window_Font, Default_GUI);
   Set_Font (Done_Button, Window_Font);

   Dock (Done_Button, At_Bottom);

   Dock_Children (Main);

   Auto_Resize (Draw_Win, False);
   Resize_Canvas (Draw_Win, Desktop_Width, Desktop_Height);

   Get_Canvas (Draw_Win, Drawing_Area);

   Background_Mode (Drawing_Area, Transparent);

   Create_Stock_Brush (NB, Hollow_Brush);
   Select_Object (Drawing_Area, NB);

   Fill_Rectangle (Drawing_Area,
                   (0, 0, Desktop_Width, Desktop_Height),
                   GWindows.Colors.COLOR_WINDOW);

   Redraw (Draw_Win, False);

   Show (Main);

   Message_Loop;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("Scribble",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end Scribble;
