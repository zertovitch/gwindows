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
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Colors;
with GWindows.Types;

--  Drawing without a drawing panel

procedure Scribble2 is
   pragma Linker_Options ("-mwindows");

   Main        : Main_Window_Type;
   Draw_Win    : Window_Type;
   Done_Button : Cancel_Button_Type;
   Window_Font : Font_Type;

   Drawing_Area : Canvas_Type;
   Saved_Area   : Memory_Canvas_Type;
   Saved_Bitmap : Bitmap_Type;

   procedure Do_Mouse_Move
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (Window);
   begin
      if Keys (Left_Button) then
         Ellipse (Saved_Area, X, Y, X+50, Y+50);
         Ellipse (Drawing_Area, X, Y, X+50, Y+50);
         Put (Saved_Area, X+15, Y+25, "Moo");
         Put (Drawing_Area, X+15, Y+25, "Moo");
      end if;
   end Do_Mouse_Move;

   procedure Do_Paint
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   pragma Unreferenced (Window, Canvas, Area);
   begin
      BitBlt (Drawing_Area, 0, 0, Width (Draw_Win), Height (Draw_Win),
              Saved_Area, 0, 0);
   end Do_Paint;

begin
   Create (Main, "Test Input",
           Width => 300, Height => 300);

   Keyboard_Support (Main, True); --  Allow ESC key for Done_Button

   Create_As_Control (Draw_Win, Main, "", 0,0,0,0);
   Dock (Draw_Win, Fill);

   Get_Canvas (Draw_Win, Drawing_Area);
   Create_Memory_Canvas (Saved_Area, Drawing_Area);

   Background_Mode (Drawing_Area, Transparent);
   Background_Mode (Saved_Area, Transparent);

   declare
      NB : Brush_Type;
   begin
      Create_Stock_Brush (NB, Hollow_Brush);
      Select_Object (Drawing_Area, NB);
      Select_Object (Saved_Area, NB);
   end;

   Create_Compatible_Bitmap (Drawing_Area, Saved_Bitmap,
                             Desktop_Width,
                             Desktop_Height);
   Select_Object (Saved_Area, Saved_Bitmap);
   Fill_Rectangle (Saved_Area,
                   (0, 0, Desktop_Width, Desktop_Height),
                   GWindows.Colors.COLOR_WINDOW);

   On_Paint_Handler (Draw_Win, Do_Paint'Unrestricted_Access);
   On_Left_Mouse_Button_Down_Handler (Draw_Win,
                                      Do_Mouse_Move'Unrestricted_Access);
   On_Mouse_Move_Handler (Draw_Win,
                          Do_Mouse_Move'Unrestricted_Access);

   Create (Done_Button, Main, "Done Scribblin'",
           0,0,60,25,
           ID => IDCANCEL);

   --  Use Standard Windows GUI font instead of system font
   Create_Stock_Font (Window_Font, Default_GUI);
   Set_Font (Done_Button, Window_Font);

   Dock (Done_Button, At_Bottom);

   Dock_Children (Main);

   Show (Main);

   Message_Loop;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("Scribble2",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      GNAT.OS_Lib.OS_Exit (1);
end Scribble2;
