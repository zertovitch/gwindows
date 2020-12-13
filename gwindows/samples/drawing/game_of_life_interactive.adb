with Ada.Exceptions;
with Ada.Command_Line;

with Game_of_Life;

with GWindows.Application,
     GWindows.Colors;

with GWindows.Panels;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;
with GWindows.Base; use GWindows.Base;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Types;

procedure Game_of_Life_Interactive is
   pragma Linker_Options ("-mwindows");

   Main        : Main_Window_Type;
   Draw_Win    : Window_Type;

   Drawing_Area : Canvas_Type;
   Saved_Area   : Memory_Canvas_Type;
   Saved_Bitmap : Bitmap_Type;

   package GoL renames Game_of_Life;

   subtype Fixed_Map is GoL.Map_Type (1 .. 134, 1 .. 100);

   type Map_Array is array (0 .. 1) of Fixed_Map;

   protected GoL_Map is
      procedure Clear;
      procedure Set (x, y : Integer; f : GoL.Figure; s : GoL.State);
      function Is_Changed (x, y : Integer) return Boolean;
      function Get (x, y : Integer) return GoL.State;
      procedure Evolve;
   private
      current : Natural := 0;
      Map : Map_Array;
   end GoL_Map;

   protected body GoL_Map is
      --
      procedure Clear is
      begin
        GoL.Clear (Map (current));
      end Clear;
      --
      procedure Set (x, y : Integer; f : GoL.Figure; s : GoL.State) is
      begin
        GoL.Add_Figure (Map (current), x, y, f, s);
      end Set;
      --
      function Is_Changed (x, y : Integer) return Boolean is
         use GoL;
      begin
        return Map (current) (x, y) /= Map (1 - current) (x, y);
      end Is_Changed;
      --
      function Get (x, y : Integer) return GoL.State is
      begin
        return Map (current) (x, y);
      end Get;
      --
      procedure Evolve is
      begin
        GoL.Move (Map (current), Map (1 - current));
        current := 1 - current;
      end Evolve;
   end GoL_Map;

   GoL_Brushes : array (GoL.State) of Brush_Type;

   procedure Draw_Map (full : Boolean) is
      type Real is digits 15;
      cw, ch : Real;
      ww : constant Integer := Draw_Win.Client_Area_Width;
      wh : constant Integer := Draw_Win.Client_Area_Height;
   begin
      cw := Real (ww) / Real (Fixed_Map'Last (1));
      ch := Real (wh) / Real (Fixed_Map'Last (2));
      for x in Fixed_Map'Range (1) loop
         for y in Fixed_Map'Range (2) loop
            if full or else GoL_Map.Is_Changed (x, y) then
               Fill_Rectangle (
                  Saved_Area,
                 (Integer (cw * Real (x - 1)),
                  Integer (ch * Real (y - 1)),
                  Integer (cw * Real (x)),
                  Integer (ch * Real (y))),
                 GoL_Brushes (GoL_Map.Get (x, y))
               );
            end if;
         end loop;
      end loop;
      --
      BitBlt (Drawing_Area, 0, 0, ww, wh, Saved_Area, 0, 0);
   end Draw_Map;

   wait : Duration := 0.1;
   pragma Volatile (wait);

   active : Boolean := True;
   pragma Volatile (active);

   current_figure : GoL.Figure := GoL.Block;

   Play_Pause_Button  : Button_Type;
   Speed_Plus_Button  : Button_Type;
   Speed_Minus_Button : Button_Type;
   Figure_Button      : Button_Type;
   Clear_Button       : Button_Type;

   procedure Refresh_Titles is
   begin
      Main.Text (
        "Game of Life - Hand of God: left-click: add life, right-click: remove - " &
        "Wait time =" & To_GString_From_String (Duration'Image (wait))
      );
      Figure_Button.Text (To_GString_From_String (GoL.Figure'Image (current_figure)));
      if active then
         Play_Pause_Button.Text ("Playing");
      else
         Play_Pause_Button.Text ("Paused");
      end if;
   end Refresh_Titles;

   task Animation is
      entry Start;
      entry Play_Pause;
      entry Quit;
   end Animation;

   task body Animation is
   begin
      accept Start;
      loop
         select
            accept Play_Pause;
            active := not active;
         or
            accept Quit;
            exit;
         or
            delay wait;
            if active then
               GoL_Map.Evolve;
               Draw_Map (full => False);
            end if;
         end select;
      end loop;
   end Animation;

   procedure Do_Mouse_Move
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
      xi, yi : Integer;
      use GoL;
   begin
      if Keys (Left_Button) or Keys (Right_Button) then
         xi := 1 + (Fixed_Map'Last (1) * X) / Window.Client_Area_Width;
         yi := 1 + (Fixed_Map'Last (2) * Y) / Window.Client_Area_Height;
         if Keys (Left_Button) then
            --  Spin_It.Add_Remove (Alive, xi, yi);
            GoL_Map.Set (xi, yi, current_figure, Alive);
         end if;
         if Keys (Right_Button) then
            --  Spin_It.Add_Remove (Dead, xi, yi);
            GoL_Map.Set (xi, yi, current_figure, Dead);
         end if;
         Draw_Map (full => True);
      end if;
   end Do_Mouse_Move;

   procedure Do_Paint
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   pragma Unreferenced (Window, Canvas, Area);
   begin
      Draw_Map (full => True);
   end Do_Paint;

   procedure Do_Close (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      Animation.Quit;
      GWindows.Application.End_Loop;
   end Do_Close;

   procedure Do_Play_Pause_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      Animation.Play_Pause;
      Refresh_Titles;
   end Do_Play_Pause_Button;

   procedure Do_Speed_Plus_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      if wait > 0.01 then
        wait := wait - 0.01;
      end if;
      Refresh_Titles;
   end Do_Speed_Plus_Button;

   procedure Do_Speed_Minus_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      if wait < 0.5 then
        wait := wait + 0.01;
      end if;
      Refresh_Titles;
   end Do_Speed_Minus_Button;

   procedure Do_Figure_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
      use GoL;
   begin
      if current_figure = Figure'Last then
        current_figure := Figure'First;
      else
        current_figure := Figure'Succ (current_figure);
      end if;
      Refresh_Titles;
   end Do_Figure_Button;

   procedure Do_Clear_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      GoL_Map.Clear;
      Draw_Map (full => True);
   end Do_Clear_Button;

   Buttons_Panel : GWindows.Panels.Panel_Type;
   Window_Font : Font_Type;

   use GoL;
   use GWindows.Application, GWindows.Colors;

   Big_W, Big_H : Integer;

   button_width : constant := 130;

begin
   GoL_Map.Clear;

   On_Destroy_Handler (Main, Do_Close'Unrestricted_Access);

   Create (Main, "Game of Life", Width => 790, Height => 590);

   Keyboard_Support (Main, True); --  Allow ESC key for Done_Button

   Create_As_Control (Draw_Win, Main, "", 0, 0, 0, 0);
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

   GoL_Brushes (GoL.Dead).Create_Solid_Brush (White);
   GoL_Brushes (GoL.Alive).Create_Solid_Brush (Dark_Green);
   for S in GoL.State loop
     Drawing_Area.Select_Object (GoL_Brushes (S));
     Saved_Area.Select_Object (GoL_Brushes (S));
   end loop;

   Big_W := 2 * Desktop_Width;
   Big_H := 2 * Desktop_Height;

   Create_Compatible_Bitmap (Drawing_Area, Saved_Bitmap, Big_W, Big_H);
   Select_Object (Saved_Area, Saved_Bitmap);

   On_Paint_Handler (Draw_Win, Do_Paint'Unrestricted_Access);
   On_Left_Mouse_Button_Down_Handler (Draw_Win,
                                      Do_Mouse_Move'Unrestricted_Access);
   On_Right_Mouse_Button_Down_Handler (Draw_Win,
                                       Do_Mouse_Move'Unrestricted_Access);
   On_Mouse_Move_Handler (Draw_Win,
                          Do_Mouse_Move'Unrestricted_Access);

   Buttons_Panel.Create (Main, 0, 0, 100, 40);

   Play_Pause_Button.Create  (Buttons_Panel, "(play / pause)", button_width * 0, 0, button_width, 40);
   Speed_Plus_Button.Create  (Buttons_Panel, "Speed +",        button_width * 1, 0, button_width, 40);
   Speed_Minus_Button.Create (Buttons_Panel, "Speed -",        button_width * 2, 0, button_width, 40);
   Figure_Button.Create      (Buttons_Panel, "(figure)",       button_width * 3, 0, button_width, 40);
   Clear_Button.Create       (Buttons_Panel, "Clear",          button_width * 4, 0, button_width, 40);

   On_Click_Handler (Play_Pause_Button,  Do_Play_Pause_Button'Unrestricted_Access);
   On_Click_Handler (Speed_Plus_Button,  Do_Speed_Plus_Button'Unrestricted_Access);
   On_Click_Handler (Speed_Minus_Button, Do_Speed_Minus_Button'Unrestricted_Access);
   On_Click_Handler (Figure_Button,      Do_Figure_Button'Unrestricted_Access);
   On_Click_Handler (Clear_Button,       Do_Clear_Button'Unrestricted_Access);

   --  Use Standard Windows GUI font instead of system font
   Create_Stock_Font (Window_Font, Default_GUI);
   Play_Pause_Button.Set_Font (Window_Font);
   Speed_Minus_Button.Set_Font (Window_Font);
   Speed_Plus_Button.Set_Font (Window_Font);
   Figure_Button.Set_Font (Window_Font);
   Clear_Button.Set_Font (Window_Font);

   --  Play_Pause_Button.Dock (At_Left);
   Buttons_Panel.Dock (At_Bottom);
   Main.Dock_Children;
   Buttons_Panel.Dock_Children;

   Show (Main);
   Refresh_Titles;

   Animation.Start;
   Message_Loop;
   Animation.Quit;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("Game_of_Life",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Game_of_Life_Interactive;
