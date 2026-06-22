with Ada.Exceptions;
with Ada.Command_Line;

with Game_of_Life;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Colors,
     GWindows.Drawing,
     GWindows.Drawing_Objects,
     GWindows.GStrings,
     GWindows.Message_Boxes,
     GWindows.Panels,
     GWindows.Timers,
     GWindows.Types,
     GWindows.Windows.Main;

with Interfaces.C;

procedure Game_of_Life_Interactive is
   pragma Linker_Options ("-mwindows");
   pragma Linker_Options ("control_test.coff");

   type Game_Window_Type is new GWindows.Windows.Main.Main_Window_Type with null record;

   overriding
   procedure On_Message
     (Window       : in out Game_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

   Main        : Game_Window_Type;
   Draw_Win    : GWindows.Windows.Window_Type;

   Drawing_Area : GWindows.Drawing.Canvas_Type;
   Saved_Area   : GWindows.Drawing.Memory_Canvas_Type;
   Saved_Bitmap : GWindows.Drawing_Objects.Bitmap_Type;

   package GoL renames Game_of_Life;

   subtype Fixed_Map is GoL.Map_Type (1 .. 134, 1 .. 100);

   type Map_Array is array (0 .. 1) of Fixed_Map;

   protected GoL_Map is
      procedure Clear_All;
      procedure Clear_Current;
      procedure Set (x, y : Integer; f : GoL.Figure; s : GoL.State);
      function Is_Changed (x, y : Integer) return Boolean;
      function Get (x, y : Integer) return GoL.State;
      procedure Swap;
      procedure Evolve;
   private
      current : Natural := 0;
      Map : Map_Array;
   end GoL_Map;

   protected body GoL_Map is
      --
      procedure Clear_All is
      begin
        GoL.Clear (Map (current));
        GoL.Clear (Map (1 - current));
      end Clear_All;
      --
      procedure Clear_Current is
      begin
        GoL.Clear (Map (current));
      end Clear_Current;
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
      procedure Swap is
      begin
        current := 1 - current;
      end Swap;
      --
      procedure Evolve is
      begin
        GoL.Move (Map (current), Map (1 - current));
        Swap;
      end Evolve;
   end GoL_Map;

   GoL_Brushes : array (GoL.State) of GWindows.Drawing_Objects.Brush_Type;
   Grid_Pen : GWindows.Drawing_Objects.Pen_Type;

   draw_grid : constant Boolean := True;

   procedure Draw_Map (full : Boolean) is
      type Real is digits 15;
      cw, ch : Real;
      ww : constant Integer := Draw_Win.Client_Area_Width;
      wh : constant Integer := Draw_Win.Client_Area_Height;
      margin : Natural;
   begin
      cw := Real (ww) / Real (Fixed_Map'Last (1));
      ch := Real (wh) / Real (Fixed_Map'Last (2));
      if draw_grid then
         margin := 1;
      else
         margin := 0;
      end if;
      if full then
         Saved_Area.Fill_Rectangle ((0, 0, ww, wh), GoL_Brushes (GoL.Dead));
         if draw_grid then
            Saved_Area.Select_Object (Grid_Pen);
            for x in 0 .. Fixed_Map'Last (1) loop
               Saved_Area.Line
                  (Integer (cw * Real (x)), 0,
                   Integer (cw * Real (x)), wh);
            end loop;
            for y in 0 .. Fixed_Map'Last (2) loop
               Saved_Area.Line
                  (0,  Integer (ch * Real (y)),
                   ww, Integer (ch * Real (y)));
            end loop;
         end if;
      end if;
      for x in Fixed_Map'Range (1) loop
         for y in Fixed_Map'Range (2) loop
            if full or else GoL_Map.Is_Changed (x, y) then
               Saved_Area.Fill_Rectangle
                  ((Integer (cw * Real (x - 1)) + margin,
                    Integer (ch * Real (y - 1)) + margin,
                    Integer (cw * Real (x)),
                    Integer (ch * Real (y))),
                 GoL_Brushes (GoL_Map.Get (x, y)));
            end if;
         end loop;
      end loop;
      --
      Drawing_Area.BitBlt (0, 0, ww, wh, Saved_Area, 0, 0);
   end Draw_Map;

   wait : Duration := 0.1;
   pragma Volatile (wait);

   active : Boolean := True;
   pragma Volatile (active);

   current_figure : GoL.Figure := GoL.Block;

   Play_Pause_Button,
   Speed_Plus_Button,
   Speed_Minus_Button,
   Figure_Button,
   Clear_Button : GWindows.Buttons.Button_Type;

   use GWindows.GStrings;

   procedure Refresh_Titles is
   begin
      Main.Text
        ("The Game of Life.       " &
         "Hand of God  -->   " &
         "Left-click: add life.   Right-click: remove.       " &
         "Wait time =" & To_GString_From_String (Duration'Image (wait)));
      Figure_Button.Text (To_GString_From_String (GoL.Figure'Image (current_figure)));
      if active then
         Play_Pause_Button.Text ("Playing");
      else
         Play_Pause_Button.Text ("-- Paused --");
      end if;
   end Refresh_Titles;

   Timer_ID : constant Natural := 0;
   Timer_Is_Active : Boolean := False;

   procedure Update_Timer is
      MS : constant Natural := Natural (wait * 1000.0);
   begin
      if Timer_Is_Active then
         GWindows.Timers.Kill_Timer (Main, Timer_ID);
         Timer_Is_Active := False;
      end if;
      if active then
         GWindows.Timers.Set_Timer
           (Window => Main, ID_Event => Timer_ID, Milliseconds => MS);
           Timer_Is_Active := True;
      end if;
   end Update_Timer;

   procedure Stop_Timer is
   begin
      if Timer_Is_Active then
         GWindows.Timers.Kill_Timer (Main, Timer_ID);
         Timer_Is_Active := False;
      end if;
   end Stop_Timer;

   overriding
   procedure On_Message
     (Window       : in out Game_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
   is
      use type Interfaces.C.unsigned;
   begin
      if message = GWindows.Timers.WM_TIMER then
         if active then
            GoL_Map.Evolve;
            Draw_Map (full => False);
         end if;
         Return_Value := 0;
      else
         GWindows.Windows.Main.Main_Window_Type (Window).On_Message
           (message, wParam, lParam, Return_Value);
      end if;
   end On_Message;

   procedure Instructions (x0, y0 : Integer) is
     subtype Row_Range is Natural range 1 .. 49;
     subtype Row_Text is String (Row_Range);
     bitmap : constant array (1 .. 10) of Row_Text :=
       ("                                                 ",
        "  ##   #     #        #                        # ",
        " #  #  #              #                        # ",
        " #     #    ##    ##  # ##      ## #    #      # ",
        " #     #     #   #    ##        # # #  # #     # ",
        " #     #     #   #    ##        # # #  ###     # ",
        " #  #  #  #  #   #    # #       # # #  #         ",
        "  ##    ##  ###   ##  #  #      # # #   ##     # ",
        "                                                 ",
        " - - - - - - - - - - - - - - - - - - - - -       ");
   begin
     GoL_Map.Clear_Current;
     for y in bitmap'Range loop
       for x in Row_Range loop
         if bitmap (y) (x) /= ' ' then
           GoL_Map.Set (x + x0, y + y0, GoL.Point, GoL.Alive);
         end if;
       end loop;
     end loop;
   end Instructions;

   procedure Do_Mouse_Move
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      xi, yi : Integer;
      use GoL, GWindows.Windows;
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
      Stop_Timer;
      GWindows.Application.End_Loop;
   end Do_Close;

   procedure Do_Play_Pause_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      active := not active;
      Update_Timer;
      Refresh_Titles;
   end Do_Play_Pause_Button;

   procedure Do_Speed_Plus_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      if wait > 0.01 then
        wait := wait - 0.01;
        Update_Timer;
      end if;
      Refresh_Titles;
   end Do_Speed_Plus_Button;

   procedure Do_Speed_Minus_Button (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      if wait < 0.5 then
        wait := wait + 0.01;
        Update_Timer;
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
      GoL_Map.Clear_All;
      Draw_Map (full => True);
   end Do_Clear_Button;

   Buttons_Panel : GWindows.Panels.Panel_Type;
   Window_Font : GWindows.Drawing_Objects.Font_Type;

   use GoL;
   use GWindows.Application, GWindows.Base, GWindows.Colors, GWindows.Drawing, GWindows.Drawing_Objects;

   Big_W, Big_H : Integer;

   button_width : constant := 130;

begin
   GoL_Map.Clear_All;

   On_Destroy_Handler (Main, Do_Close'Unrestricted_Access);

   Create (Main, "Game of Life", Width => 790, Height => 590);

   Keyboard_Support (Main, True);  --  Allow ESC key for Done_Button

   Draw_Win.Create_As_Control (Main, "", 0, 0, 0, 0);
   Draw_Win.Dock (Fill);

   Draw_Win.Get_Canvas (Drawing_Area);
   Saved_Area.Create_Memory_Canvas (Drawing_Area);

   Drawing_Area.Background_Mode (Transparent);
   Saved_Area.Background_Mode (Transparent);

   declare
      NB : GWindows.Drawing_Objects.Brush_Type;
   begin
      NB.Create_Stock_Brush (Hollow_Brush);
      Drawing_Area.Select_Object (NB);
      Saved_Area.Select_Object (NB);
   end;

   GoL_Brushes (GoL.Dead).Create_Solid_Brush (White);
   GoL_Brushes (GoL.Alive).Create_Solid_Brush (Dark_Green);
   Grid_Pen.Create_Pen (Solid, 0, Light_Gray);

   Big_W := 2 * Desktop_Width;
   Big_H := 2 * Desktop_Height;

   Drawing_Area.Create_Compatible_Bitmap (Saved_Bitmap, Big_W, Big_H);
   Saved_Area.Select_Object (Saved_Bitmap);

   Draw_Win.On_Paint_Handler (Do_Paint'Unrestricted_Access);

   Draw_Win.On_Left_Mouse_Button_Down_Handler  (Do_Mouse_Move'Unrestricted_Access);
   Draw_Win.On_Right_Mouse_Button_Down_Handler (Do_Mouse_Move'Unrestricted_Access);
   Draw_Win.On_Mouse_Move_Handler              (Do_Mouse_Move'Unrestricted_Access);

   Buttons_Panel.Create (Main, 0, 0, 100, 40);

   Play_Pause_Button.Create  (Buttons_Panel, "(play / pause)", button_width * 0, 0, button_width, 40);
   Speed_Plus_Button.Create  (Buttons_Panel, "Speed +",        button_width * 1, 0, button_width, 40);
   Speed_Minus_Button.Create (Buttons_Panel, "Speed -",        button_width * 2, 0, button_width, 40);
   Figure_Button.Create      (Buttons_Panel, "(figure)",       button_width * 3, 0, button_width, 40);
   Clear_Button.Create       (Buttons_Panel, "Clear",          button_width * 4, 0, button_width, 40);

   --  Callbacks. Note: in a larger application, you can prefer overriding
   --  the methods and avoid pointers (access types) completely.
   Play_Pause_Button.On_Click_Handler  (Do_Play_Pause_Button'Unrestricted_Access);
   Speed_Plus_Button.On_Click_Handler  (Do_Speed_Plus_Button'Unrestricted_Access);
   Speed_Minus_Button.On_Click_Handler (Do_Speed_Minus_Button'Unrestricted_Access);
   Figure_Button.On_Click_Handler      (Do_Figure_Button'Unrestricted_Access);
   Clear_Button.On_Click_Handler       (Do_Clear_Button'Unrestricted_Access);

   --  Use Standard Windows GUI font instead of system font
   Window_Font.Create_Stock_Font (Default_GUI);

   Play_Pause_Button.Set_Font  (Window_Font);
   Speed_Minus_Button.Set_Font (Window_Font);
   Speed_Plus_Button.Set_Font  (Window_Font);
   Figure_Button.Set_Font      (Window_Font);
   Clear_Button.Set_Font       (Window_Font);

   --  Play_Pause_Button.Dock (At_Left);
   Buttons_Panel.Dock (At_Bottom);
   Main.Dock_Children;
   Buttons_Panel.Dock_Children;

   Main.Show;
   Refresh_Titles;

   Draw_Map (full => True);
   Main.Redraw (Redraw_Now => True);

   --  Show instructions first...

   Instructions (40, 20);
   GoL_Map.Swap;
   Instructions (20, 40);
   for blinks in 1 .. 5 loop
     GoL_Map.Swap;
     Draw_Map (full => False);
     Main.Redraw (Redraw_Now => True);
     Message_Check;
     delay 0.3;
   end loop;
   delay 1.0;
   Update_Timer;

   --  Now the interactive part begins...

   Message_Loop;

   Stop_Timer;

exception
   when E : others =>
      GWindows.Message_Boxes.Message_Box
        ("Game_of_Life",
         To_GString_From_String (Ada.Exceptions.Exception_Name (E) & " : " &
                                 Ada.Exceptions.Exception_Message (E)));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Game_of_Life_Interactive;
