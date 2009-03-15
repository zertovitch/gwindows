with Ada.Numerics.Discrete_Random;

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Drawing; use GWindows.Drawing;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;

with GWindows.Application;
with GWindows.Events;
with GWindows.Colors;

procedure Thread_Test is
   Main_Window  : Main_Window_Type;
   Title_String : GWindows.GString := "Thread_Test";

   task Title_Animation is
      entry Start;
      entry Stop;
   end Title_Animation;

   task body Title_Animation is
      Location : Natural := Title_String'First;

      type Direction_Type is (Forward, Back);
      Direction : Direction_Type := Forward;
   begin
      accept Start;

      loop
         select
            accept Stop;
            exit;
         or
            delay 0.10;
            Text (Main_Window,
                  Title_String (Title_String'First .. Location));
            if Direction = Forward then
               if Location = Title_String'Last then
                  Direction := Back;
               else
                  Location := Location + 1;
               end if;
            else
               if Location < Title_String'First then
                  Direction := Forward;
               else
                  Location := Location - 1;
               end if;
            end if;
         end select;
      end loop;
   end Title_Animation;

   task Random_Squares is
      entry Start;
      entry Stop;
   end Random_Squares;

   task body Random_Squares is
      Canvas : Canvas_Type;
   begin
      accept Start;
      Get_Canvas (Main_Window, Canvas);

      loop
         select
            accept Stop;
            exit;
         or
            delay 0.01;

            declare
               Brush : Brush_Type;

               subtype X_Range is
                 Integer range 0 .. Client_Area_Width (Main_Window);

               subtype Y_Range is
                 Integer range 0 .. Client_Area_Height (Main_Window);

               package X_Random is
                  new Ada.Numerics.Discrete_Random (X_Range);

               package Y_Random is
                  new Ada.Numerics.Discrete_Random (Y_Range);

               package Color_Random is new Ada.Numerics.Discrete_Random
                 (GWindows.Colors.Color_Type);

               X_Generator     : X_Random.Generator;
               Y_Generator     : Y_Random.Generator;
               Color_Generator : Color_Random.Generator;
            begin
               X_Random.Reset (X_Generator);
               Y_Random.Reset (Y_Generator);
               Color_Random.Reset (Color_Generator);

               Create_Solid_Brush (Brush,
                                   Color_Random.Random (Color_Generator));
               Fill_Rectangle
                 (Canvas,
                  (X_Random.Random (X_Generator),
                   Y_Random.Random (Y_Generator),
                   X_Random.Random (X_Generator),
                   Y_Random.Random (Y_Generator)),
                  Brush);
            end;
         end select;
      end loop;
   end Random_Squares;

begin
   Create (Main_Window);
   Show (Main_Window);

   Title_Animation.Start;
   Random_Squares.Start;
   GWindows.Application.Message_Loop;
   Random_Squares.Stop;
   Title_Animation.Stop;
end Thread_Test;
