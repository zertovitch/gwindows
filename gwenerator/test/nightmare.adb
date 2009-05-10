with GWindows.Base;                     use GWindows.Base;
with GWindows.Windows.Main;             use GWindows.Windows.Main;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.GStrings;
with GWindows.Application;
with GWin_Util;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

procedure Nightmare is
   pragma Linker_Options ("-mwindows");

   function "+"(S : String) return GWindows.GString
     renames GWindows.GStrings.To_GString_From_String;

   rg: Generator;
   hell: Integer:= 1;
   total: Integer:= 0;

   procedure Daemons;

   procedure Get_Data(Window : in out GWindows.Base.Base_Window_Type'Class) is
   begin
     Daemons;
   end Get_Data;

   -- A fake message box, with more :-)
   type Kookoo_box is new Window_Type with record
     OK_Button : GWindows.Buttons.Dialog_Button_Type;
   end record;
   type Kookoo_box_access is access Kookoo_box;

   procedure Daemons is
      A_Window :  Kookoo_box_access;
   begin
      for n in 1 .. hell loop
         A_Window := new  Kookoo_box;
         total:= total + 1;
         Create (A_Window.all, +("Asking -" & total'Img),
                 Left       => Integer(Random(rg) * 800.0),
                 Top        => Integer(Random(rg) * 600.0),
                 Width      => 220,
                 Height     => 120,
                 Is_Dynamic => True);
         GWin_Util.Use_GUI_Font(A_Window.all);
         Create_Label (A_Window.all, "How's life ?", 10, 10, 100, 25);
         Create (A_Window.OK_Button, A_Window.all, "OK", 30, 50, 80, 30);
         On_Click_Handler (A_Window.OK_Button, Get_data'Unrestricted_Access);
         On_Destroy_Handler (A_Window.all, Get_Data'Unrestricted_Access);
         Visible (A_Window.all);
      end loop;
      hell:= hell + 1;
   end Daemons;

   Main_Window : Main_Window_Type;
   Disp_Button : GWindows.Buttons.Button_Type;

begin
   Reset(rg);

   Create (Main_Window, "Nightmare", 10,10, 500, 300);
   GWin_Util.Use_GUI_Font(Main_Window);
   Create (Disp_Button, Main_Window, "&Start nightmare", 200, 150, 175, 30);
   Center(Disp_Button);
   On_Click_Handler (Disp_Button, Get_data'Unrestricted_Access);
   Visible (Main_Window, True);

   GWindows.Application.Message_Loop;
end Nightmare;
