with GWindows.Message_Boxes;
with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Types;
with GWindows.Base;
with GWindows.GStrings;
with GNAT.IO; use GNAT.IO;

with GWindows.Combo_Boxes;

procedure Win_Controls is
   use GWindows.Windows.Main;
   use GWindows.Combo_Boxes;

   Top           : GWindows.Windows.Main.Main_Window_Type;
   Combo_Control : GWindows.Combo_Boxes.Combo_Box_Type;

   procedure Do_Dbl_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Message_Boxes.Message_Box
        ("Selection",
         Value (Combo_Control, Current (Combo_Control)));
   end Do_Dbl_Click;

begin
   Create (Top, "Hello World");
   Size (Top, 500, 300);
   Create (Combo_Control, Top, "", 10, 10, 200, 200);
   On_Double_Click_Handler (Combo_Control, Do_Dbl_Click'Unrestricted_Access);

   for N in 1 .. 50 loop
      Add (Combo_Control, GWindows.GStrings.Image (N));
   end loop;

   Visible (Top);
   GWindows.Application.Message_Loop;
end Win_Controls;
