--  Testing some Windows controls
--  
--  - Combo_Box_Type
--  - Drop_Down_Combo_Box_Type
--  - Drop_Down_List_Box_Type

with GWindows.Message_Boxes;
with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Base;
with GWindows.GStrings;

with GWindows.Combo_Boxes;

procedure Win_Controls is
   --  Resource is just used for having the nice XP+ design.
   --  Resource file is obtained with the command:
   --  windres -i control_test.rc -o ..\..\obj\control_test.coff
   --
   --  NB: 32 bit and 64 bit binaries .coff are uncompatible!
   --
   --  pragma Linker_Options ("control_test.coff");

   use GWindows.Windows.Main;
   use GWindows.Combo_Boxes;

   Top              : Main_Window_Type;
   Combo_Control    : Combo_Box_Type;
   DD_Combo_Control : Drop_Down_Combo_Box_Type;
   DD_List_Control  : Drop_Down_List_Box_Type;

   procedure Do_Dbl_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      GWindows.Message_Boxes.Message_Box
        ("Selection",
         Value (Combo_Control, Current (Combo_Control)));
   end Do_Dbl_Click;

begin
   Top.Create ("Hello World");
   Top.Size (620, 300);

   --  Combo
   Combo_Control.Create (Top, "", 10, 10, 180, 200);
   Combo_Control.On_Double_Click_Handler (Do_Dbl_Click'Unrestricted_Access);
   for N in 1 .. 50 loop
      Combo_Control.Add (GWindows.GStrings.Image (N));
   end loop;
   Combo_Control.Text ("I'm a Combo_Box_Type");

   --  Drop Down Combo
   DD_Combo_Control.Create (Top, "", 210, 10, 180, 200);
   for N in 1 .. 50 loop
      DD_Combo_Control.Add (GWindows.GStrings.Image (N));
   end loop;
   DD_Combo_Control.Text ("I'm a Drop_Down_Combo_Box_Type");

   --  Drop Down List
   --
   --  CAUTION, banana skin here: if you let the argument for Text, like
   --           the "" above, the wrong Create will be called and
   --           a Combo Box will actually be created!
   --
   DD_List_Control.Create (Top, 410, 10, 180, 200);
   for N in 1 .. 50 loop
      DD_List_Control.Add (GWindows.GStrings.Image (N));
   end loop;
   DD_List_Control.Add ("I'm a Drop_Down_List_Box_Type");
   --  Text must be in the list.
   DD_List_Control.Text ("I'm a Drop_Down_List_Box_Type");

   Top.Visible;
   GWindows.Application.Message_Loop;
end Win_Controls;
