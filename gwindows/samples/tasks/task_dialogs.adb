with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Application;
with GWindows.Base;

with GWindows.Buttons; use GWindows.Buttons;

procedure Task_Dialogs is
   task type Task_Dialog is
      entry Start;
   end Task_Dialog;

   task body Task_Dialog is
      Task_Win : Window_Type;
   begin
      accept Start;

      Create_As_Dialog (Task_Win, "Task_Dialog", Width => 100, Height => 100);

      GWindows.Application.Show_Modal (Task_Win);
   end Task_Dialog;

   Main_Win     : Main_Window_Type;
   Start_Button : Button_Type;

   T1 : Task_Dialog;
   T2 : Task_Dialog;
   T3 : Task_Dialog;

   procedure Start_Dialogs
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      T1.Start;
      T2.Start;
      T3.Start;
      Disable (Start_Button);
   end Start_Dialogs;

begin
   Create (Main_Win, "Task_Dialogs", Width => 300, Height => 300);
   Create (Start_Button, Main_Win, "Start Dialogs", 0, 0, 100, 30);
   Center (Start_Button);
   On_Click_Handler (Start_Button, Start_Dialogs'Unrestricted_Access);
   Visible (Main_Win);

   GWindows.Application.Message_Loop;
end Task_Dialogs;
