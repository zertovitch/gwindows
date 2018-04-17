--  Testing Tab Control within a dialog (see also tab_test.adb)

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Common_Controls; use GWindows.Common_Controls;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Base; use GWindows.Base;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.Application;

procedure Tab_in_dialog_test is
   Top_Window  : Main_Window_Type;
   Dialog_Box  : Window_Type;
   Tab_Control : Tab_Window_Control_Type;
   --  Window is an invisible window created as a control
   --  and containing the Tab Control.
   Window      : Window_Type;
   Window_1    : aliased Window_Type;
   Window_2    : aliased Window_Type;

   procedure Do_Click (Window : in out Base_Window_Type'Class) is
   begin
      Text (Dialog_Box, "Clicked: ---> " & Text (Window));
      Message_Box (Window, "Click!", Text (Window));
   end Do_Click;

begin
   Create (Top_Window, "Control Test - Main window");
   Create_As_Dialog (Dialog_Box, Top_Window, "Control Test - Dialog");

   Create_As_Control (Window, Dialog_Box, "", 0, 0, 0, 0);
   Keyboard_Support (Window);
   Border (Window);
   Dock (Window, Fill);

   Create (Tab_Control, Window, 10, 10, 300, 150);
   --  In some cases, a button click freezes the
   --  application if the following is not called:
   Set_As_Control_Parent (Tab_Control);

   Insert_Tab (Tab_Control, 0, "Tab 1");
   Insert_Tab (Tab_Control, 1, "Tab 2");

   Create_As_Control (Window_1,
                      Tab_Control, "",
                      0, 0, 10, 10,
                      Show => False);

   Create_As_Control (Window_2,
                      Tab_Control, "",
                      0, 0, 10, 10,
                      Show => False);

   Tab_Window (Tab_Control, 0, Window_1'Unchecked_Access);
   Tab_Window (Tab_Control, 1, Window_2'Unchecked_Access);

   declare
      Button_1 : Button_Access;
      Button_2 : Button_Access;
      EB_1 : Edit_Box_Access;
      EB_2 : Edit_Box_Access;
   begin
      Button_1 := new Button_Type;
      Create (Button_1.all, Window_1, "Button 1",
              10, 40, 75, 25, Is_Dynamic => True);
      On_Click_Handler (Button_1.all, Do_Click'Unrestricted_Access);

      EB_1 := new Edit_Box_Type;
      Create (EB_1.all,
              Window_1, "Window 1", 10, 10, 100, 25, Is_Dynamic => True);

      Button_2 := new Button_Type;
      Create (Button_2.all, Window_2, "Button 2",
              10, 40, 75, 25, Is_Dynamic => True);
      On_Click_Handler (Button_2.all, Do_Click'Unrestricted_Access);

      EB_2 := new Edit_Box_Type;
      Create (EB_2.all,
              Window_2, "Window 2", 10, 10, 100, 25, Is_Dynamic => True);

   end;

   Dock (Tab_Control, Fill);

   Dock_Children (Dialog_Box);

   Show (Top_Window);

   GWindows.Application.Show_Dialog (Dialog_Box, Top_Window);

end Tab_in_dialog_test;
