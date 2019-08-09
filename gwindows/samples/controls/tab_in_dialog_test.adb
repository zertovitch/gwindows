--  Testing Tab Control within a dialog (see also tab_test.adb).
--  By Andre van Splunter.
--
--  For real applications using tabs in dialogs, see:
--
--    AZip ( http://azip.sf.net/ ), quick help dialog:
--      package AZip_GWin.Help (azip_gwin-help.adb)
--
--    TeXCAD ( http://texcad.sf.net ), procedure On_General_Options
--      in package TC.GWin.MDI_Main (tc_gwin\tc-gwin-mdi_main.adb)

with GWindows;                          use GWindows;
with GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.Main;             use GWindows.Windows.Main;

procedure Tab_in_dialog_test is

   Top_Window  : Main_Window_Type;
   Start_Panel : Button_Type;

   procedure Panel_Dialog
      (Parent : in out GWindows.Base.Base_Window_Type'Class)
   is
      Panel       : Window_Type;
      Okay        : Default_Button_Type;
      Tab_Control : Tab_Window_Control_Type;
      Window_1    : aliased Window_Type;
      Window_2    : aliased Window_Type;

      procedure Do_Click (Window : in out Base_Window_Type'Class)
      is
      begin
         Text (Top_Window, "Clicked: ---> " & Text (Window));
      end Do_Click;

   begin
      Create_As_Dialog (Panel, Parent, "Panel");
      Create (Tab_Control, Panel, 10, 10, 300, 150);
      --  Any click in the buttons Button_1, Button_2 below
      --  will freeze the application if the following has
      --  *not* been called:
      Set_As_Control_Parent (Tab_Control);
      --  See discussion:
      --  [Gnavi-discuss] Button clicked inside of a tab -> hanging
      --  06.01.2007

      Insert_Tab (Tab_Control, 0, "Tab 1");
      Insert_Tab (Tab_Control, 1, "Tab 2");

      Create_As_Control (Window_1, Tab_Control, "", 0, 0, 10, 10,
         Show => False);
      Create_As_Control (Window_2, Tab_Control, "", 0, 0, 10, 10,
         Show => False);

      Tab_Window (Tab_Control, 0, Window_1'Unchecked_Access);
      Tab_Window (Tab_Control, 1, Window_2'Unchecked_Access);

      declare
         Button_1 : Button_Access;
         Button_2 : Button_Access;
      begin
         Button_1 := new Button_Type;
         Create (Button_1.all, Window_1, "Button 1",
            10, 30, 75, 25, Is_Dynamic => True);
         On_Click_Handler (Button_1.all, Do_Click'Unrestricted_Access);

         Button_2 := new Button_Type;
         Create (Button_2.all, Window_2, "Button 2",
            10, 30, 75, 25, Is_Dynamic => True);
         On_Click_Handler (Button_2.all, Do_Click'Unrestricted_Access);
      end;

      Dock (Tab_Control, At_Top);
      Create (Okay, Panel, "O&k", 20,
         Client_Area_Height (Panel) - 40, 60, 25, ID => IDOK);
      Dock_Children (Panel);

      GWindows.Application.Show_Dialog (Panel, Top_Window);
   end Panel_Dialog;

begin
   Create (Top_Window, "Control Test");
   Create (Start_Panel, Top_Window, "Start Panel",
      10, 30, 125, 25, Is_Dynamic => True);
   On_Click_Handler (Start_Panel, Panel_Dialog'Unrestricted_Access);
   Dock_Children (Top_Window);
   Show (Top_Window);
   GWindows.Application.Message_Loop;

end Tab_in_dialog_test;
