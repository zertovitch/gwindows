with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Static_Controls,
     GWindows.Windows.Main;

procedure Dock_Test is
   use GWindows.Base, GWindows.Buttons, GWindows.Static_Controls,
       GWindows.Windows, GWindows.Windows.Main;

   Window : Main_Window_Type;
   Button : Cancel_Button_Type;
   L1     : Label_Type;
   L2     : Label_Type;
   L3     : Label_Type;
   L4     : Label_Type;
   L5     : Label_Type;

   procedure Do_On_Click (Window : in out Base_Window_Type'Class) is
   begin
      Dock (L5, Dock_Type'Succ (Dock (L5)));
      Size (L5, 25, 25);
      if Dock (L5) = Fill then
         Dock (L5, At_Top);
      end if;

      Dock_Children (Main_Window_Type (Parent (Window).all));
      Redraw (Window_Type (Parent (Window).all));
   end Do_On_Click;

begin
   Create (Window, "Dock Test Window - Please resize me!");

   Create (Button, Window, "&Done", 0, 0, 25, 25);
   Dock (Button, Fill);

   Create (L1, Window, "L1", 0, 0, 25, 25, Center);
   Dock (L1, At_Top);
   Border (L1);

   Create (L2, Window, "L2", 0, 0, 25, 25, Center);
   Dock (L2, At_Bottom);
   Border (L2);

   Create (L3, Window, "L3", 0, 0, 25, 25, Center);
   Dock (L3, At_Left);
   Border (L3);

   Create (L4, Window, "L4", 0, 0, 25, 25, Center);
   Dock (L4, At_Right);
   Border (L4);

   Create (L5, Window, "Float", 0, 0, 25, 25, Center);
   Dock (L5, At_Left);
   Border (L5);
   On_Click_Handler (L5, Do_On_Click'Unrestricted_Access);

   Dock_Children (Window);

   Show (Window);

   GWindows.Application.Message_Loop;

end Dock_Test;
