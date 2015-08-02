with Ada.Calendar;

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Base; use GWindows.Base;
--  with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Common_Controls; use GWindows.Common_Controls;
with GWindows.Image_Lists; use GWindows.Image_Lists;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;
with GWindows.Application;

procedure Control_Test is
   pragma Linker_Options ("control_test.coff");
   --  Resource with the toolbar bitmap. File obtained with:
   --  windres -i control_test.rc -o ..\..\obj\control_test.coff

   Top_Window   : Main_Window_Type;
   Window       : Window_Type;
   Date_Control : Date_Time_Picker_Type;
   IP_Control   : IP_Address_Control_Type;
   Tool_Tip     : Tool_Tip_Type;
   Prog_Control : Progress_Control_Type;
   List_Control : List_View_Control_Type;
   Tree_Control : Tree_View_Control_Type;
   Bar_Control  : Trackbar_Control_Type;
   Buddy_Box    : Edit_Box_Type;
   Up_Control   : Up_Down_Control_Type;
   Tab_Control  : Tab_Window_Control_Type;
   W1           : aliased Window_Type;
   W2           : aliased Window_Type;
   Toolbar      : Toolbar_Control_Type;
   Images       : Image_List_Type;

   procedure Do_Change (Window : in out Base_Window_Type'Class) is
   begin
      Text (Control_Test.Window, Text (Window));
      Step (Prog_Control);
   end Do_Change;

   procedure Do_Pos_Change (Window         : in out Base_Window_Type'Class;
                            Position       : in     Integer;
                            Delta_Position : in     Integer)
   is
   pragma Unreferenced (Window);
   begin
      Text (Top_Window, To_GString_From_String
            (Integer'Image (Position + Delta_Position)));
   end Do_Pos_Change;

   procedure Do_Track_Change (Window  : in out Base_Window_Type'Class;
                              Request : in     Scroll_Request_Type)
   is
   pragma Unreferenced (Window, Request);
   begin
      Text (Top_Window,
            To_GString_From_String (Integer'Image (Position (Bar_Control))));
   end Do_Track_Change;

   procedure Do_Tree_Pick (Window : in out Base_Window_Type'Class)
   is
   pragma Unreferenced (Window);
   begin
      GWindows.Message_Boxes.Message_Box
        ("Your Pick", Selected_Item (Tree_Control));
   end Do_Tree_Pick;

   procedure Do_Menu_Select (Window : in out Base_Window_Type'Class;
                             Item   : in     Integer)
   is
   pragma Unreferenced (Window);
   begin
      GWindows.Message_Boxes.Message_Box
        ("Command Sent", To_GString_From_String (Item'Img));
   end Do_Menu_Select;

   Start_Date : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   End_Date   : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (2030, 3, 22);

begin
   Create (Top_Window, "Control Test");

   Create_As_Control (Window, Top_Window, "", 0, 0, 0, 0);
   Keyboard_Support (Window);
   Border (Window);
   Dock (Window, Fill);

   Create (Toolbar, Top_Window, 0, 0, 0, 40);
   Create (Images, "#101", 16);
   Set_Image_List (Toolbar, Images);
   Add_Button (Toolbar, 0, 101);
   Add_Button (Toolbar, 1, 102);
   Add_Button (Toolbar, 2, 103);
   On_Button_Select_Handler (Toolbar, Do_Menu_Select'Unrestricted_Access);
   Dock (Toolbar, At_Top);

   ----------------------------------
   --  Date_Time_Picker_Type demo  --
   ----------------------------------

   Create (Date_Control, Window, 0, 0, 400, 25);
   On_Focus_Handler (Date_Control, Do_Change'Unrestricted_Access);

   Date_Time_Format
      (Date_Control, "'Today is: 'HH':'m':'s dddd MMM dd', 'yyyy");

   Set_Range (Date_Control,
              Start_Date,
              End_Date);

   Create (IP_Control, Window, 0, 50, 175, 25);
   On_Focus_Handler (IP_Control, Do_Change'Unrestricted_Access);
   Text (IP_Control, "128.0.0.1");

   Create (Tool_Tip, Window);
   Add_Tool_Tip (Tool_Tip, Ip_Control, "I am a tool tip on the IP Control");
   Maximum_Width (Tool_Tip, 150);

   Create (Prog_Control, Top_Window, 0, 0, 10, 25);
   Dock (Prog_Control, At_Bottom);
   Step_Size (Prog_Control, 5);

   Create (List_Control, Window, 0, 100, 200, 100,
           View => Report_View);

   Insert_Column (List_Control, "Item", 0, 75);
   Insert_Column (List_Control, "Sub_Item", 1, 100);

   for N in 0 .. 5 loop
      Insert_Item (List_Control, To_GString_From_String (N'Img), N);
      Set_Sub_Item (List_Control,
                    To_GString_From_String ("Sub of" & N'Img),
                    N,
                    1);
   end loop;

   Create (Tree_Control, Window, 300, 100, 200, 100);
   On_Double_Click_Handler (Tree_Control, Do_Tree_Pick'Unrestricted_Access);
   declare
      Root_Node : Tree_Item_Node;
      An_Item   : Tree_Item_Node;
   begin
      Insert_Item (Tree_Control, "Root", 0, Root_Node, As_A_Root);
      Insert_Item (Tree_Control, "Child1", Root_Node, An_Item);
      Insert_Item (Tree_Control, "Child2", Root_Node, An_Item);
      Insert_Item (Tree_Control, "Child3", Root_Node, An_Item);
      Insert_Item (Tree_Control, "Sub-Child1", An_Item, An_Item);
      Insert_Item (Tree_Control, "Sub-Child2", An_Item, An_Item);
      Insert_Item (Tree_Control, "Sub-Child3", An_Item, An_Item);
   end;

   Create (Bar_Control, Window, 0, 300, 200, 25);
   On_Horizontal_Scroll_Handler (Bar_Control,
                                 Do_Track_Change'Unrestricted_Access);
   Minimum (Bar_Control, 20);
   Maximum (Bar_Control, 180);

   Create (Buddy_Box, Window, "", 300, 300, 90, 25);
   Create (Up_Control, Window, 300, 300, 50, 50);
   Set_Range (Up_Control, 40, 80);
   Position (Up_Control, 50);
   On_Position_Changing_Handler (Up_Control,
                                 Do_Pos_Change'Unrestricted_Access);

   Create (Tab_Control, Window, 300, 375, 300, 150);
   Insert_Tab (Tab_Control, 0, "Tab1");
   Insert_Tab (Tab_Control, 1, "Tab2");

   Create_As_Control (W1, Tab_Control, "", 0, 0, 0, 0, Show => False);
   Create_As_Control (W2, Tab_Control, "", 0, 0, 0, 0, Show => False);

   Tab_Window (Tab_Control, 0, W1'Unchecked_Access);
   Tab_Window (Tab_Control, 1, W2'Unchecked_Access);

   declare
      EB : Edit_Box_Access;
--      BB : Button_Access;
   begin
      EB := new Edit_Box_Type;
      Create (EB.all, W1, "Window 1", 10, 10, 100, 25, Is_Dynamic => True);
      EB := new Edit_Box_Type;
      Create (EB.all, W2, "Window 2", 10, 10, 100, 25, Is_Dynamic => True);

--        BB := new Button_Type;
--        Create (BB.all, W1, "Button 1", 10, 50, 75, 25, Is_Dynamic => True);
--        BB := new Button_Type;
--        Create (BB.all, W2, "Button 2", 10, 50, 75, 25, Is_Dynamic => True);
   end;

   Dock_Children (Top_Window);
   Show (Top_Window);

   GWindows.Application.Message_Loop;
end Control_Test;
