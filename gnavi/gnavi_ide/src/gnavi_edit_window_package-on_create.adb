with GWindows.Application;
with GWindows.Drawing_Objects;

--  GNAVI: No user changes may be made here

separate (GNAVI_Edit_Window_Package)
procedure On_Create (Window : in out GNAVI_Edit_Window_Type)
is
begin
   Width (Window, 500);
   Height (Window, 400);
   Set_Standard_Font (Window, GWindows.Drawing_Objects.Default_GUI);
   Visible (Window, True);
   Text (Window, "Edit Window");
On_Create_Handler (Window, Do_Create'Access);
On_Close_Handler (Window, Do_Close'Access);
On_Menu_Hover_Handler (Window, Check_Menu'Access);
On_Menu_Select_Handler (Window, Handle_Menu'Access);
   GWindows.Packing_Boxes.Create (Window.Button_Pack_Box,
      Parent => Window,
      direction => GWindows.Packing_Boxes.Horizontal,
      height => 25,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Padding (Window.Button_Pack_Box, 5);
   GWindows.Packing_Boxes.Fill_Width (Window.Button_Pack_Box, True);
   GWindows.Packing_Boxes.Insets (Window.Button_Pack_Box, (3, 3, 3, 3));
   GWindows.Packing_Boxes.Dock (Window.Button_Pack_Box, GWindows.Base.At_Top);
   GWindows.Buttons.On_Click_Handler (Window.Save_Button, Do_Save_Window'Access);
   GWindows.Buttons.Create (Window.Save_Button,
      Parent => Window.Button_Pack_Box,
      height => 20,
      left => 0,
      text => "Save",
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Create (Window.Tab_Pack_Box,
      Parent => Window,
      direction => GWindows.Packing_Boxes.Horizontal,
      height => 25,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Padding (Window.Tab_Pack_Box, 5);
   GWindows.Packing_Boxes.Fill_Width (Window.Tab_Pack_Box, True);
   GWindows.Packing_Boxes.Insets (Window.Tab_Pack_Box, (3, 3, 3, 3));
   GWindows.Packing_Boxes.Dock (Window.Tab_Pack_Box, GWindows.Base.At_Bottom);
   GWindows.Buttons.On_Click_Handler (Window.Spec_Button, Do_Spec_Window'Access);
   GWindows.Buttons.Create (Window.Spec_Button,
      Parent => Window.Tab_Pack_Box,
      height => 20,
      left => 0,
      text => "Spec",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Body_Button, Do_Body_Window'Access);
   GWindows.Buttons.Create (Window.Body_Button,
      Parent => Window.Tab_Pack_Box,
      height => 20,
      left => 0,
      text => "Body",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.XML_Button, Do_XML_Window'Access);
   GWindows.Buttons.Create (Window.XML_Button,
      Parent => Window.Tab_Pack_Box,
      height => 20,
      left => 0,
      text => "XML",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Outline_Button, Do_Outline_Window'Access);
   GWindows.Buttons.Create (Window.Outline_Button,
      Parent => Window.Tab_Pack_Box,
      height => 20,
      left => 0,
      text => "Outline",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Layout_Button, Do_Layout_Window'Access);
   GWindows.Buttons.Create (Window.Layout_Button,
      Parent => Window.Tab_Pack_Box,
      height => 20,
      left => 0,
      text => "Layout",
      top => 0,
      width => 1);
   GWindows.Scintilla.Create (Window.Body_Edit_Box,
      Parent => Window,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Scintilla.Create (Window.Spec_Edit_Box,
      Parent => Window,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Scintilla.Create (Window.XML_Edit_Box,
      Parent => Window,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Create (Window.Outline_Box,
      Parent => Window,
      direction => GWindows.Packing_Boxes.Horizontal,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Padding (Window.Outline_Box, 5);
   GWindows.Packing_Boxes.Fill_Height (Window.Outline_Box, True);
   GWindows.Packing_Boxes.Fill_Width (Window.Outline_Box, True);
   GWindows.Packing_Boxes.Insets (Window.Outline_Box, (3, 3, 3, 3));
   GWindows.Packing_Boxes.Dock (Window.Outline_Box, GWindows.Base.Fill);
   GWindows.Panels.Create (Window.Outline_Panel,
      Parent => Window.Outline_Box,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.List_Boxes.Create (Window.Outline_View,
      Parent => Window.Outline_Panel,
      height => 10,
      left => 0,
      sort => False,
      top => 0,
      width => 10);
   GWindows.List_Boxes.Dock (Window.Outline_View, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.Outline_View, Do_Select_OV_Control'Access);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.Outline_View, Do_Jump_To_Control'Access);
   GWindows.Packing_Boxes.Create (Window.Outline_Control_Panel,
      Parent => Window.Outline_Panel,
      direction => GWindows.Packing_Boxes.Horizontal,
      height => 25,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Padding (Window.Outline_Control_Panel, 5);
   GWindows.Packing_Boxes.Fill_Width (Window.Outline_Control_Panel, True);
   GWindows.Packing_Boxes.Insets (Window.Outline_Control_Panel, (3, 3, 3, 3));
   GWindows.Packing_Boxes.Dock (Window.Outline_Control_Panel, GWindows.Base.At_Bottom);
   GWindows.Buttons.On_Click_Handler (Window.Add_Control_Button, Do_Add_Control'Access);
   GWindows.Buttons.Create (Window.Add_Control_Button,
      Parent => Window.Outline_Control_Panel,
      height => 20,
      left => 0,
      text => "+",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Delete_Control_Button, Do_Delete_Control'Access);
   GWindows.Buttons.Create (Window.Delete_Control_Button,
      Parent => Window.Outline_Control_Panel,
      height => 20,
      left => 0,
      text => "-",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Up_Control_Button, Do_Control_Up'Access);
   GWindows.Buttons.Create (Window.Up_Control_Button,
      Parent => Window.Outline_Control_Panel,
      height => 20,
      left => 0,
      text => "Up",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Down_Control_Button, Do_Control_Down'Access);
   GWindows.Buttons.Create (Window.Down_Control_Button,
      Parent => Window.Outline_Control_Panel,
      height => 20,
      left => 0,
      text => "Down",
      top => 0,
      width => 1);
   GWindows.Buttons.On_Click_Handler (Window.Right_Control_Button, Do_Control_Right'Access);
   GWindows.Buttons.Create (Window.Right_Control_Button,
      Parent => Window.Outline_Control_Panel,
      height => 20,
      left => 0,
      text => ">",
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Create (Window.Properties_Section,
      Parent => Window.Outline_Box,
      direction => GWindows.Packing_Boxes.Vertical,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Packing_Boxes.Padding (Window.Properties_Section, 5);
   GWindows.Packing_Boxes.Fill_Height (Window.Properties_Section, True);
   GWindows.Packing_Boxes.Fill_Width (Window.Properties_Section, True);
   GWindows.Packing_Boxes.Insets (Window.Properties_Section, (3, 3, 3, 3));
   GWindows.Panels.Create (Window.Properties_Box,
      Parent => Window.Properties_Section,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Create (Window.Property_Edit_Panel,
      Parent => Window.Properties_Box,
      height => 25,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Dock (Window.Property_Edit_Panel, GWindows.Base.At_Top);
   GWindows.Buttons.Create (Window.Property_Set_Button,
      Parent => Window.Property_Edit_Panel,
      height => 25,
      left => 0,
      text => "SET",
      top => 0,
      width => 25);
   GWindows.Buttons.Dock (Window.Property_Set_Button, GWindows.Base.At_Right);
   GWindows.Buttons.On_Click_Handler (Window.Property_Set_Button, Do_Property_Value_Change'Access);
   GWindows.Edit_Boxes.Create (Window.Property_Edit_Box,
      Parent => Window.Property_Edit_Panel,
      height => 25,
      left => 0,
      text => "",
      top => 0,
      width => 0);
   GWindows.Edit_Boxes.Dock (Window.Property_Edit_Box, GWindows.Base.Fill);
   GWindows.List_Boxes.Create (Window.Properties_View,
      Parent => Window.Properties_Box,
      height => 1,
      left => 0,
      sort => False,
      top => 0,
      width => 1);
   GWindows.List_Boxes.Dock (Window.Properties_View, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.Properties_View, Do_Property_Change'Access);
   GWindows.Panels.Create (Window.Handlers_Box,
      Parent => Window.Properties_Section,
      height => 1,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Create (Window.Handler_Edit_Panel,
      Parent => Window.Handlers_Box,
      height => 25,
      left => 0,
      top => 0,
      width => 1);
   GWindows.Panels.Dock (Window.Handler_Edit_Panel, GWindows.Base.At_Top);
   GWindows.Buttons.Create (Window.Handler_Set_Button,
      Parent => Window.Handler_Edit_Panel,
      height => 25,
      left => 0,
      text => "SET",
      top => 0,
      width => 25);
   GWindows.Buttons.Dock (Window.Handler_Set_Button, GWindows.Base.At_Right);
   GWindows.Buttons.On_Click_Handler (Window.Handler_Set_Button, Do_Handler_Value_Change'Access);
   GWindows.Edit_Boxes.Create (Window.Handler_Edit_Box,
      Parent => Window.Handler_Edit_Panel,
      height => 25,
      left => 0,
      text => "",
      top => 0,
      width => 0);
   GWindows.Edit_Boxes.Dock (Window.Handler_Edit_Box, GWindows.Base.Fill);
   GWindows.List_Boxes.Create (Window.Handlers_View,
      Parent => Window.Handlers_Box,
      height => 1,
      left => 0,
      sort => False,
      top => 0,
      width => 1);
   GWindows.List_Boxes.Dock (Window.Handlers_View, GWindows.Base.Fill);
   GWindows.List_Boxes.On_Selection_Change_Handler (Window.Handlers_View, Do_Handler_Change'Access);
   GWindows.List_Boxes.On_Double_Click_Handler (Window.Handlers_View, Do_Jump_To_Handler'Access);
   GWindows.Scroll_Panels.Create (Window.Layout_Box,
      Parent => Window,
      height => 100,
      left => 0,
      top => 0,
      width => 100);
   GWindows.Scroll_Panels.Visible (Window.Layout_Box, False);
   GWindows.Scroll_Panels.Dock (Window.Layout_Box, GWindows.Base.None);
   GWindows.Windows.MDI.On_Create (GWindows.Windows.MDI.MDI_Child_Window_Type (Window));
end On_Create;
