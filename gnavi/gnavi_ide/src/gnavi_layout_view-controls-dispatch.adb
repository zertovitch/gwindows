--  This file contains only automatically generated code.
--  DO NOT MODIFY !
--  Change the `datastore\controls.xml` file instead.
--  Generated by: Control_Generator.

with GNAVI_Window;
with Control_Creators; use Control_Creators;

separate (GNAVI_Layout_View.Controls)
function Dispatch
     (Parent      : GWindows.Base.Pointer_To_Base_Window_Class;
      Control_XML : GNAVI_Window.Control_Element)
     return GWindows.Base.Pointer_To_Base_Window_Class
is
   T : constant GWindows.GString := GNAVI_Window.Control_Type (Control_XML);
begin
   if False then
      return null;
   elsif T = "GWindows.Static_Controls.Label_Type" then
      return Create_Index_1 (Parent, Control_XML);
   elsif T = "GWindows.Static_Controls.Icon_Type" then
      return Create_Index_2 (Parent, Control_XML);
   elsif T = "GWindows.Static_Controls.Bitmap_Type" then
      return Create_Index_3 (Parent, Control_XML);
   elsif T = "GWindows.Static_Controls.Meta_File_Type" then
      return Create_Index_4 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Button_Type" then
      return Create_Index_5 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Default_Button_Type" then
      return Create_Index_6 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Cancel_Button_Type" then
      return Create_Index_7 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Default_Cancel_Button_Type" then
      return Create_Index_8 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Check_Box_Type" then
      return Create_Index_9 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Two_State_Button_Type" then
      return Create_Index_10 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Radio_Button_Type" then
      return Create_Index_11 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Push_Radio_Button_Type" then
      return Create_Index_12 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Three_State_Box_Type" then
      return Create_Index_13 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Three_State_Button_Type" then
      return Create_Index_14 (Parent, Control_XML);
   elsif T = "GWindows.Edit_Boxes.Edit_Box_Type" then
      return Create_Index_15 (Parent, Control_XML);
   elsif T = "GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type" then
      return Create_Index_16 (Parent, Control_XML);
   elsif T = "GWindows.Edit_Boxes.Rich.Rich_Edit_Box_Type" then
      return Create_Index_17 (Parent, Control_XML);
   elsif T = "GWindows.Scintilla.Scintilla_Type" then
      return Create_Index_18 (Parent, Control_XML);
   elsif T = "GWindows.List_Boxes.List_Box_Type" then
      return Create_Index_19 (Parent, Control_XML);
   elsif T = "GWindows.List_Boxes.Multiple_Selection_List_Box_Type" then
      return Create_Index_20 (Parent, Control_XML);
   elsif T = "GWindows.Combo_Boxes.Combo_Box_Type" then
      return Create_Index_21 (Parent, Control_XML);
   elsif T = "GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type" then
      return Create_Index_22 (Parent, Control_XML);
   elsif T = "GWindows.Combo_Boxes.Drop_Down_List_Box_Type" then
      return Create_Index_23 (Parent, Control_XML);
   elsif T = "GWindows.Common_Controls.List_View_Control_Type" then
      return Create_Index_24 (Parent, Control_XML);
   elsif T = "GWindows.Buttons.Group_Box_Type" then
      return Create_Index_25 (Parent, Control_XML);
   elsif T = "GWindows.Panels.Panel_Type" then
      return Create_Index_26 (Parent, Control_XML);
   elsif T = "GWindows.Scroll_Panels.Scroll_Panel_Type" then
      return Create_Index_27 (Parent, Control_XML);
   elsif T = "GWindows.Packing_Boxes.Packing_Box_Type" then
      return Create_Index_28 (Parent, Control_XML);
   elsif T = "GWindows.Drawing_Panels.Drawing_Panel_Type" then
      return Create_Index_29 (Parent, Control_XML);
   elsif T = "GWindows.Scroll_Bars.Scroll_Bar_Type" then
      return Create_Index_30 (Parent, Control_XML);
   elsif T = "GWindows.Common_Controls.Up_Down_Control_Type" then
      return Create_Index_31 (Parent, Control_XML);
   elsif T = "GWindows.Common_Controls.Progress_Control_Type" then
      return Create_Index_32 (Parent, Control_XML);
   elsif T = "GWindows.GControls.GSize_Bars.GSize_Bar_Type" then
      return Create_Index_33 (Parent, Control_XML);
   elsif T = "GWindows.GControls.Duration.Duration_Type" then
      return Create_Index_34 (Parent, Control_XML);
   elsif T = "GWindows.GControls.Duration.Edit.Duration_Edit_Type" then
      return Create_Index_35 (Parent, Control_XML);

   else
      return null;
   end if;
end Dispatch;
