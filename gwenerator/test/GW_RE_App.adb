-- ResEdit -> GWindows test application

with GWindows;                    use GWindows;
with GWindows.Application;        use GWindows.Application;
with GWindows.Base;               use GWindows.Base;
with GWindows.Buttons;            use GWindows.Buttons;
with GWindows.Common_Controls;    use GWindows.Common_Controls;
with GWindows.Edit_Boxes;         use GWindows.Edit_Boxes;
with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Windows;            use GWindows.Windows;
with GWindows.Windows.Main;       use GWindows.Windows.Main;
with GWindows.Menus;              use GWindows.Menus;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

with GW_RE_App_Resource_GUI;      use GW_RE_App_Resource_GUI;

procedure GW_RE_App is

  pragma Linker_Options ("-mwindows");

  Dialog_1  : GW_RE_App_Resource_GUI.Dialog_without_picture_Type;
  Dialog_2  : GW_RE_App_Resource_GUI.Dialog_without_picture_adv_controls_Type;
  Dialog_3  : GW_RE_App_Resource_GUI.Dialog_with_pictures_Type;
  menu_1    : GW_RE_App_Resource_GUI.Menu_gourmet_Type;
  Dlg_Statix: GW_RE_App_Resource_GUI.Static_Borders_Type;
  Dlg_LVs   : GW_RE_App_Resource_GUI.Dialog_ListViews_Type;

  Result      : Integer;
  No_Parent   : Window_Type;
  some_window : Main_Window_Type;
  Final_text  : GString_Unbounded;
  type Radio_choice_type is (None, AM, FM);
  radio_choice: Radio_choice_type;
  
  procedure Get_Data(Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Final_text:= To_GString_Unbounded(Text(Dialog_1.Edit_Box_1));
    if State (Dialog_1.IDC_RADIO1) = Checked then
      radio_choice := AM;
    elsif State (Dialog_1.IDC_RADIO2) = Checked then
      radio_choice := FM;
    else
      radio_choice := None;
    end if;
  end Get_Data;

  NL : GCharacter := GCharacter'Val(10);

begin
  Create_Full_Dialog (Dlg_Statix, No_Parent);
  Center(Dlg_Statix);
  Result := GWindows.Application.Show_Dialog (Dlg_Statix);
  --
  Create_Full_Dialog (Dlg_LVs, No_Parent);
  Center(Dlg_LVs);
  Result := GWindows.Application.Show_Dialog (Dlg_LVs);
  --
  --  Dialog with radio buttons, check boxes,
  --  combo boxes, list boxes, edit boxes, ...
  --
  Create_Full_Dialog (Dialog_1, No_Parent);
  On_Destroy_Handler (Dialog_1, Get_Data'Unrestricted_Access);
  Center(Dialog_1);
  Text(Dialog_1.Edit_Box_1, "Type some text here...");
  Result := GWindows.Application.Show_Dialog (Dialog_1);
  Message_Box (
    "Info",
    "Text in the edit box: [" & 
    To_GString_From_Unbounded (Final_text) & "]." & NL &
    "Radio choice (None, AM, FM) is: " &
    To_GString_From_String(Radio_choice_type'Image(radio_choice))
  );
  --
  Create_Full_Dialog (Dialog_2, No_Parent);
  Center(Dialog_2);
  Result := GWindows.Application.Show_Dialog (Dialog_2);
  --
  Create_Full_Dialog (Dialog_3, No_Parent);
  Center(Dialog_3);
  Result := GWindows.Application.Show_Dialog (Dialog_3);
  --
  -- Test menus
  --
  Create(some_window, "Look at the nice menus!");
  Create_Full_menu(menu_1);
  Menu(some_window, menu_1.main);
  Visible(some_window, True);
  Message_Loop; -- wait the window is closed
  --
end GW_RE_App;
