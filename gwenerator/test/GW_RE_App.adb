-- ResEdit -> GWindows test application

with GWindows.Application;        use GWindows.Application;
with GWindows.Base;               use GWindows.Base;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Edit_Boxes;         use GWindows.Edit_Boxes;
with GWindows.Windows;            use GWindows.Windows;
with GWindows.Windows.Main;       use GWindows.Windows.Main;
with GWindows.Menus;              use GWindows.Menus;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

with GW_RE_App_Resource_GUI;      use GW_RE_App_Resource_GUI;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure GW_RE_App is

  pragma Linker_Options ("-mwindows");

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  Dialog_1  : GW_RE_App_Resource_GUI.Dialog_without_picture_Type;
  Dialog_2  : GW_RE_App_Resource_GUI.Dialog_without_picture_adv_controls_Type;
  Dialog_3  : GW_RE_App_Resource_GUI.Dialog_with_pictures_Type;
  menu_1    : GW_RE_App_Resource_GUI.Menu_gourmet_Type;
  Dlg_Statix: GW_RE_App_Resource_GUI.Static_Borders_Type;
  Dlg_LVs   : GW_RE_App_Resource_GUI.Dialog_ListViews_Type;

  Result      : Integer;
  No_Parent   : Window_Type;
  some_window : Main_Window_Type;
  Final_text  : Unbounded_String;

  procedure Get_Data(Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Final_text:= U(Text(Dialog_1.Edit_Box_1));
  end Get_Data;

  procedure Populate_LV(lv: in out List_View_Control_Type) is
  begin
   Insert_Column (lv, "Item", 0, 75);
   Insert_Column (lv, "Sub_Item", 1, 100);

   for N in 0 .. 20 loop
      Insert_Item (lv, N'Img, N);
      Set_Sub_Item (lv,
                    "Sub of" & N'Img,
                    N,
                    1);
   end loop;
  end;

  root, node: Tree_Item_Node;

begin
  Create_Full_Dialog (Dlg_Statix, No_Parent);
  Center(Dlg_Statix);
  Result := GWindows.Application.Show_Dialog (Dlg_Statix);
  --
  Create_Full_Dialog (Dlg_LVs, No_Parent);
  Populate_LV(Dlg_LVs.IDC_LIST1);
  Populate_LV(Dlg_LVs.IDC_LIST2);
  Populate_LV(Dlg_LVs.IDC_LIST3);
  Populate_LV(Dlg_LVs.IDC_LIST4);
  Center(Dlg_LVs);
  Result := GWindows.Application.Show_Dialog (Dlg_LVs);
  --
  Create_Full_Dialog (Dialog_1, No_Parent);
  On_Destroy_Handler (Dialog_1, Get_Data'Unrestricted_Access);
  Center(Dialog_1);
  Text(Dialog_1.Edit_Box_1, "Type some text here...");
  Result := GWindows.Application.Show_Dialog (Dialog_1);
  Message_Box ("Info", "Text in the edit box: [" & S(Final_text) & "].");
  --
  Create_Full_Dialog (Dialog_2, No_Parent);
  Dialog_2.IDC_TREE1.Insert_Item("The Root", 0, root, As_a_root);
  Dialog_2.IDC_TREE1.Insert_Item("First level item 1", root, node);
  Dialog_2.IDC_TREE1.Insert_Item("First level item 2", root, node);
  Dialog_2.IDC_TREE1.Insert_Item("Second level item",  node, node);
  Dialog_2.IDC_TREE1.Insert_Item("First level item 3", root, node);
  Dialog_2.IDC_TREE1.Expand(root);
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
