-- GWindows test application from a random .rc file found on the Web

with GWindows.Base;               use GWindows.Base;
with GWindows.Edit_Boxes;         use GWindows.Edit_Boxes;
with GWindows.Windows;            use GWindows.Windows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.Application;

with Web10_Resource_GUI;
 use Web10_Resource_GUI;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure Web10 is

  pragma Linker_Options ("-mwindows");

  Dlg1      : Web10_Resource_GUI.IDD_OPTIONDIALOG_Type;
  Dlg2      : Web10_Resource_GUI.IDD_FILETRANSFER_DLG_Type;
  Dlg3      : Web10_Resource_GUI.IDD_SESSION_DLG_Type;
  Result    : Integer;
  No_Parent : Window_Type;
  Final_text: Unbounded_String;

begin
  Create_Full_Dialog (dlg1, No_Parent);
  Center(dlg1);
  Result := GWindows.Application.Show_Dialog (dlg1);
  --
  Create_Full_Dialog (dlg2, No_Parent);
  Center(dlg2);
  Result := GWindows.Application.Show_Dialog (dlg2);
  --
  Create_Full_Dialog (dlg3, No_Parent);
  Center(dlg3);
  Result := GWindows.Application.Show_Dialog (dlg3);
end Web10;
