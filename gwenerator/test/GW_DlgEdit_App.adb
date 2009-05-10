-- MS Dialog Editor (DlgEdit) -> GWindows test application

with GWindows.Windows;            use GWindows.Windows;
with GWindows.Application;

with GW_DlgEdit_App_Resource_GUI;
 use GW_DlgEdit_App_Resource_GUI;

procedure GW_DlgEdit_App is

  pragma Linker_Options ("-mwindows");

  Dialog_1  : Big_Dialog_Type;
  No_Parent : Window_Type;
  REsult: Integer;

begin
  Create_Full_Dialog (Dialog_1, No_Parent);
  Center(Dialog_1);
  Result := GWindows.Application.Show_Dialog (Dialog_1);
end GW_DlgEdit_App;
