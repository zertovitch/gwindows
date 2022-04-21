-- Visual Studio 2008 -> GWindows test application

with GWindows.Base;               use GWindows.Base;
with GWindows.Edit_Boxes;         use GWindows.Edit_Boxes;
with GWindows.GStrings;
with GWindows.Windows;            use GWindows.Windows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.Application;

-- Resources are separate (we chose the -s option)
with VS_2008_Resource_GUI.MY_NICE_DIALOG;
 use VS_2008_Resource_GUI.MY_NICE_DIALOG;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure VS_2008 is

  use GWindows, GWindows.GStrings;

  pragma Linker_Options ("-mwindows");

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  Dialog_1  : VS_2008_Resource_GUI.MY_NICE_DIALOG.MY_NICE_DIALOG_Type;
  Result    : Integer;
  No_Parent : Window_Type;
  Final_text: GString_Unbounded;

  procedure Get_Data(Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Final_text:= To_GString_Unbounded (Text (Dialog_1.Simple_text_box));
  end Get_Data;

begin
  Create_Full_Dialog (Dialog_1, No_Parent);
  On_Destroy_Handler (Dialog_1, Get_Data'Unrestricted_Access);
  Center(Dialog_1);
  Result := GWindows.Application.Show_Dialog (Dialog_1);
  Message_Box ("Info", "Text in the edit box: [" & To_GString_From_Unbounded (Final_text) & "].");
end VS_2008;
