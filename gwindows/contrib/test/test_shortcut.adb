--  Demo for the GWin_Util package, Create_Desktop_Shortcut procedure.
--
--  See: gwindows\contrib, gwindows\gwindows_contrib.gpr, gwindows\contrib\test

with Ada.Command_Line;
with GWin_Util;
with GWindows.Message_Boxes;

procedure Test_Shortcut is
begin
  GWin_Util.Create_Desktop_Shortcut (
    "Such a nice Shortcut!",
    Ada.Command_Line.Command_Name,
    All_Users => False
  );
  GWindows.Message_Boxes.Message_Box (
    "Desktop Shortcut",
    "Shortcut to myself has been created."
  );
end Test_Shortcut;
