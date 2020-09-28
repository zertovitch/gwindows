--  Demo for the GWin_Util package, Create_Desktop_Shortcut procedure.
--
--  See: gwindows\contrib\, gwindows\gwindows_contrib.gpr,
--       gwindows\contrib\test\

with Ada.Command_Line;
with GWin_Util;
with GWindows.Message_Boxes;

procedure Test_Shortcut is
  use GWindows.Message_Boxes, GWin_Util;
begin
  Create_Desktop_Shortcut (
    "Such a nice Shortcut!",
    Ada.Command_Line.Command_Name,
    Current_User
  );
  Message_Box (
    "Desktop Shortcut",
    "A shortcut to myself has just been created.",
    Icon => Information_Icon
  );
end Test_Shortcut;
