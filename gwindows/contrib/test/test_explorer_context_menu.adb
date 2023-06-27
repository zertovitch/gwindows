--  Demo for the GWin_Util package, Explorer_Context_Menu procedure.
--
--  See: gwindows\contrib\, gwindows\gwindows_contrib.gpr,
--       gwindows\contrib\test\

with GWin_Util;
with GWindows.Message_Boxes;

with Ada.Command_Line,
     Ada.Environment_Variables;

procedure Test_Explorer_Context_Menu is
  use Ada.Command_Line, Ada.Environment_Variables, GWindows.Message_Boxes, GWin_Util;
begin
  if Argument_Count = 0 then
    Message_Box (
      "Test Explorer Context Menu",
      "I'll set up the Explorer context menu for: " & NL &
      "  - files" & NL &
      "  - folders." & NL & NL &
      "When right-clicking anything, spot the Regedit icon and the ""GWin_Util Demo"" label.",
      Icon => Information_Icon
    );
    for Subject in Context_Menu_Subject loop
      Explorer_Context_Menu (
        Entry_Name  => "Test_Explorer_Context_Menu",
        Subject     => Subject,
        User_Scope  => Current_User,
        Action      => Add,
        Entry_Label => "GWin_Util Demo for " & (if Subject = Any_File then "File" else "Folder"),
        Command     => S2G (Command_Name) & " ""%1""",
        Icon_Path   => S2G (Value ("windir")) & "\regedit.exe"
      );
    end loop;
  else
    Message_Box (
      "Test Explorer Context Menu",
      "You just right-clicked: " & NL & NL &
      S2G (Argument (1)) & NL & NL &
      "Now, I'll remove the demo context menu entries." & NL &
      "Re-run me without parameter for recreating them again.",
      Icon => Information_Icon
    );
    for Subject in Context_Menu_Subject loop
      Explorer_Context_Menu (
        Entry_Name => "Test_Explorer_Context_Menu",
        Subject    => Subject,
        User_Scope => Current_User,
        Action     => Remove
      );
    end loop;
  end if;
end Test_Explorer_Context_Menu;
