with Ada.Text_IO;                       use Ada.Text_IO;

--  Project specific:
with RC_Help;

procedure YYError (s: in String) is
begin
  for p in RC_Help.Pkg_output loop
    if Is_Open(RC_Help.Ada_files(p)) then
      Close(RC_Help.Ada_files(p));
    end if;
  end loop;
  Put_Line(Current_Error,s);
  raise RC_Help.Syntax_Error
    with
      "Possible cause is the eventual Unicode text format of .rc file. " &
      "RC parser needs ANSI format. " &
      "You can change it with Notepad++ for instance.";
end YYError;
