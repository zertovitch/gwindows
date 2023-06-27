with Ada.Text_IO;

--  Project specific:
with RC_Help;
with RC_IO;

procedure YYError (s: in String) is
  use Ada.Text_IO;
begin
  for p in RC_Help.Pkg_output loop
    if Is_Open (RC_Help.Ada_files (p)) then
      Close (RC_Help.Ada_files (p));
    end if;
  end loop;
  Put_Line (Current_Error, s);
  New_Line (Current_Error);
  Put_Line
    (Current_Error, "Syntax error in RC file, line number:" &
     RC_IO.Input_Line'Image);
  --  ^ NB: the function `RC_IO.Input_Line` is generated
  --  or is working correctly *only* when AFlex has been called
  --  with the `-E` option.

  raise RC_Help.Syntax_Error
    with
      "Possible cause is the eventual Unicode text format of .rc file. " &
      "RC parser needs ANSI format. " &
      "You can change it with Notepad++ for instance.";
end YYError;
