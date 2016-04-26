with Ada.Text_IO;                       use Ada.Text_IO;

-- Project specific:
with RC_Help;

procedure YYError (s: in String) is
begin
  for p in RC_Help.Pkg_output loop
    if Is_Open(RC_Help.Ada_files(p)) then
      Close(RC_Help.Ada_files(p));
    end if;
  end loop;
  Put_Line(Current_Error,s);
  raise RC_Help.Syntax_Error;
end YYError;
