-- Generates an Ada code generation code from an Ada source

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Gen_Gen is
  s: String(1..1000);
  l: Natural;
begin
  while not End_Of_File loop
    Get_Line(s,l);
    if l = 0 then
      Put_Line("Ada_New_Line(to_body);");
    else
      Put("Ada_Put_Line(to_body, """);
      for i in 1..l loop
        if s(i)= '"' then
          Put(""""""); -- double ", hence written 4x+2 times !
        else
          Put(s(i));
        end if;
      end loop;
      Put_Line(""");");
    end if;
  end loop;
end;