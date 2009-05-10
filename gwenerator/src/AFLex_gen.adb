-- Generates AFLEX tokens code from a list

with Ada.Text_IO;                       use Ada.Text_IO;

procedure AFLex_Gen is
  s: String(1..1000);
  l: Natural;
begin
  while not End_Of_File loop
    Get_Line(s,l);
    Put(s(1..l));
    for i in l..30 loop
      Put(' ');
    end loop;
    Put_Line("{ return " & s(1..l) & "_t; }");
  end loop;
end;