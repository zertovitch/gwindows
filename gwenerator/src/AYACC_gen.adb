-- Generates AYACC tokens code from a list

with Ada.Text_IO;                       use Ada.Text_IO;

procedure AYACC_Gen is
  s: String(1..1000);
  l: Natural;
begin
  Put("%token ");
  while not End_Of_File loop
    Get_Line(s,l);
    Put(s(1..l) & "_t");
    if not End_Of_File then
      Put(", ");
      if Col > 65 then
        New_Line;
        Put("       ");
      end if;
    end if;
  end loop;
  New_Line;
end;