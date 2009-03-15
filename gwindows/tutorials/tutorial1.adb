with GWindows.GStrings;
with GWindows.Message_Boxes;

procedure Tutorial1 is
   use GWindows.Message_Boxes;

   Result : Message_Box_Result;
begin
   Message_Box (Title => "Tutorial1",
                Text  => "My first GWindows Application",
                Icon  => Exclamation_Icon);

   Result := Message_Box (Title => "Tutorial1",
                          Text  => "Nice GUI huh?",
                          Style => Yes_No_Box,
                          Icon  => Question_Icon);

   if Result = Yes then
      Message_Box ("Cool....", "I like your answer");
   else
      Message_Box ("What....", "You have no taste");
   end if;

   for I in 1 .. 3 loop
      Message_Box ("A Number",
                   GWindows.GStrings.To_GString_From_String (I'Img));
   end loop;
end Tutorial1;
