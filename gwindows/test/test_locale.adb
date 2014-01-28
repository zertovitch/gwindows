with GWindows.Locale;

with GWindows;                    use GWindows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

procedure Test_Locale is
  NL : constant GCharacter := GCharacter'Val (10); -- New Line for message box
begin
   Message_Box (
      "Some locale (regional settings)",
      "Decimal separator is [" &
         GWindows.Locale.Get_Decimal_Separator & ']' &
      NL &
      "Thousands separator is [" &
         GWindows.Locale.Get_Thousands_Separator & ']'
   );
end Test_Locale;
