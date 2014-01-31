with GWindows.Locales;

with GWindows;                    use GWindows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

procedure Test_Locale is
  NL : constant GCharacter := GCharacter'Val (10); -- New Line for message box
begin
   Message_Box (
      "Some locale (regional settings)",
      "Decimal separator is [" &
         GWindows.Locales.Get_Decimal_Separator & ']' &
      NL &
      "Thousands separator is [" &
         GWindows.Locales.Get_Thousands_Separator & ']'
   );
end Test_Locale;
