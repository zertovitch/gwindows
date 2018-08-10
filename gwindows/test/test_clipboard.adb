with GWindows.Clipboard;

with GWindows;                    use GWindows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.Windows;            use GWindows.Windows;

procedure Test_Clipboard is
   some_window : Window_Type;
   some_text : constant GString :=
      "GWindows' Test_Clipboard message. First line here..." &
      GCharacter'Val (13) &  --  <- only needed by a few apps like Notepad...
      GCharacter'Val (10) &
      "...second line here.";
begin
   Create (some_window, "Just a window...");
   --
   --  Test 1: Get text (CF_TEXT or CF_UNICODETEXT) from the Windows clipboard
   --
   Message_Box (
      "Test 1",
      "Test 1: getting a text from the Windows clipboard." &
      GCharacter'Val (10) &
      GCharacter'Val (10) &
      "Please copy a text, then press return here in this app."
   );
   Message_Box (
      some_window,
      "Contents of the clipboard",
      GWindows.Clipboard.Clipboard_Text (some_window)
   );
   --
   --  Test 2: Put text (CF_TEXT or CF_UNICODETEXT) to the Windows clipboard
   --
   GWindows.Clipboard.Clipboard_Text (some_window, some_text);
   Message_Box (
      "Test 2",
      "Test 2: putting a text to the Windows clipboard." &
      GCharacter'Val (10) &
      GCharacter'Val (10) &
      "I have put a message on the clipboard, please " &
      "paste it into another application."
   );
end Test_Clipboard;
