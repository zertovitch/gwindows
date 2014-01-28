with GWindows.Clipboard;

with GWindows;                    use GWindows;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Windows;            use GWindows.Windows;

procedure Test_Clipboard is
   some_window : Window_Type;
   function GS (Value : String) return GString
     renames To_GString_From_String;
   some_text : constant String :=
      "GWindows' Test_Clipboard - first line" &
      ASCII.CR & -- <- only needed by a few apps like Notepad...
      ASCII.LF &
      "second line";
begin
   Create (some_window, "Just a window...");
   --
   --  Test 1: Get text (CF_TEXT) from the Windows clipboard
   --
   Message_Box (
      "Test 1",
      GS ("Test 1: getting a text from the Windows clipboard." &
         ASCII.LF &
         ASCII.LF &
         "Please copy a text, then press return."
      )
   );
   Message_Box (
      some_window,
      "Contents of the clipboard",
      GS (GWindows.Clipboard.Get_Clipboard_Text (some_window))
   );
   --
   --  Test 2: Put text (CF_TEXT) to the Windows clipboard
   --
   GWindows.Clipboard.Set_Clipboard_Text (some_window, some_text);
   Message_Box (
      "Test 2",
      GS ("Test 2: putting a text to the Windows clipboard." &
         ASCII.LF &
         ASCII.LF &
         "I have put something, please paste into another application."
      )
   );
end Test_Clipboard;
