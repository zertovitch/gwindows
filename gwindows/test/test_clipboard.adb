with GWindows.Clipboard,
     GWindows.GStrings,
     GWindows.Message_Boxes,
     GWindows.Windows;

procedure Test_Clipboard is

   use GWindows, GWindows.GStrings, GWindows.Message_Boxes, GWindows.Windows;

   some_window : Window_Type;

   CR : constant GCharacter := GCharacter'Val (13);
   LF : constant GCharacter := GCharacter'Val (10);
   NL : constant GString := CR & LF;
   --  ^ CR only needed by a few apps like Notepad

   some_text : constant GString :=
      "GWindows' Test_Clipboard message. First line here..." &
      NL &
      "...second line here." & NL &
      "Now the text you have copied before:" & NL;
begin
   Create (some_window, "Just a window...");
   --
   --  Test 1: Get text (CF_TEXT or CF_UNICODETEXT) from the Windows clipboard
   --
   Message_Box
     ("Test 1",
      "Test 1: getting a text from the Windows clipboard." &
      NL & NL &
      "Please copy a text, then press return here in this app." &
      NL &
      "If the GWindows character mode is Unicode, don't hesitate to copy " &
      "non-Latin characters! Mode = " &
      To_GString_From_String (Character_Mode_Type'Image (Character_Mode)));
   declare
      mem_text : GString := GWindows.Clipboard.Clipboard_Text (some_window);
   begin
      Message_Box
        (some_window,
         "Contents of the clipboard",
         mem_text);
      --
      --  Test 2: Put text (CF_TEXT or CF_UNICODETEXT) to the Windows clipboard
      --
      GWindows.Clipboard.Clipboard_Text (some_window, some_text & mem_text);
      Message_Box
        ("Test 2",
         "Test 2: putting a text to the Windows clipboard." &
         NL &
         NL &
         "I have put a message on the clipboard, please " &
         "paste it into another application.");
   end;
end Test_Clipboard;
