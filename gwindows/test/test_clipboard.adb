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

begin
   Create (some_window, "Just a window...");
   some_window.Size (400, 200);
   some_window.Show;

   -----------------------------------------------------
   --  Test 1: Get a text from the Windows clipboard  --
   -----------------------------------------------------

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
      mem_text : constant GString := GWindows.Clipboard.Clipboard_Text (some_window);
      some_text : constant GString :=
         "GWindows' Test_Clipboard message. First line here..." & NL &
         "...second line here." & NL & NL &
         "Now, the text you have copied previously:" & NL;
   begin
      Message_Box
        (some_window,
         "Contents of the clipboard",
         mem_text);

      ---------------------------------------------------
      --  Test 2: Put a text to the Windows clipboard  --
      ---------------------------------------------------

      GWindows.Clipboard.Clipboard_Text (some_window, some_text & mem_text);
      Message_Box
        ("Test 2",
         "Test 2: putting a text to the Windows clipboard." &
         NL &
         NL &
         "I have put a message on the clipboard, please " &
         "paste it into another application.");
   end;

   ------------------------------------------------------------
   --  Test 3: Put a HTML fragment to the Windows clipboard  --
   ------------------------------------------------------------
   GWindows.Clipboard.Clipboard_HTML
      (some_window,
       "<!--StartFragment -->" &  --  Works also when comment is omitted.
       "<font face=""Calibri, Arial, Tahoma"">" &
       "Here is some <b>text</b> with a nice font " &
       "and even a bit of <b><font color=""#dd1111"">color!</font></b>" &
       "</font><br><br><br>" &
       "Some code..." & NL &
       "<pre>" &
       "<font face=""Consolas"">" &
       "<font color=""#0000bb"">with</font> Ada.Text_IO;" & NL & NL &
       "<font color=""#0000bb"">procedure</font> Hello <font color=""#0000bb"">is</font>" & NL &
       "<font color=""#0000bb"">begin</font>" & NL &
       "  Ada.Text_IO.Put (""Hello world!"");" & NL &
       "<font color=""#0000bb"">end</font>;" & NL &
       "</font>" &
       "</pre><br><br>" &
       "<table>" &
       "<tr><td>First column</td><td>Second column</td></tr>" &
       "<tr><td>1.1</td><td>1.2</td></tr>" &
       "<tr><td>2.1</td><td>2.2</td></tr>" &
       "<tr><td>3.1</td><td>3.2</td></tr>" &
       "</table>" &
       "<!--EndFragment-->");  --  Works also when comment is omitted.

    Message_Box
      ("Test 3",
       "Test 3: putting a HTML content to the Windows clipboard." & NL &
       "Paste it into some Office application.");

end Test_Clipboard;
