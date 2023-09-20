--  Scintilla editor sample, preset with the Ada syntax.
--------------------------------------------------------
--
--  Sci_Example.exe will work only if SciLexer.dll is in the same path.
--  SciLexer.dll can be found @ http://www.scintilla.org/, or in the
--  redist directory, or with the Notepad++ installation
--  ( https://notepad-plus-plus.org/ ).
--
--  For a complete open-source application using GWindows.Scintilla, see
--  the LEA (Lightweight Editor for Ada) project @
--      https://sf.net/projects/l-e-a/
--      https://github.com/zertovitch/lea

with GWindows.Application,
     GWindows.Base,
     GWindows.Colors,
     GWindows.GStrings,
     GWindows.Message_Boxes,
     GWindows.Scintilla,
     GWindows.Types,
     GWindows.Windows.Main;

with Ada.Strings.Fixed;

procedure Sci_Example is
   pragma Linker_Options ("-mwindows");

   use Ada.Strings, Ada.Strings.Fixed,
       GWindows.Message_Boxes, GWindows.Scintilla,
       GWindows.GStrings, GWindows.Windows.Main;

   Main_Window : Main_Window_Type;
   Sci_Control : Scintilla_Type;

   TAB_WIDTH : constant := 3;

   Key_Words : constant GWindows.GString :=
     "abort abs abstract accept access aliased all and array at " &
     "begin body case constant " &
     "declare delay delta digits do else elsif end entry exception " &
     "exit for function generic goto " &
     "if in interface is limited loop " &
     "mod new not null of or others out overriding " &
     "package pragma private procedure protected " &
     "raise range record rem renames requeue return reverse " &
     "select separate some subtype synchronized " &
     "tagged task terminate then type until use when while with xor";

   procedure Do_Character_Added
      (Window      : in out GWindows.Base.Base_Window_Type'Class;
       Special_Key : in     GWindows.Windows.Special_Key_Type;
       Value       : in     GWindows.GCharacter);
   --  Handle auto tab

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Set up the editor widget

   procedure Do_Dwell_Start
     (Control : in out GWindows.Base.Base_Window_Type'Class;
      Pos     : in     Position);

   procedure Do_Dwell_End
     (Control : in out GWindows.Base.Base_Window_Type'Class;
      Pos     : in     Position);

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows, GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
      Editor : Scintilla_Type renames Scintilla_Type (Window);
   begin
      --  Setting up some handlers for interaction.
      --  On a larger application, it is better to derive Scintilla_Type
      --  and override the methods On_Character_Added, On_Dwell_Start, etc.
      --  instead of using handlers and type conversions.
      Editor.On_Character_Added_Handler (Do_Character_Added'Unrestricted_Access);
      Editor.On_Dwell_Start_Handler (Do_Dwell_Start'Unrestricted_Access);
      Editor.On_Dwell_End_Handler (Do_Dwell_End'Unrestricted_Access);

      --  Set up editor
      Editor.Set_EOL_Mode (SC_EOL_LF);
      Editor.Set_Tab_Width (TAB_WIDTH);
      Editor.Set_Use_Tabs (False);
      Editor.Set_Edge_Column (100);
      Editor.Set_Edge_Mode (EDGE_LINE);
      Editor.Set_Indentation_Guides (True);
      Editor.Set_Mouse_Dwell_Time (500);
      --
      --  Enable sexy features like in Notepad++  :
      --    multi-line edit, rectangular selections, ...
      Editor.Set_Multiple_Selection;
      Editor.Set_Mouse_Selection_Rectangular;
      Editor.Set_Additional_Selection_Typing;
      Editor.Set_Virtual_Space_Options (SCVS_RECTANGULARSELECTION);

      --  Define the lexer.
      Editor.Set_Lexer (SCLEX_ADA);
      Editor.Set_Key_Words (0, Key_Words);

      Editor.Style_Set_Fore (STYLE_DEFAULT, Black);
      Editor.Style_Set_Back (STYLE_DEFAULT, White);
      Editor.Style_Set_Size (STYLE_DEFAULT, App_default_font_size);
      Editor.Style_Set_Font (STYLE_DEFAULT, App_default_font);
      Editor.Style_Clear_All;

      Editor.Style_Set_Fore (SCE_ADA_DEFAULT, Black);
      Editor.Style_Set_Back (SCE_ADA_DEFAULT, White);
      Editor.Style_Set_Size (SCE_ADA_DEFAULT, App_default_font_size);
      Editor.Style_Set_Font (SCE_ADA_DEFAULT, App_default_font);

      Editor.Style_Set_Fore (SCE_ADA_COMMENTLINE, Dark_Green);
      Editor.Style_Set_Fore (SCE_ADA_NUMBER,      Blue);
      Editor.Style_Set_Fore (SCE_ADA_WORD,        Dark_Blue);
      Editor.Style_Set_Fore (SCE_ADA_STRING,      Dark_Red);
      Editor.Style_Set_Fore (SCE_ADA_CHARACTER,   Blue);
      Editor.Style_Set_Fore (SCE_ADA_IDENTIFIER,  Black);

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      Editor.Style_Set_Fore (SCE_ADA_CHARACTEREOL, White);
      Editor.Style_Set_Back (SCE_ADA_CHARACTEREOL, Dark_Red);
      Editor.Style_Set_Fore (SCE_ADA_STRINGEOL, White);
      Editor.Style_Set_Back (SCE_ADA_STRINGEOL, Dark_Red);
      Editor.Style_Set_Fore (SCE_ADA_ILLEGAL, White);
      Editor.Style_Set_Back (SCE_ADA_ILLEGAL, Dark_Red);

      Editor.Set_Margin_Type_N (1, SC_MARGIN_NUMBER);
      Editor.Set_Margin_Width_N (1, 40);
      Editor.Set_Margin_Width_N (2, 10);

      Editor.Focus;
   end Do_Create;

   --  CR : constant GWindows.GCharacter := GWindows.GCharacter'Val (13);
   LF : constant GWindows.GCharacter := GWindows.GCharacter'Val (10);

   NL : constant GWindows.GCharacter := LF;  --  We use Unix end-of-lines

   procedure Do_Character_Added
     (Window      : in out GWindows.Base.Base_Window_Type'Class;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
      pragma Unreferenced (Special_Key);
      Editor : Scintilla_Type renames Scintilla_Type (Window);
      Cur_Pos : constant Position := Get_Current_Pos (Editor);
      Line, Indent_Previous_Line : Integer;
      Back_Pos : Position;
      Max_Backwards_Lines : constant := 5;
   begin
      case Value is
         when LF =>
            --  Auto-indent on new line.
            Line := Editor.Line_From_Position (Cur_Pos);
            if Line > 0 then
               Indent_Previous_Line := Editor.Get_Line_Indentation (Line - 1);
               if Indent_Previous_Line > 0 then
                  Editor.Add_Text
                    (To_GString_From_String (Indent_Previous_Line * ' '));
               end if;
            end if;
         when '(' =>
            --  Search backwards for an identifier
            Line :=
               Integer'Max
                 (0, Editor.Line_From_Position (Cur_Pos) - Max_Backwards_Lines);
            Back_Pos := Editor.Position_From_Line (Line);
            for Pos in reverse Back_Pos .. Cur_Pos - 1 loop
                if Editor.Get_Style_At (Pos) = SCE_ADA_IDENTIFIER then
                   --  Call tip for hypothetical subprogram parameters...
                   Editor.Call_Tip_Show
                      (Cur_Pos, Editor.Get_Word_At (Pos, True) &
                       " (A : Integer, B : Float)");
                   exit;
                end if;
            end loop;
         when ')' =>
            Editor.Call_Tip_Cancel;
         when 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' =>
            Editor.Auto_C_Set_Ignore_Case (True);
            Editor.Auto_C_Show
              (Integer (Cur_Pos - Editor.Word_Start_Position (Cur_Pos, False)),
               --  ^ Characters to word's begin are used for context.
               --    We should include digits in order to be complete...
               "algo Alpha A4 A42 beta Bravo cargo Charlie Id_42_7 id_eal");
         when others =>
            null;
      end case;
   end Do_Character_Added;

   procedure Do_Dwell_Start
     (Control : in out GWindows.Base.Base_Window_Type'Class;
      Pos     : in     Position)
   is
      Editor : Scintilla_Type renames Scintilla_Type (Control);
   begin
      if Editor.Get_Style_At (Pos) = SCE_ADA_IDENTIFIER then
         --  Mouse hover tool tip
         Editor.Call_Tip_Show
           (Pos, Editor.Get_Word_At (Pos, True) & NL & "___" & NL & "Defined somewhere");
      end if;
   end Do_Dwell_Start;

   procedure Do_Dwell_End
     (Control : in out GWindows.Base.Base_Window_Type'Class;
      Pos     : in     Position)
   is
   pragma Unreferenced (Pos);
   begin
      Scintilla_Type (Control).Call_Tip_Cancel;
   end Do_Dwell_End;

begin
   Main_Window.Create ("Scintilla Example");

   if not SCI_Lexer_DLL_Successfully_Loaded then
     Message_Box
       ("DLL error - could not load: ""scilexer.dll""",
        "Either the file ""scilexer.dll"" doesn't exist, or there is" &
        " a 32 vs. 64 bit compatibility mismatch." & NL &
        "You can copy ""scilexer.dll"" from the" &
        " .\gwindows\redist directory to .\gwindows\samples ." &
        NL & NL &
        "This program is built as a" &
        GWindows.GStrings.To_GString_From_String
          (Integer'Image (GWindows.Types.Wparam'Size)) &
        " bit application.",
        Icon => Error_Icon);
   end if;

   Sci_Control.On_Create_Handler (Do_Create'Unrestricted_Access);
   Sci_Control.Create (Main_Window, 1, 1, 1, 1);

   Sci_Control.Add_Text (
     "with Ada.Text_IO;"                  & NL &
     NL &
     "procedure Hello is"                 & NL &
     "   use Ada.Text_IO;"                & NL &
     "begin"                              & NL &
     "    "                               & NL &
     "   Put_Line (""World!"");"          & NL &
     "    "                               & NL &
     "   for i in 1 .. 10 loop"           & NL &
     "      "                             & NL &
     "      --  Now, it's your turn!"     & NL &
     "       "                            & NL &
     "        "                           & NL &
     "   --  For a complete application using GWindows.Scintilla, see" & NL &
     "   --  the LEA (Lightweight Editor for Ada) project @"           & NL &
     "   --      https://sf.net/projects/l-e-a/"                       & NL &
     "   --      https://github.com/zertovitch/lea"                    & NL
   );
   --  NB: at some places we have put extra spaces to
   --      show off indentation guides...

   Sci_Control.Dock (GWindows.Base.Fill);

   Main_Window.Visible;

   GWindows.Application.Message_Loop;
end Sci_Example;
