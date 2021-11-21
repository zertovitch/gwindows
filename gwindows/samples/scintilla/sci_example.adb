--  Scintilla editor sample with Ada syntax.
--
--  Sci_Example.exe will work only if SciLexer.dll is in the same path.
--  SciLexer.dll can be found @ http://www.scintilla.org/, or in the redist directory,
--  or with the Notepad++ installation ( https://notepad-plus-plus.org/ ).
--
--  For a complete open-source application using GWindows.Scintilla, see the
--  LEA (Lightweight Editor for Ada) project @
--      https://sf.net/projects/l-e-a/
--      https://github.com/zertovitch/lea

with GWindows.Application;
with GWindows.Base;
with GWindows.Colors;
with GWindows.GStrings;
with GWindows.Message_Boxes;
with GWindows.Scintilla;
with GWindows.Types;
with GWindows.Windows.Main;

procedure Sci_Example is
   pragma Linker_Options ("-mwindows");

   use GWindows.Windows.Main, GWindows.Message_Boxes, GWindows.Scintilla;

   Main_Window : Main_Window_Type;
   Sci_Control : Scintilla_Type;

   TAB_WIDTH : constant := 3;

   Key_Words : constant GWindows.GString :=
     "abort abs abstract accept access aliased all and array at begin body case " &
     "constant declare delay delta digits do else elsif end entry exception " &
     "exit for function generic goto if in interface is limited loop mod new not null of " &
     "or others out overriding package pragma private procedure protected raise range " &
     "record rem renames requeue return reverse select separate some subtype synchronized " &
     "tagged task terminate then type until use when while with xor";

   procedure Do_Character_Added
      (Window      : in out GWindows.Base.Base_Window_Type'Class;
       Special_Key : in     GWindows.Windows.Special_Key_Type;
       Value       : in     GWindows.GCharacter);
   --  Handle auto tab

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Set up color highlighting

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows, GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
      SW : Scintilla_Type renames Scintilla_Type (Window);
   begin
      SW.On_Character_Added_Handler (Do_Character_Added'Unrestricted_Access);

      --  Set up editor
      SW.Set_EOL_Mode (SC_EOL_CR);
      SW.Set_Tab_Width (TAB_WIDTH);
      SW.Set_Use_Tabs (False);
      SW.Set_Edge_Column (100);
      SW.Set_Edge_Mode (EDGE_LINE);
      SW.Set_Indentation_Guides (True);
      --
      --  Enable sexy features like in Notepad++  :
      --    multi-line edit, rectangular selections, ...
      SW.Set_Multiple_Selection;
      SW.Set_Mouse_Selection_Rectangular;
      SW.Set_Additional_Selection_Typing;
      SW.Set_Virtual_Space_Options (SCVS_RECTANGULARSELECTION);

      --  Define the lexer.
      SW.Set_Lexer (SCLEX_ADA);
      SW.Set_Key_Words (0, Key_Words);

      SW.Style_Set_Fore (STYLE_DEFAULT, Black);
      SW.Style_Set_Back (STYLE_DEFAULT, White);
      SW.Style_Set_Size (STYLE_DEFAULT, App_default_font_size);
      SW.Style_Set_Font (STYLE_DEFAULT, App_default_font);
      SW.Style_Clear_All;

      SW.Style_Set_Fore (SCE_ADA_DEFAULT, Black);
      SW.Style_Set_Back (SCE_ADA_DEFAULT, White);
      SW.Style_Set_Size (SCE_ADA_DEFAULT, App_default_font_size);
      SW.Style_Set_Font (SCE_ADA_DEFAULT, App_default_font);

      SW.Style_Set_Fore (SCE_ADA_COMMENTLINE, Dark_Green);
      SW.Style_Set_Fore (SCE_ADA_NUMBER,      Blue);
      SW.Style_Set_Fore (SCE_ADA_WORD,        Dark_Blue);
      SW.Style_Set_Fore (SCE_ADA_STRING,      Dark_Red);
      SW.Style_Set_Fore (SCE_ADA_CHARACTER,   Blue);
      SW.Style_Set_Fore (SCE_ADA_IDENTIFIER,  Black);

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      SW.Style_Set_Fore (SCE_ADA_CHARACTEREOL, White);
      SW.Style_Set_Back (SCE_ADA_CHARACTEREOL, Dark_Red);
      SW.Style_Set_Fore (SCE_ADA_STRINGEOL, White);
      SW.Style_Set_Back (SCE_ADA_STRINGEOL, Dark_Red);
      SW.Style_Set_Fore (SCE_ADA_ILLEGAL, White);
      SW.Style_Set_Back (SCE_ADA_ILLEGAL, Dark_Red);

      SW.Set_Margin_Type_N (1, SC_MARGIN_NUMBER);
      SW.Set_Margin_Width_N (1, 40);
      SW.Set_Margin_Width_N (2, 10);

      SW.Focus;
   end Do_Create;

   CR : constant GWindows.GCharacter := GWindows.GCharacter'Val (13);
   LF : constant GWindows.GCharacter := GWindows.GCharacter'Val (10);

   NL : constant GWindows.GCharacter := LF;  --  We use Unix end-of-lines

   procedure Do_Character_Added
     (Window      : in out GWindows.Base.Base_Window_Type'Class;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
   pragma Unreferenced (Special_Key);
      CurPos : constant Position := Get_Current_Pos (Scintilla_Type (Window));
   begin
      if Value = LF or Value = CR then
         declare
            Line     : constant Integer := Line_From_Position
              (Scintilla_Type (Window), CurPos);
            Prev_Loc : constant Integer := Get_Line_Indentation
              (Scintilla_Type (Window), Line - 1);
         begin
            if Line > 0 and Prev_Loc > 0 then
               Set_Line_Indentation (Scintilla_Type (Window),
                                   Line,
                                   Prev_Loc - TAB_WIDTH);
            end if;
         end;
      end if;
   end Do_Character_Added;

begin
   Create (Main_Window, "Scintilla Example");

   if not SCI_Lexer_DLL_Successfully_Loaded then
     Message_Box (
       "DLL error - could not load scilexer.dll",
       "Either the file scilexer.dll doesn't exist, or there is a 32 vs. 64 bit mismatch." & NL &
       "You can copy scilexer.dll from the .\gwindows\redist directory." & NL &
       "This program is a" &
       GWindows.GStrings.To_GString_From_String (Integer'Image (GWindows.Types.Wparam'Size)) &
       " bit application.",
       Icon => Error_Icon
     );
   end if;

   On_Create_Handler (Sci_Control, Do_Create'Unrestricted_Access);
   Create (Sci_Control, Main_Window, 1, 1, 1, 1);

   Add_Text (Sci_Control,
     "with Ada.Text_IO; use Ada.Text_IO;" & NL &
     "procedure Hello is"                 & NL &
     "begin"                              & NL &
     "    "                               & NL &
     "   Put_Line (""World!"");"          & NL &
     "    "                               & NL &
     "   for i in 1 .. 10 loop"           & NL &
     "      "                             & NL &
     "      --  Now, it's your turn!"     & NL &
     "      "                             & NL &
     "      "                             & NL &
     "   --  For a complete application using GWindows.Scintilla, see" & NL &
     "   --  LEA (Lightweight Editor for Ada) project @"               & NL &
     "   --      https://sf.net/projects/l-e-a/"                       & NL &
     "   --      https://github.com/zertovitch/lea"                    & NL
   );
   --  NB: at some places we have put extra spaces to show off indentation guides...

   Dock (Sci_Control, GWindows.Base.Fill);

   Visible (Main_Window);

   GWindows.Application.Message_Loop;
end Sci_Example;
