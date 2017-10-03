--  Scintilla editor sample with Ada syntax.
--
--  Sci_Example.exe will work only if SciLexer.dll is in the same path.
--  SciLexer.dll can be found @ http://www.scintilla.org/ or in the redist directory.

with GWindows.Windows.Main;             use GWindows.Windows.Main;
with GWindows.Scintilla;                use GWindows.Scintilla;
with GWindows.Base;
with GWindows.Application;
with GWindows.Colors;

procedure Sci_Example is
   pragma Linker_Options ("-mwindows");

   Main_Window : Main_Window_Type;
   Sci_Control : Scintilla_Type;

   TAB_WIDTH : constant := 3;

   Key_Words : constant GWindows.GString :=
     "abort abs abstract accept access aliased all and array at begin body case " &
     "constant declare delay delta digits do else elsif end entry exception " &
     "exit for function generic goto if in interface is limited loop mod new not null of " &
     "or others out overriding package pragma private procedure protected raise range " &
     "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
     "task terminate then type until use when while with xor";

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
      SW.SetEOLMode (SC_EOL_CRLF);
      SW.SetTabWidth (TAB_WIDTH);
      SW.SetUseTabs (False);
      SW.SetEdgeColumn (80);
      SW.SetEdgeMode (EDGE_LINE);
      --  SW.SetIndentationGuides (True);

      SW.SetLexer (SCLEX_ADA);
      SW.SetKeyWords (0, Key_Words);

      SW.StyleSetFore (STYLE_DEFAULT, Black);
      SW.StyleSetBack (STYLE_DEFAULT, White);
      SW.StyleSetSize (STYLE_DEFAULT, App_default_font_size);
      SW.StyleSetFont (STYLE_DEFAULT, App_default_font);
      SW.StyleClearAll;

      SW.StyleSetFore (SCE_ADA_DEFAULT, Black);
      SW.StyleSetBack (SCE_ADA_DEFAULT, White);
      SW.StyleSetSize (SCE_ADA_DEFAULT, App_default_font_size);
      SW.StyleSetFont (SCE_ADA_DEFAULT, App_default_font);

      SW.StyleSetFore (SCE_ADA_COMMENTLINE, Red);
      SW.StyleSetFore (SCE_ADA_NUMBER,      Blue);
      SW.StyleSetFore (SCE_ADA_WORD,        Dark_Green);
      SW.StyleSetFore (SCE_ADA_STRING,      Dark_Red);
      SW.StyleSetFore (SCE_ADA_CHARACTER,   Blue);
      SW.StyleSetFore (SCE_ADA_IDENTIFIER,  Black);

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      SW.StyleSetFore (SCE_ADA_CHARACTEREOL, White);
      SW.StyleSetBack (SCE_ADA_CHARACTEREOL, Dark_Red);
      SW.StyleSetFore (SCE_ADA_STRINGEOL, White);
      SW.StyleSetBack (SCE_ADA_STRINGEOL, Dark_Red);
      SW.StyleSetFore (SCE_ADA_ILLEGAL, White);
      SW.StyleSetBack (SCE_ADA_ILLEGAL, Dark_Red);

      SW.SetMarginTypeN (1, SC_MARGIN_NUMBER);
      SW.SetMarginWidthN (1, 40);
      SW.SetMarginWidthN (2, 10);

      SW.Focus;
   end Do_Create;

   procedure Do_Character_Added
     (Window      : in out GWindows.Base.Base_Window_Type'Class;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
   pragma Unreferenced (Special_Key);
      CurPos : constant Position := GetCurrentPos (Scintilla_Type (Window));
   begin
      if
        Value = GWindows.GCharacter'Val (10)
        or
        Value = GWindows.GCharacter'Val (13)
      then
         declare
            Line     : constant Integer := LineFromPosition
              (Scintilla_Type (Window), CurPos);
            Prev_Loc : constant Integer := GetLineIndentation
              (Scintilla_Type (Window), Line - 1);
         begin
            if Line > 0 and Prev_Loc > 0 then
               SetLineIndentation (Scintilla_Type (Window),
                                   Line,
                                   Prev_Loc - TAB_WIDTH);
            end if;
         end;
      end if;
   end Do_Character_Added;

begin
   Create (Main_Window, "Scintilla Example");

   On_Create_Handler (Sci_Control, Do_Create'Unrestricted_Access);
   Create (Sci_Control, Main_Window, 1, 1, 1, 1);

   AddText (Sci_Control, "with Ada.Text_IO; use Ada.Text_IO;");

   Dock (Sci_Control, GWindows.Base.Fill);

   Visible (Main_Window);

   GWindows.Application.Message_Loop;
end Sci_Example;
