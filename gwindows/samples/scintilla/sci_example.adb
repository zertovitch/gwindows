--  Scintilla editor sample with Ada syntax.
--
--  Sci_Example.exe will work only if Scintilla.dll
--  and SciLexer.dll are in the same path.


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
     "abort abstract accept access aliased all array at begin body case " &
     "constant declare delay delta digits do else elsif end entry exception " &
     "exit for function generic goto if in is limited loop new null of " &
     "others out package pragma private procedure protected raise range " &
     "record renames requeue return reverse select separate subtype tagged " &
     "task terminate then type until use when while with";

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
   begin
      On_Character_Added_Handler (Sci_Control,
                                  Do_Character_Added'Unrestricted_Access);

      --  Set up editor
      SetEOLMode (Scintilla_Type (Window), SC_EOL_CRLF);
      SetTabWidth (Scintilla_Type (Window), TAB_WIDTH);
      SetUseTabs (Scintilla_Type (Window), False);
      SetEdgeColumn (Scintilla_Type (Window), 80);
      SetEdgeMode (Scintilla_Type (Window), EDGE_LINE);
      --  SetIndentationGuides (Scintilla_Type (Window), True);

      SetLexer (Scintilla_Type (Window), SCLEX_ADA);
      SetKeyWords (Scintilla_Type (Window), 0, Key_Words);

      StyleSetFore (Scintilla_Type (Window), STYLE_DEFAULT, Black);
      StyleSetBack (Scintilla_Type (Window), STYLE_DEFAULT, White);
      StyleSetSize (Scintilla_Type (Window), STYLE_DEFAULT, App_default_font_size);
      StyleSetFont (Scintilla_Type (Window), STYLE_DEFAULT, App_default_font);
      StyleClearAll (Scintilla_Type (Window));

      StyleSetFore (Scintilla_Type (Window), SCE_ADA_DEFAULT, Black);
      StyleSetBack (Scintilla_Type (Window), SCE_ADA_DEFAULT, White);
      StyleSetSize (Scintilla_Type (Window), SCE_ADA_DEFAULT, App_default_font_size);
      StyleSetFont (Scintilla_Type (Window), SCE_ADA_DEFAULT, App_default_font);

      StyleSetFore (Scintilla_Type (Window), SCE_ADA_COMMENTLINE, Red);
      StyleSetFore (Scintilla_Type (Window), SCE_ADA_NUMBER, Blue);
      StyleSetFore (Scintilla_Type (Window), SCE_ADA_WORD, Dark_Green);
      StyleSetFore (Scintilla_Type (Window), SCE_ADA_STRING, Dark_Red);
      StyleSetFore (Scintilla_Type (Window), SCE_ADA_CHARACTER, Blue);
      --  StyleSetFore (Scintilla_Type (Window), SCE_ADA_OPERATOR, Black);
      StyleSetFore (Scintilla_Type (Window), SCE_ADA_IDENTIFIER, Black);

      StyleSetFore (Scintilla_Type (Window), SCE_ADA_STRINGEOL, White);
      StyleSetBack (Scintilla_Type (Window), SCE_ADA_STRINGEOL, Red);

      Focus (Scintilla_Type (Window));
   end Do_Create;

   procedure Do_Character_Added
     (Window      : in out GWindows.Base.Base_Window_Type'Class;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
      CurPos : Position := GetCurrentPos (Scintilla_Type (Window));
   begin
      if
        Value = GWindows.GCharacter'Val (10)
        or
        Value = GWindows.GCharacter'Val (13)
      then
         declare
            Line     : Integer := LineFromPosition
              (Scintilla_Type (Window), CurPos);
            Prev_Loc : Integer := GetLineIndentation
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
