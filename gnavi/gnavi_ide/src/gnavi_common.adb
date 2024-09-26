with GWindows.Colors;

package body GNAVI_Common is

   -----------------------------------------------------------------------
   --  The following is borrowed from LEA. In a later version we might  --
   --  introduce a dependency and use the LEA_Common package "as is".   --
   -----------------------------------------------------------------------

   type RGB_Type is range 0 .. 2**24 - 1;

   Very_Light_Gray : constant RGB_Type := 16#F8F8F8#;
   Light_Gray      : constant RGB_Type := 16#C0C0C0#;
   Very_Dark_Gray  : constant RGB_Type := 16#101010#;
   Dark_Green      : constant RGB_Type := 16#008000#;
   Dark_Red        : constant RGB_Type := 16#800000#;

   function Color_Convert
     (rgb : RGB_Type) return GWindows.Colors.Color_Type is
     use GWindows.Colors;
   begin
     return
       To_Color
         (Red_Value   => Color_Range (rgb / 2**16),
          Green_Value => Color_Range ((rgb / 2**8) mod (2**8)),
          Blue_Value  => Color_Range (rgb mod (2**8)));
   end Color_Convert;

   procedure Set_Up_Editor
     (Editor : in out GWindows.Scintilla.Scintilla_Type;
      Lexer  : in     Natural)
   is
      use GWindows, GWindows.Colors, GWindows.Scintilla;
      default_font      : constant GString := "Consolas";
      default_font_size : constant := 10;
      tab_width         : constant := 3;

      margin_leftmost         : constant := 0;
      margin_for_line_numbers : constant := 1;
      margin_for_bookmarks    : constant := 2;

      Key_Words : constant GWindows.GString :=
        "abort abs abstract accept access aliased all and array at begin body case " &
        "constant declare delay delta digits do else elsif end entry exception " &
        "exit for function generic goto if in interface is limited loop mod new not null of " &
        "or others out overriding package pragma private procedure protected raise range " &
        "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
        "task terminate then type until use when while with xor";
   begin
      --  Set up editor
      Editor.Set_EOL_Mode (SC_EOL_CRLF);
      Editor.Set_Tab_Width (tab_width);
      Editor.Set_Use_Tabs (False);
      Editor.Set_Edge_Column (80);
      Editor.Set_Edge_Mode (EDGE_LINE);
      --  Multi-line edit
      Editor.Set_Multiple_Selection;
      Editor.Set_Mouse_Selection_Rectangular;
      Editor.Set_Additional_Selection_Typing;
      Editor.Set_Virtual_Space_Options (SCVS_RECTANGULARSELECTION);

      Editor.Set_Margin_Width_N (margin_for_line_numbers, 50);
      Editor.Set_Margin_Type_N (margin_for_line_numbers, SC_MARGIN_NUMBER);

      Editor.Set_Lexer (Lexer);
      if Lexer = SCLEX_ADA then
         Editor.Set_Key_Words (0, Key_Words);
      end if;

      Editor.Style_Set_Fore (STYLE_DEFAULT, Black);
      Editor.Style_Set_Back (STYLE_DEFAULT, White);
      Editor.Style_Set_Size (STYLE_DEFAULT, default_font_size);
      Editor.Style_Set_Font (STYLE_DEFAULT, default_font);
      Style_Clear_All (Editor);

      Editor.Style_Set_Fore (STYLE_LINENUMBER, Color_Convert (Light_Gray));
      Editor.Style_Set_Back (STYLE_LINENUMBER, Color_Convert (Very_Light_Gray));

      case Lexer is
      
         when SCLEX_ADA =>

            -----------
            --  Ada  --
            -----------
            
            Editor.Style_Set_Fore (SCE_ADA_DEFAULT, Color_Convert (Very_Dark_Gray));
            Editor.Style_Set_Back (SCE_ADA_DEFAULT, White);
            Editor.Style_Set_Size (SCE_ADA_DEFAULT, default_font_size);
            Editor.Style_Set_Font (SCE_ADA_DEFAULT, default_font);
           
            Editor.Style_Set_Fore (SCE_ADA_COMMENTLINE, Color_Convert (Dark_Green));
            Editor.Style_Set_Fore (SCE_ADA_NUMBER, Dark_Orange);
            Editor.Style_Set_Fore (SCE_ADA_WORD, Blue);
            Editor.Style_Set_Fore (SCE_ADA_STRING, Dark_Gray);
            Editor.Style_Set_Fore (SCE_ADA_CHARACTER, Dark_Gray);
            --  Editor.Style_Set_Fore ( SCE_ADA_OPERATOR, Black);
            Editor.Style_Set_Fore (SCE_ADA_IDENTIFIER, Color_Convert (Very_Dark_Gray));
           
            Editor.Style_Set_Fore (SCE_ADA_STRINGEOL, White);
            Editor.Style_Set_Back (SCE_ADA_STRINGEOL, Red);

            --  Style: parentheses coloring
            --    For matched parentheses:
            Editor.Style_Set_Fore (STYLE_BRACELIGHT, Color_Convert (Dark_Green));
            Editor.Style_Set_Back (STYLE_BRACELIGHT, Color_Convert (16#cbe7f5#));
            --    For unmatched parentheses:
            Editor.Style_Set_Fore (STYLE_BRACEBAD, Color_Convert (Dark_Red));
            Editor.Style_Set_Back (STYLE_BRACEBAD, Color_Convert (16#cbe7f5#));

         when SCLEX_XML =>

            -----------
            --  XML  --
            -----------
            
            Editor.Style_Set_Fore (SCE_H_DEFAULT, Black);
            Editor.Style_Set_Back (SCE_H_DEFAULT, White);
            Editor.Style_Set_Size (SCE_H_DEFAULT, default_font_size);
            Editor.Style_Set_Font (SCE_H_DEFAULT, default_font);
            
            Editor.Style_Set_Fore (SCE_H_COMMENT, Red);
            Editor.Style_Set_Fore (SCE_H_NUMBER, Blue);
            Editor.Style_Set_Fore (SCE_H_TAG, Color_Convert (Dark_Green));
            Editor.Style_Set_Fore (SCE_H_ATTRIBUTE, Color_Convert (Dark_Red));
            Editor.Style_Set_Fore (SCE_H_ENTITY, Blue);
            Editor.Style_Set_Fore (SCE_H_SINGLESTRING, Dark_Blue);
            Editor.Style_Set_Fore (SCE_H_DOUBLESTRING, Dark_Blue);
            
         when others =>
            null;
            
      end case;
       
   end Set_Up_Editor;
   
end GNAVI_Common;
