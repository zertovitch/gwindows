with GWindows.Base,
     GWindows.Scintilla;
     
with Interfaces.C;

with GNAT.OS_Lib;

package GNAVI_Common is

   procedure Set_Up_Editor
     (Editor : in out GWindows.Scintilla.Scintilla_Type;
      Lexer  : in     Natural);

   subtype Time_Stamp is GNAT.OS_Lib.OS_Time;

end GNAVI_Common;
