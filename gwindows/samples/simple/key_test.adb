with GWindows.Message_Boxes;
with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Types;
with GWindows.Base;

with GWindows.GStrings.IO; use GWindows.GStrings.IO;
with GWindows.GStrings; use GWindows.GStrings;

procedure Key_Test is
   use GWindows.Windows.Main;

   Top : GWindows.Windows.Main.Main_Window_Type;

   procedure Do_Key_Down
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter)
   is
   begin
      Put_Line ("Special Key: " & To_GString_From_String (Special_Key'Img));
      Put_Line ("Value      : " & Value);
      Put_Line ("Ascii Code : " & Image (GWindows.GCharacter'Pos (Value)));
   end Do_Key_Down;

begin
   Create (Top, "Hello World");
   Size (Top, 300, 100);
   On_Character_Down_Handler (Top, Do_Key_Down'Unrestricted_Access);
   Visible (Top);
   Focus (Top);
   GWindows.Application.Message_Loop;
end Key_Test;

