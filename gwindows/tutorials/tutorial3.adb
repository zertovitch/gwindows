with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.GStrings;
with GWindows.Application;

procedure Tutorial3 is
   pragma Linker_Options ("-mwindows");

   function "+"(S : String) return GWindows.GString
     renames GWindows.GStrings.To_GString_From_String;

   Main_Window : Main_Window_Type;
begin
   Create (Main_Window, "The Main Window");
   Visible (Main_Window, True);

   declare
      A_Window : Window_Access;
   begin
      for N in 1 .. 3 loop
         A_Window := new Window_Type;

         Create (A_Window.all, +("Dynamic Window" & N'Img),
                 Width      => 75,
                 Height     => 75,
                 Is_Dynamic => True);
         Visible (A_Window.all);
      end loop;
   end;

   GWindows.Application.Message_Loop;
end Tutorial3;
