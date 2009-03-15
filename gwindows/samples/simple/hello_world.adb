with GWindows.Message_Boxes;
with GWindows.Windows.Main;
with GWindows.Application;

procedure Hello_World is
begin
   GWindows.Message_Boxes.Message_Box ("Hello_World", "Hello World!");

   declare
      use GWindows.Windows.Main;

      Top : GWindows.Windows.Main.Main_Window_Type;
   begin
      Create (Top, "Hello World");
      Size (Top, 300, 100);
      Visible (Top);
      GWindows.Application.Message_Loop;
   end;

end Hello_World;
