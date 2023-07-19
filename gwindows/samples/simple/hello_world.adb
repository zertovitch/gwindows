with GWindows.Application,
     GWindows.Message_Boxes,
     GWindows.Windows.Main;

procedure Hello_World is
begin
   GWindows.Message_Boxes.Message_Box
      ("Hello World Message", "Hello World!");

   declare
      use GWindows.Windows.Main;
      Top : Main_Window_Type;
   begin
      Create (Top, "Hello World Window");
      Size (Top, 500, 300);
      Visible (Top);
      GWindows.Application.Message_Loop;
   end;

end Hello_World;
