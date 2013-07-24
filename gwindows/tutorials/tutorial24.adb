with GWindows.Application;

with Tutorial24_Window; use Tutorial24_Window;

procedure Tutorial24 is
   pragma Linker_Options ("-mwindows");
   My_Window   : Tutorial24_Window.My_Window_Type;
begin
   Create (My_Window, "Drag test - tutorial 24");
   Insert_Item (My_Window.Some_list, "Item Nr 1", 0);
   Visible (My_Window, True);
   GWindows.Application.Message_Loop;
end Tutorial24;
