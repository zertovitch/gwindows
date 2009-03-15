with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial13 is
   pragma Linker_Options ("-mwindows");
   
   subtype Window_Number_Type is Positive range 1 .. 5;
   
   type Window_Array_Type is array (Window_Number_Type'Range) of
     GWindows.Windows.Window_Type;
   
   Main_Window    : GWindows.Windows.Main.Main_Window_Type;
   Windows        : Window_Array_Type;
   Next_Button    : GWindows.Buttons.Button_Type;
   Current_Window : Window_Number_Type := Window_Number_Type'Last;
   
   procedure Do_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Order (Windows (Current_Window), GWindows.Base.Bottom);
      if Current_Window = Window_Number_Type'Last then
	 Current_Window := Window_Number_Type'First;
      else
	 Current_Window := Window_Number_Type'Succ (Current_Window);
      end if;
   end Do_Click;
   
begin
   Create (Main_Window, "Z-Order Window", Width => 100, Height => 100);
   Visible (Main_Window, True);
   Center (Main_Window);
   
   for N in Window_Array_Type'Range loop
      Create (Windows (N), To_GString_From_String ("TEST" & N'Img),
	      Width => 75, Height => 75);
      Visible (Windows (N));
   end loop;
   
   Create (Next_Button, Main_Window, "&Next", 10, 10,
	   Client_Area_Width (Main_Window) - 20,
	   Client_Area_Height (Main_Window) - 20);
   On_Click_Handler (Next_Button, Do_Click'Unrestricted_Access);
   
   GWindows.Application.Message_Loop;
end Tutorial13;
