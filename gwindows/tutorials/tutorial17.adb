with GWindows.GStrings.IO; use GWindows.GStrings.IO;

with GWindows.Windows; use GWindows.Windows;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Application;
with GWindows.Constants; use GWindows.Constants;

procedure Tutorial17 is
   Dialog        : Window_Type;
   OK_Button     : Default_Button_Type;
   Cancel_Button : Cancel_Button_Type;
   More_Button   : Dialog_Button_Type;

   ID_MORE       : constant := 101;

   Result        : Integer := ID_MORE;

begin
   while Result = ID_MORE loop
     Create_As_Dialog (Dialog, "My Dialog Window",
                       Width => 200, Height => 100);

     Create_Label (Dialog, "Have you had enough yet?",
                   10, 10,
                   Client_Area_Width (Dialog) - 20,
                   25, Center);

     Create (OK_Button, Dialog, "O&k",
             10, Client_Area_Height (Dialog) - 40,
             50, 25, ID => IDOK);

     Create (Cancel_Button, Dialog, "&Cancel",
             70, Client_Area_Height (Dialog) - 40,
             50, 25, ID => IDCANCEL);

     Create (More_Button, Dialog, "&More",
             130, Client_Area_Height (Dialog) - 40,
             50, 25, ID => ID_MORE);

     Result := GWindows.Application.Show_Dialog (Dialog);
   end loop;

   if Result = IDOK then
      Put_Line ("Have a nice day!");
   else
      Put_Line ("Sorry for the trouble...");
   end if;
end Tutorial17;
