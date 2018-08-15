--  Demo for mouse capture, drag & drop, and Get_Window_At_Location

with GWindows.Types;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Base; use GWindows.Base;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Cursors; use GWindows.Cursors;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Application;

procedure Cap_Test is
   Window       : Main_Window_Type;
   Label        : Label_Type;
   Status       : Label_Type;
   Cross_Cursor : Cursor_Type;
   Arrow_Cursor : Cursor_Type;
   In_Capture   : Boolean := False;

   procedure Do_Mouse_Down (Window : in out Base_Window_Type'Class;
                            X, Y   : in     Integer;
                            Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (X, Y, Keys);
   begin
      Set_Cursor (Cross_Cursor);
      Capture_Mouse (Window);
      In_Capture := True;
   end Do_Mouse_Down;

   procedure Do_Mouse_Move (Window : in out Base_Window_Type'Class;
                            X, Y   : in     Integer;
                            Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (Keys);
   begin
      if In_Capture then
         declare
            Is_GWindow : Boolean := False;
            Location   : constant GWindows.Types.Point_Type :=
              Point_To_Desktop (Window, (X, Y));
            Win_Ptr    : constant Pointer_To_Base_Window_Class :=
              GWindows.Application.Get_Window_At_Location (Location.X,
                                                           Location.Y);
         begin
            if Win_Ptr /= null then
               Is_GWindow := True;
            end if;

            Text (Status, To_GString_From_String ("Location:" &
                                                  Location.X'Img &
                                                  Location.Y'Img &
                                                  " Is a GWindow " &
                                                  Is_GWindow'Img));
         end;
      else
         Text (Status, To_GString_From_String ("Location:" & X'Img & Y'Img));
      end if;
   end Do_Mouse_Move;

   procedure Do_Mouse_Up (Window : in out Base_Window_Type'Class;
                          X, Y   : in     Integer;
                          Keys   : in     Mouse_Key_States)
   is
   pragma Unreferenced (Window, X, Y, Keys);
   begin
      Release_Mouse;
      Set_Cursor (Arrow_Cursor);
      In_Capture := False;
   end Do_Mouse_Up;

begin
   Cross_Cursor := Load_System_Cursor (IDC_CROSS);
   Arrow_Cursor := Load_System_Cursor (IDC_ARROW);

   Create (Window, "Mouse Capture Test Window");
   On_Left_Mouse_Button_Down_Handler
     (Window, Do_Mouse_Down'Unrestricted_Access);
   On_Left_Mouse_Button_Up_Handler
     (Window, Do_Mouse_Up'Unrestricted_Access);
   On_Mouse_Move_Handler (Window, Do_Mouse_Move'Unrestricted_Access);

   Create (Label, Window,
           "Click on window then drag mouse to other windows",
           0,0,25,25,Center);
   Border (Label);
   Dock (Label, At_Top);

   Create (Status, Window,
           "Location: None",
           0,0,25,25,Center);
   Dock (Status, At_Top);
   Border (Status);
   Show (Window);

   GWindows.Application.Message_Loop;
end Cap_Test;
