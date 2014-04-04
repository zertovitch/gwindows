with GWindows.System_Tray;              use GWindows.System_Tray;

with GWindows;                          use GWindows;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Types;

with Interfaces.C;

procedure Test_System_Tray is

   d : Notify_Icon_Data;
   i1, i2, ib : Icon_Type;

   --------------------------------------------------------
   --  The following is only for getting the popup menu  --
   --------------------------------------------------------

   Systray_Menu : Menu_Type := Create_Popup;
   --  Menu ID's
   ID_Exit     : constant := 100;
   ID_Show     : constant := 101;
   ID_Hide     : constant := 102;

   type My_Window_Type is new Window_Type with null record;

   overriding
   procedure On_Menu_Select (Window : in out My_Window_Type;
                             Item   : in     Integer);

   overriding
   procedure On_Message
     (Window       : in out My_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
   is
      WM_RBUTTONUP : constant := 517;
   begin
      if Integer (message) = WM_TRAY_MESSAGE then
         if Integer (lParam) = WM_RBUTTONUP then
            --  Ada.Text_IO.Put ('*');
            Immediate_Popup_Menu (Systray_Menu, Window);
         end if;
      end if;
      --  Call parent method
      On_Message (
         Window_Type (Window),
         message,
         wParam,
         lParam,
         Return_Value
      );
   end On_Message;

   overriding
   procedure On_Menu_Select (Window : in out My_Window_Type;
                             Item   : in     Integer)
   is
   begin
      case Item is
         when ID_Show =>   Window.Show;
         when ID_Hide =>   Window.Hide;
         when ID_Exit =>   null;
         when others =>    null;
      end case;
   end On_Menu_Select;

   some_window : My_Window_Type;

   NL : constant GCharacter := GCharacter'Val (10); -- New Line

begin
   --  Create a dummy window. We don't even show it (yet).
   Create (some_window, "Just a window...");
   --  Preparing the system tray icon:
   Set_Window (d, some_window);
   Load_Stock_Icon (i1, IDI_QUESTION); -- Icon for the system tray
   Set_Icon (d, i1, 1);
   Load_Stock_Icon (ib, IDI_HAND); -- Icon for the balloons
   Set_Balloon_Icon (d, ib); -- Set user icon for the balloons
   Set_Tool_Tip (d, "Test_System_Tray" & NL & "The coolest app ever ;-)");
   Set_Balloon (d,
      "Balloon's text" & NL & "Funny, isn't it ?",
      "Balloon's title",
      Error_Icon   --   User_Icon
   );
   --  Now the fun part:
   Notify_Icon (d, Add);
   Message_Box (some_window, "Test_System_Tray", "Look at the system tray !");
   Load_Stock_Icon (i2, IDI_EXCLAMATION);
   Set_Icon (d, i2, 1);
   Set_Balloon (d, "Can't have enough!", "Another balloon");
   Notify_Icon (d, Modify);
   Message_Box (some_window, "Test_System_Tray", "Look again... !");
   --
   --  Menu fun
   --
   Append_Item (Systray_Menu, "&Show window", ID_Show);
   Append_Item (Systray_Menu, "&Hide window", ID_Hide);
   Append_Separator (Systray_Menu);
   Append_Item (Systray_Menu, "E&xit", ID_Exit);
   State (Systray_Menu, Command, ID_Exit, Grayed);
   --
   --  Activate messaging, such as mouse clicks in
   --  the vicinity of the systray icon
   --
   Set_Windows_Messaging (d);
   Notify_Icon (d, Modify);
   --
   --  Properly delete the systray icon
   --
   Message_Box (some_window,
      "Test_System_Tray",
      "Now try the menu with the right mouse button"
   );
   Notify_Icon (d, Delete);
end Test_System_Tray;
