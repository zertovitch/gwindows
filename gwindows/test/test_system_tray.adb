--  This test demonstrates:
--  - simple systray icon and message features
--      (a few lines in Show_systray_icon,
--       Hide_systray_icon, Swap_systray_icon below)
--  - how to set up a systray menu
--  - how to show / hide a window from a systray menu
--  - how to show / hide a systray icon from a window menu

with GWindows.System_Tray;              use GWindows.System_Tray;

with GWindows;                          use GWindows;
with GWindows.Application;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Windows.Main;             use GWindows.Windows.Main;
with GWindows.Types;

with Interfaces.C;

procedure Test_System_Tray is

   pragma Linker_Options ("-mwindows");

   --  Menu ID's
   ID_Exit        : constant := 100;
   ID_Show_Icon   : constant := 101;
   ID_Hide_Icon   : constant := 102;
   ID_Swap_Icon   : constant := 103;
   ID_Show_Window : constant := 104;
   ID_Hide_Window : constant := 105;

   type My_Window_Type is new Main_Window_Type with record
      d                    : Notify_Icon_Data;
      i1, i2, ib           : Icon_Type;
      Systray_Icon_Visible : Boolean := False;
      Icon_shown           : Positive range 1 .. 2 := 1;
      Systray_Menu         : Menu_Type := Create_Popup;
      Top_Menu             : Menu_Type := Create_Menu;
      File_Menu            : Menu_Type := Create_Popup;
   end record;

   overriding
   procedure On_Create (Window : in out My_Window_Type);

   overriding
   procedure On_Close (Window : in out My_Window_Type;
                       Can_Close :    out Boolean);

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
            Immediate_Popup_Menu (Window.Systray_Menu, Window);
         end if;
      end if;
      --  Call parent method
      On_Message (
         Main_Window_Type (Window),
         message,
         wParam,
         lParam,
         Return_Value
      );
   end On_Message;

   procedure Show_systray_icon (Window : in out My_Window_Type);
   procedure Hide_systray_icon (Window : in out My_Window_Type);

   overriding
   procedure On_Create (Window : in out My_Window_Type) is
   begin
      Append_Item (Window.Systray_Menu, "&Show window", ID_Show_Window);
      Append_Item (Window.Systray_Menu, "&Hide window", ID_Hide_Window);
      Append_Separator (Window.Systray_Menu);
      Append_Item (Window.Systray_Menu, "E&xit", ID_Exit);
      --
      Append_Menu (Window.Top_Menu, "&File", Window.File_Menu);
      --
      Append_Item (Window.File_Menu, "&Show icon", ID_Show_Icon);
      Append_Item (Window.File_Menu, "&Hide icon", ID_Hide_Icon);
      Append_Item (Window.File_Menu, "&Swap icon", ID_Swap_Icon);
      Append_Separator (Window.File_Menu);
      Append_Item (Window.File_Menu, "E&xit", ID_Exit);
      Menu (Window, Window.Top_Menu);
      --
      Load_Stock_Icon (Window.i1, IDI_QUESTION);
      Load_Stock_Icon (Window.i2, IDI_EXCLAMATION);
      Show_systray_icon (Window);
      --
      --  Activate messaging, such as mouse clicks in
      --  the vicinity of the systray icon
      --
      Set_Windows_Messaging (Window.d);
      Notify_Icon (Window.d, Modify);
      Window.Show;
   end On_Create;

   overriding
   procedure On_Close (Window : in out My_Window_Type;
                       Can_Close :    out Boolean) is
   begin
      Hide_systray_icon (Window);
      Can_Close := True;
   end On_Close;

   NL : constant GCharacter := GCharacter'Val (10); -- New Line

   procedure Show_systray_icon (Window : in out My_Window_Type) is
   begin
      if Window.Systray_Icon_Visible then
         return;
      end if;
      --  Preparing the system tray icon:
      Set_Window (Window.d, Window);
      Set_Icon (Window.d, Window.i1, 1);
      Load_Stock_Icon (Window.ib, IDI_HAND); -- Icon for the balloons
      Set_Balloon_Icon (Window.d, Window.ib);
      Set_Tool_Tip (Window.d,
         "Test_System_Tray" & NL & "The coolest app ever ;-)"
      );
      Set_Balloon (Window.d,
         "Balloon's text" & NL & "Funny, isn't it ?",
         "Balloon's title",
         Error_Icon   --   User_Icon
      );
      Notify_Icon (Window.d, Add);
      Window.Systray_Icon_Visible := True;
   end Show_systray_icon;

   procedure Swap_systray_icon (Window : in out My_Window_Type) is
   begin
      if Window.Systray_Icon_Visible then
         Window.Icon_shown := 3 - Window.Icon_shown;
         case Window.Icon_shown is
            when 1 => Set_Icon (Window.d, Window.i1, 1);
            when 2 => Set_Icon (Window.d, Window.i2, 1);
         end case;
         Set_Balloon (Window.d, "Can't have enough!", "Another balloon");
         Notify_Icon (Window.d, Modify);
      end if;
   end Swap_systray_icon;

   procedure Hide_systray_icon (Window : in out My_Window_Type) is
   begin
      if Window.Systray_Icon_Visible then
         --  Properly delete the systray icon
         Notify_Icon (Window.d, Delete);
         Window.Systray_Icon_Visible := False;
      end if;
   end Hide_systray_icon;

   overriding
   procedure On_Menu_Select (Window : in out My_Window_Type;
                             Item   : in     Integer)
   is
   begin
      case Item is
         when ID_Show_Window => Window.Show;
         when ID_Hide_Window => Window.Hide;
         when ID_Show_Icon   => Show_systray_icon (Window);
         when ID_Hide_Icon   => Hide_systray_icon (Window);
         when ID_Swap_Icon   => Swap_systray_icon (Window);
         when ID_Exit        => Window.Close;
         when others =>    null;
      end case;
   end On_Menu_Select;

   some_window : My_Window_Type;

begin
   Create (some_window, "A very simple window");
   GWindows.Application.Message_Loop;
end Test_System_Tray;
