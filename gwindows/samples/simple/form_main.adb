with Standard_IDs;

with GWindows.Constants;
with GWindows.Static_Controls;
with GWindows.Buttons;
with GWindows.Events;
with GWindows.Drawing_Objects;

package body Form_Main is

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Form_Main_Type) is
      use GWindows.Edit_Boxes;
      use GWindows.Static_Controls;
      use GWindows.Buttons;

      Window_Font : GWindows.Drawing_Objects.Font_Type;
      Label       : Label_Access;
      OK          : Default_Button_Access := new Default_Button_Type;
   begin
      Menu (Window, "Main_Menu");
      Accelerator_Table (Window, "Main_Menu");

      --  Use Standard Windows GUI font instead of system font
      GWindows.Drawing_Objects.Create_Stock_Font
        (Window_Font, GWindows.Drawing_Objects.Default_GUI);
      Set_Font (Window, Window_Font);

      Label := new Label_Type;
      Create (Label.all, Window, "Please enter your information:",
              10, 10, 200, 25, Is_Dynamic => True);

      Create (OK.all, Window, "O&k", 220, 5, 75, 30,
              ID => GWindows.Constants.IDOK,
              Is_Dynamic => True);
      On_Click_Handler (OK.all, GWindows.Events.Do_End_Application'Access);

      Label := new Label_Type;
      Create
        (Label.all, Window, "Last Name", 10, 50, 80, 25, Is_Dynamic => True);

      Create (Window.Last_Name,
              Window,
              "",
              100, 50, 200, 25);

      Label := new Label_Type;
      Create
        (Label.all, Window, "First Name", 10, 80, 80, 25, Is_Dynamic => True);

      Create (Window.First_Name,
              Window,
              "",
              100, 80, 200, 25);

      Label := new Label_Type;
      Create
        (Label.all, Window, "Address", 10, 110, 80, 25, Is_Dynamic => True);

      Create (Window.Address,
              Window,
              "",
              100, 110, 200, 25);

      Label := new Label_Type;
      Create (Label.all, Window, "City", 10, 140, 80, 25, Is_Dynamic => True);

      Create (Window.City,
              Window,
              "",
              100, 140, 200, 25);

      Label := new Label_Type;
      Create (Label.all, Window, "State", 10, 170, 80, 25, Is_Dynamic => True);

      Create (Window.State,
              Window,
              "",
              100, 170, 200, 25);

      Label := new Label_Type;
      Create (Label.all, Window, "Zip", 10, 200, 80, 25, Is_Dynamic => True);

      Create (Window.Zip,
              Window,
              "",
              100, 200, 200, 25);

      Label := new Label_Type;
      Create
        (Label.all, Window, "Country", 10, 230, 80, 25, Is_Dynamic => True);

      Create (Window.Country,
              Window,
              "",
              100, 230, 200, 25);

      Focus (Window.Last_Name);
      Size (Window, 320, 320);
      Center (Window);
      Keyboard_Support (Window, True);
      Show (Window);

      On_Destroy_Handler (Window, GWindows.Events.Do_End_Application'Access);
   end On_Create;

   --------------------
   -- On_Menu_Select --
   --------------------

   procedure On_Menu_Select
     (Window : in out Form_Main_Type;
      Item   : in     Integer)
   is
      use Standard_IDs;
   begin
      case Item is
         when ID_APP_EXIT =>
            Close (Window);
         when others =>
            null;
      end case;
   end On_Menu_Select;

end Form_Main;
