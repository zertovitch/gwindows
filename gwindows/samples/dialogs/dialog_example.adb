with GWindows.Base; use GWindows.Base;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.Constants;
with GWindows.Application;

procedure Dialog_Example is
   pragma Linker_Options ("dialog_example.coff");

   IDD_DIALOG1 : constant := 101;
   IDD_DIALOG2 : constant := 102;
   IDI_ICON1   : constant := 103;
   IDC_EDIT1   : constant := 1000;
   IDC_DONE    : constant := 1001;
   IDC_TRYME   : constant := 1002;

   Window       : GWindows.Windows.Window_Type;
   Done_Button  : GWindows.Buttons.Button_Type;
   TryMe_Button : GWindows.Buttons.Button_Type;

   procedure Do_TryMe
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Done
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_TryMe
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      procedure Get_Data
        (Window : in out GWindows.Base.Base_Window_Type'Class);

      Name_Dlg  : GWindows.Windows.Window_Type;
      Name_Box  : GWindows.Edit_Boxes.Edit_Box_Type;
      Name_Str  : GWindows.GString_Unbounded;

      procedure Get_Data
        (Window : in out GWindows.Base.Base_Window_Type'Class)
      is
      begin
         If Modal_Result (Window) = GWindows.Constants.IDOK then
            Name_Str := GWindows.GStrings.To_GString_Unbounded
              (GWindows.Edit_Boxes.Text (Name_Box));
         end if;
      end Get_Data;

   begin
      Create_Dialog (Name_Dlg, Resource_ID (IDD_DIALOG1));
      --  Loads dialog resource in to GWindows object

      Center (Name_Dlg);

      Attach_Dialog_Item (Name_Box, Name_Dlg, IDC_EDIT1);
      --  Attach a dialog resource item to a GWindows objects
      Focus (Name_Box);

      On_Destroy_Handler (Name_Dlg, Get_Data'Unrestricted_Access);
      --  When the window has closed as a result of one of:
      --  1) The Enter Key 2) The ESC key 3) Clicking OK 4) Clicking Cancel
      --  We can collect our data from the dialog.

      if
          GWindows.Application.Show_Dialog (Name_Dlg, Window)
        =
          GWindows.Constants.IDOK
      then
         Message_Box ("Hello", To_GString_From_Unbounded (Name_Str));
      else
         Message_Box ("What!", "Who do you think you are?");
      end if;
   end Do_TryMe;

   procedure Do_Done
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      GWindows.Application.End_Application;
   end Do_Done;

begin
      Create_Dialog (Window, Resource_ID (IDD_DIALOG2));
      --  Loads dialog resource in to GWindows object, which in this case
      --  describes a regular window. Once loaded, we just treat it as
      --  any other GWindows window object.

      Keyboard_Support (Window);

      Large_Icon (Window, Resource_ID (IDI_ICON1));

      Center (Window);

      Attach_Dialog_Item (Done_Button, Window, IDC_DONE);
      Attach_Dialog_Item (TryMe_Button, Window, IDC_TRYME);
      --  Attach dialog resource items to GWindows objects
      --  Once attached they can be treated like any other GWindows object

      On_Click_Handler (Done_Button, Do_Done'Unrestricted_Access);
      On_Click_Handler (TryMe_Button, Do_TryMe'Unrestricted_Access);
      On_Destroy_Handler (Window, Do_Done'Unrestricted_Access);

      Visible (Window);

      GWindows.Application.Message_Loop;

end Dialog_Example;
