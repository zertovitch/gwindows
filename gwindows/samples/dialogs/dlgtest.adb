with GWindows.Base; use GWindows.Base;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Events;
with GWindows.Application;
with GWindows.Constants;
with GWindows.Message_Boxes;
with GWindows.GStrings;

procedure Dlgtest is
   pragma Linker_Options ("dlgtest.coff");

   ID_EDIT1 : constant := 104;
   ID_BTN1  : constant := 101;
   ID_BTN2  : constant := 102;

   Top : GWindows.Windows.Window_Type;
   E1  : GWindows.Edit_Boxes.Edit_Box_Type;
   B1  : GWindows.Buttons.Button_Type;
   B2  : GWindows.Buttons.Button_Type;

   procedure Do_ModalTest (Window : in out Base_Window_Type'Class) is
      use GWindows.GStrings;

      Dlg : GWindows.Windows.Window_Type;
      TX  : GWindows.Edit_Boxes.Edit_Box_Type;
      ET  : GWindows.GString_Unbounded;

      procedure Do_Destroy
        (Window : in out GWindows.Base.Base_Window_Type'Class);
      --  Grab data

      procedure Do_Destroy
        (Window : in out GWindows.Base.Base_Window_Type'Class)
      is
      begin
         ET := GWindows.GStrings.To_GString_Unbounded
           (GWindows.Edit_Boxes.Text (TX));
      end Do_Destroy;

   begin
      Create_Dialog (Dlg, "#100");
      On_Destroy_Handler (Dlg, Do_Destroy'Unrestricted_Access);
      Text (Dlg, "Test1");
      Center (Dlg);

      Attach_Dialog_Item (TX, Dlg, ID_EDIT1);
      Text (TX, "");
      Focus (TX);

      GWindows.Application.Show_Dialog (Dlg, Top);

      if  Modal_Result (Dlg) = GWindows.Constants.IDOK then
         GWindows.Message_Boxes.Message_Box (Top, "OK", To_GString (ET));
      end if;
   end Do_ModalTest;

begin
   Create_Dialog (Top, "#100");

   Attach_Dialog_Item (E1, Top, ID_EDIT1);
   Text (E1, "HELLO");
   Focus (E1);

   Attach_Dialog_Item (B1, Top, ID_BTN1);
   On_Click_Handler (B1, GWindows.Events.Do_Dialog_Cancel'Access);

   Attach_Dialog_Item (B2, Top, ID_BTN2);
   On_Click_Handler (B2, Do_ModalTest'Unrestricted_Access);

   GWindows.Application.Show_Dialog (Top);

end Dlgtest;
