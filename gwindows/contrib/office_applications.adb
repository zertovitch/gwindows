with GWindows.Base;

package body Office_Applications is

   overriding procedure On_Button_Select
      (Control : in out Classic_Main_Tool_Bar_Type;
       Item    : in     Integer)
   is
      Parent : constant Access_To_Classic_Main_Window_Class :=
         Access_To_Classic_Main_Window_Class (Controlling_Parent (Control));
  begin
     --  A click simulates a menu entry selection.
     On_Menu_Select (Parent.all, Item);
  end On_Button_Select;

   procedure Close_Initial_Document
      (Main_Window : in out Classic_Main_Window_Type)
   is
      use GWindows.Base;

      procedure Try_Closing_Initial_Document
         (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
      is
      begin
         if Any_Window /= null
            and then Any_Window.all in Classic_Document_Window_Type'Class
         then
            declare
               W : Classic_Document_Window_Type renames
                  Classic_Document_Window_Type (Any_Window.all);
            begin
               if W.Extra_First_Doc
                  and then not
                     Is_Document_Modified
                        (Classic_Document_Window_Type'Class (W))
               then
                  --  This situation happens only if the startup (usually
                  --  blank) document is at its initial state.
                  --  Contents are either untouched, or with all
                  --  modifications undone.
                  Close (Any_Window.all);
               end if;
            end;
         end if;
      end Try_Closing_Initial_Document;

   begin
      Enumerate_Children
         (MDI_Client_Window (Main_Window).all,
          Try_Closing_Initial_Document'Unrestricted_Access);
   end Close_Initial_Document;

end Office_Applications;
