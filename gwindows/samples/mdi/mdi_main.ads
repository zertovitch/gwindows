with GWindows.Windows.MDI;
with GWindows.Menus;
with GWindows.Common_Controls;
with GWindows.Image_Lists;

package MDI_Main is

   type MDI_Status_Bar_Type is
     new GWindows.Common_Controls.Status_Bar_Type with null record;

   procedure On_Right_Click (Control : in out MDI_Status_Bar_Type);
   --  Handle right clicks on status bar

   type MDI_Toolbar_Type is
     new GWindows.Common_Controls.Toolbar_Control_Type with null record;

   procedure On_Button_Select (Control : in out MDI_Toolbar_Type;
                               Item    : in     Integer);
   --  Handle click on toolbar

   type MDI_Main_Type is
     new GWindows.Windows.MDI.MDI_Main_Window_Type with
      record
         Tool_Bar             : MDI_Toolbar_Type;
         Images               : GWindows.Image_Lists.Image_List_Type;
         Status_Bar           : MDI_Status_Bar_Type;
      end record;
   type MDI_Main_Access is access all MDI_Main_Type;

   procedure On_File_New (Window : in out MDI_Main_Type);
   --  File|New event

   procedure On_File_Open (Window : in out MDI_Main_Type);
   --  File|Open event

   procedure On_About (Window : in out MDI_Main_Type);
   --  Help|About event

   procedure On_Create (Window : in out MDI_Main_Type);
   --  Handles setting up icons, menus, etc.

   procedure On_Menu_Hover (Window  : in out MDI_Main_Type;
                            Item    : in     Integer;
                            Kind    : in     GWindows.Windows.Hover_Item_Type);
   --  Handle setting status bar

   procedure On_Menu_Select (Window : in out MDI_Main_Type;
                             Item   : in     Integer);
   --  Handle standard menu selections

end MDI_Main;
