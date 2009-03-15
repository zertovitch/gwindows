with GWindows.Windows.MDI;
with GWindows.Edit_Boxes;
with GWindows.Menus;

package MDI_Edit_Child is

   type MDI_Edit_Child_Type is
     new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
         Edit_Box   : GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         File_Name  : GWindows.GString_Unbounded;
      end record;
   type MDI_Edit_Child_Access is access all MDI_Edit_Child_Type;

   procedure Save (Window    : in out MDI_Edit_Child_Type;
                   File_Name : in     GWindows.GString);
   procedure On_Save (Window : in out MDI_Edit_Child_Type);
   procedure On_Save_As (Window : in out MDI_Edit_Child_Type);
   --  Handles file saves

   procedure On_Create (Window : in out MDI_Edit_Child_Type);
   --  Handles creating window

   procedure On_Menu_Select
     (Window : in out MDI_Edit_Child_Type;
      Item   : in     Integer);
   --  Handles menu selections

end MDI_Edit_Child;
