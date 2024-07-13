with GWindows.Scroll_Panels;
with GWindows.Panels;
with GWindows.Windows.MDI;
with GWindows.Scintilla;
with GWindows.List_Boxes;
with GWindows.Edit_Boxes;
with GWindows.Buttons;
with GWindows.Packing_Boxes;
with GWindows.Windows;
with GWindows.Base;

with GNAVI_Common;
with GNAVI_Layout_View;
with GNAVI_Window;

with GNAT.OS_Lib;

package GNAVI_Edit_Window_Package is

   Epoch : constant GNAVI_Common.Time_Stamp := GNAT.OS_Lib.GM_Time_Of (1900, 1, 1, 0, 0, 0);

   -------------------------------------------------------------------------
   --  GNAVI_Edit_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_Edit_Window_Type is
     new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
         --  GNAVI: Controls
         Button_Pack_Box : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Save_Button : aliased GWindows.Buttons.Button_Type;
         Tab_Pack_Box : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Spec_Button : aliased GWindows.Buttons.Button_Type;
         Body_Button : aliased GWindows.Buttons.Button_Type;
         XML_Button : aliased GWindows.Buttons.Button_Type;
         Outline_Button : aliased GWindows.Buttons.Button_Type;
         Layout_Button : aliased GWindows.Buttons.Button_Type;
         Body_Edit_Box : aliased GWindows.Scintilla.Scintilla_Type;
         Spec_Edit_Box : aliased GWindows.Scintilla.Scintilla_Type;
         XML_Edit_Box : aliased GWindows.Scintilla.Scintilla_Type;
         Outline_Box : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Outline_Panel : aliased GWindows.Panels.Panel_Type;
         Outline_View : aliased GWindows.List_Boxes.List_Box_Type;
         Outline_Control_Panel : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Add_Control_Button : aliased GWindows.Buttons.Button_Type;
         Delete_Control_Button : aliased GWindows.Buttons.Button_Type;
         Up_Control_Button : aliased GWindows.Buttons.Button_Type;
         Down_Control_Button : aliased GWindows.Buttons.Button_Type;
         Right_Control_Button : aliased GWindows.Buttons.Button_Type;
         Properties_Section : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Properties_Box : aliased GWindows.Panels.Panel_Type;
         Property_Edit_Panel : aliased GWindows.Panels.Panel_Type;
         Property_Set_Button : aliased GWindows.Buttons.Button_Type;
         Property_Edit_Box : aliased GWindows.Edit_Boxes.Edit_Box_Type;
         Properties_View : aliased GWindows.List_Boxes.List_Box_Type;
         Handlers_Box : aliased GWindows.Panels.Panel_Type;
         Handler_Edit_Panel : aliased GWindows.Panels.Panel_Type;
         Handler_Set_Button : aliased GWindows.Buttons.Button_Type;
         Handler_Edit_Box : aliased GWindows.Edit_Boxes.Edit_Box_Type;
         Handlers_View : aliased GWindows.List_Boxes.List_Box_Type;
         Layout_Box : aliased GWindows.Scroll_Panels.Scroll_Panel_Type;
         --  GNAVI: Add custom data below this comment

         Win_XML  : GNAVI_Window.GNAVI_Window_Type;
         Spec_TS  : GNAVI_Common.Time_Stamp := Epoch;
         XML_TS   : GNAVI_Common.Time_Stamp := Epoch;
         Body_TS  : GNAVI_Common.Time_Stamp := Epoch;
         OL_TS    : GNAVI_Common.Time_Stamp := Epoch;

         Edit_Box      : GWindows.Scintilla.Scintilla_Access := null;
         Layout_Editor : GNAVI_Layout_View.Layout_View_Type;
      end record;

   type GNAVI_Edit_Window_Access is
     access all GNAVI_Edit_Window_Type;

   type Pointer_To_GNAVI_Edit_Window_Class is
     access all GNAVI_Edit_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_Edit_Window_Type);

   --  On_Menu_Select added by GdM, July 2012
   procedure On_Menu_Select (Window : in out GNAVI_Edit_Window_Type;
                             Item   : in     Integer);

   -------------------------------------------------------------------------
   --  Public Methods
   -------------------------------------------------------------------------

   procedure Open_Window (Name : GWindows.GString);
   --  Creates a new Edit_Window for Name

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);

   procedure Do_Save_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_XML_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Outline_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Layout_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Spec_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Body_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Select_OV_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Jump_To_Handler
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Add_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Jump_To_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Delete_Control
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);

   procedure Do_Property_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Property_Value_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Handler_Value_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Handler_Change
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type);

   procedure Do_Control_Up
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Control_Down
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Control_Right
     (Window : in out GWindows.Base.Base_Window_Type'Class);

end GNAVI_Edit_Window_Package;
