with GWindows.Static_Controls;
with GWindows.Buttons;
with GWindows.Edit_Boxes;
with GWindows.List_Boxes;
with GWindows.Packing_Boxes;
with GWindows.Windows;
with GWindows.Base;

package GNAVI_New_Window_Package is

   -------------------------------------------------------------------------
   --  GNAVI_New_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_New_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         --  GNAVI: Controls
         New_Window_Box : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Window_Label : aliased GWindows.Static_Controls.Label_Type;
         Window_Name_Box : aliased GWindows.Edit_Boxes.Edit_Box_Type;
         Window_Type_Label : aliased GWindows.Static_Controls.Label_Type;
         Window_Type_List : aliased GWindows.List_Boxes.List_Box_Type;
         Description_Box : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Button_Pack_Box : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         OK_Button : aliased GWindows.Buttons.Default_Button_Type;
         Cancel_Button : aliased GWindows.Buttons.Cancel_Button_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type GNAVI_New_Window_Access is
     access all GNAVI_New_Window_Type;

   type Pointer_To_GNAVI_New_Window_Class is
     access all GNAVI_New_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_New_Window_Type);

   GNAVI_New_Window : GNAVI_New_Window_Type;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_OK
     (Window : in out GWindows.Base.Base_Window_Type'Class);

end GNAVI_New_Window_Package;

