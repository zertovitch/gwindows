with GWindows.GControls.GSize_Bars;
with GWindows.Common_Controls;
with GWindows.Panels;
with GWindows.List_Boxes;
with GWindows.Windows;
with GWindows.Base;

with GNAVI_Project;

package GNAVI_Project_Window_Package is

   -------------------------------------------------------------------------
   --  GNAVI_Project_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_Project_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         --  GNAVI: Controls
         Right_Size_Bar : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         Project_Panel : aliased GWindows.Panels.Panel_Type;
         Project_Tools : aliased GWindows.Common_Controls.Toolbar_Control_Type;
         Window_Type_List : aliased GWindows.List_Boxes.List_Box_Type;
         File_Panel : aliased GWindows.Panels.Panel_Type;
         File_Size_Bar : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         File_List : aliased GWindows.List_Boxes.List_Box_Type;
         --  GNAVI: Add custom data below this comment
         Project : GNAVI_Project.GNAVI_Project_Type;
      end record;

   type GNAVI_Project_Window_Access is
     access all GNAVI_Project_Window_Type;

   type Pointer_To_GNAVI_Project_Window_Class is
     access all GNAVI_Project_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_Project_Window_Type);

   GNAVI_Project_Window : GNAVI_Project_Window_Type;

   -------------------------------------------------------------------------
   --  Public Package Methods
   -------------------------------------------------------------------------

   procedure Load_Project (File_Name : in GWindows.GString);

   procedure New_Window;

   procedure Edit_Window;

   procedure Add_Window (File_Name : in GWindows.GString);

   procedure Add_Existing_Window;

   procedure Delete_Window;

   procedure Save_Project;

   procedure Refresh_Project;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);

   procedure Do_New_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Delete_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Edit_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Add_Window
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Window_Selection
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Toolbar_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);

   procedure Do_File_Edit
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_File_Selection
     (Window : in out GWindows.Base.Base_Window_Type'Class);

end GNAVI_Project_Window_Package;
