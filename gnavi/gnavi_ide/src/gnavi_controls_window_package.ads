with GWindows.Common_Controls;
with GWindows.GControls.GSize_Bars;
with GWindows.List_Boxes;
with GWindows.Panels;
with GWindows.Windows;
with GWindows.Base;

package GNAVI_Controls_Window_Package is

   -------------------------------------------------------------------------
   --  GNAVI_Controls_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_Controls_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         --  GNAVI: Controls
         Left_Size_Bar : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         Controls_Panel : aliased GWindows.Panels.Panel_Type;
         Controls_List : aliased GWindows.Common_Controls.List_View_Control_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type GNAVI_Controls_Window_Access is
     access all GNAVI_Controls_Window_Type;

   type Pointer_To_GNAVI_Controls_Window_Class is
     access all GNAVI_Controls_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_Controls_Window_Type);

   GNAVI_Controls_Window : GNAVI_Controls_Window_Type;

   -------------------------------------------------------------------------
   --  Public Methods
   -------------------------------------------------------------------------

   function Current_Control_Index return Positive;
   --  Returns the Index of the currently selected control

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

end GNAVI_Controls_Window_Package;
