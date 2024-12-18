with GWindows.Common_Controls;
with GWindows.Windows.MDI;
with GWindows.Base;

package GNAVI_Main_Package is

   -------------------------------------------------------------------------
   --  GNAVI_Main Specs
   -------------------------------------------------------------------------

   type GNAVI_Main_Type is
     new GWindows.Windows.MDI.MDI_Main_Window_Type with
      record
         --  GNAVI: Controls
         Top_Tools : aliased GWindows.Common_Controls.Toolbar_Control_Type;
         Bottom_Status : aliased GWindows.Common_Controls.Status_Bar_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type GNAVI_Main_Access is
     access all GNAVI_Main_Type;

   type Pointer_To_GNAVI_Main_Class is
     access all GNAVI_Main_Type'Class;

   procedure On_Create (Window : in out GNAVI_Main_Type);

   --  On_Menu_Select added by GdM, July 2012
   procedure On_Menu_Select (Window : in out GNAVI_Main_Type;
                             Item   : in     Integer);

   GNAVI_Main : GNAVI_Main_Type;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_New_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Open_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Save_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Close_GNAVI_Project
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Drop_GNAVI_Project
     (Window     : in out GWindows.Base.Base_Window_Type'Class;
      File_Names : in     GWindows.Windows.Array_Of_File_Names);

   procedure Do_Compile
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Run
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type);
      
   procedure Do_Toolbar_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);

end GNAVI_Main_Package;
