with GWindows.ActiveX;
with GWindows.Base;
with GWindows.Windows;

package GNAVI_Help_Window_Package is

   -------------------------------------------------------------------------
   --  GNAVI_Help_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_Help_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         --  GNAVI: Controls
         html_browser : aliased GWindows.ActiveX.ActiveX_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type GNAVI_Help_Window_Access is
     access all GNAVI_Help_Window_Type;

   type Pointer_To_GNAVI_Help_Window_Class is
     access all GNAVI_Help_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_Help_Window_Type);

   GNAVI_Help_Window : GNAVI_Help_Window_Type;

   procedure Display_Help;

   procedure Display_About;

   procedure Navigate (Page : in GWindows.GString);

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);

end GNAVI_Help_Window_Package;
