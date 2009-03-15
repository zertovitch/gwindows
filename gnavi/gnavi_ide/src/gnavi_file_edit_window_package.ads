with GWindows.Scintilla;
with GWindows.Windows.MDI;
with GWindows.Windows;
with GWindows.Base;

package GNAVI_File_Edit_Window_Package is

   -------------------------------------------------------------------------
   --  GNAVI_File_Edit_Window Specs
   -------------------------------------------------------------------------

   type GNAVI_File_Edit_Window_Type is
     new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
         --  GNAVI: Controls
         Edit_Box : aliased GWindows.Scintilla.Scintilla_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type GNAVI_File_Edit_Window_Access is
     access all GNAVI_File_Edit_Window_Type;

   type Pointer_To_GNAVI_File_Edit_Window_Class is
     access all GNAVI_File_Edit_Window_Type'Class;

   procedure On_Create (Window : in out GNAVI_File_Edit_Window_Type);

   GNAVI_File_Edit_Window : GNAVI_File_Edit_Window_Type;

   -------------------------------------------------------------------------
   --  Public Package Methods
   -------------------------------------------------------------------------

   procedure New_File;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Check_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer;
      Kind   : in     GWindows.Windows.Hover_Item_Type);

   procedure Handle_Menu
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);

end GNAVI_File_Edit_Window_Package;
