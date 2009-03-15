with GWindows.Static_Controls;
with GWindows.Packing_Boxes;
with GWindows.Windows.Main;
with GWindows.Base;

package Hello_Window_Package is

   -------------------------------------------------------------------------
   --  Hello_Window Specs
   -------------------------------------------------------------------------

   type Hello_Window_Type is
     new GWindows.Windows.Main.Main_Window_Type with
      record
         --  GNAVI: Controls
         Hello_Pack : GWindows.Packing_Boxes.Packing_Box_Type;
         Hello_Label : GWindows.Static_Controls.Label_Type;
         --  GNAVI: Add custom data below this comment
      end record;

   type Hello_Window_Access is
     access all Hello_Window_Type;

   type Pointer_To_Hello_Window_Class is
     access all Hello_Window_Type'Class;

   procedure On_Create (Window : in out Hello_Window_Type);

   Hello_Window : Hello_Window_Type;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class);

end Hello_Window_Package;


