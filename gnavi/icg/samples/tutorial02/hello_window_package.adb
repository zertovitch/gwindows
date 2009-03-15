with GWindows.Message_Boxes; use GWindows.Message_Boxes;

package body Hello_Window_Package is

   procedure On_Create (Window : in out Hello_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Dock_Children (Hello_Window);
      GWindows.Packing_Boxes.Pack (Hello_Window.Hello_Pack);
   end Do_Create;

   procedure Do_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      Message_Box ("Alert...", "Ouch! That smarts!", Icon => Asterisk_Icon);
   end Do_Click;

--  GNAVI: Create Global Instance
begin
   Create (Hello_Window);
end Hello_Window_Package;

