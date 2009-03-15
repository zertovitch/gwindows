with GNAVI_Main_Package;
with GNAVI_Datastore;

with GWindows.GStrings;

with GNATCOM.Dispinterface;

package body GNAVI_Help_Window_Package is

   procedure On_Create (Window : in out GNAVI_Help_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
   begin
      null;
   end Do_Create;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
   begin
      Can_Close := True;
   end Do_Close;

   procedure Display_Help
   is
      use GNAVI_Datastore;
   begin
      Navigate (IDE_Dir & "help\" & "index.html");
   end Display_Help;

   procedure Display_About
   is
      use GNAVI_Datastore;
   begin
      Navigate (IDE_Dir & "help\" & "about.html");
   end Display_About;

   procedure Navigate (Page : in GWindows.GString)
   is
      use GNATCOM.Dispinterface;
      use GWindows.GStrings;
      use GWindows.ActiveX;

      Explorer : Dispinterface_Type;
   begin
      if not Valid (GNAVI_Help_Window) then
         Create (GNAVI_Help_Window);
      end if;

      Show (GNAVI_Help_Window);
      Focus (GNAVI_Help_Window);

      Query (Explorer, Interface (GNAVI_Help_Window.HTML_Browser));

      Invoke (Explorer, "navigate",
              (1 => To_VARIANT_From_GString (Page)));
   end Navigate;

end GNAVI_Help_Window_Package;
