with GNAVI_Controls;

package body GNAVI_Controls_Window_Package is

   procedure On_Create
     (Window : in out GNAVI_Controls_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Public Methods
   -------------------------------------------------------------------------

   function Current_Control_Index return Positive is
      use GWindows.List_Boxes;
   begin
      return Current (GNAVI_Controls_Window.Controls_List);
   end Current_Control_Index;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Controls;
      use GWindows.List_Boxes;

      This : GNAVI_Controls_Window_Type renames
        GNAVI_Controls_Window_Type (Window);
   begin
      Dock_Children (This);

      for N in 1 .. Control_Count loop
         if Control_Category (N) = "Windows" then
            exit;
         end if;

         Add (This.Controls_List, Control_Display_Name (N));
      end loop;

      Current (This.Controls_List, 1);
   end Do_Create;

--  GNAVI: Create Global Instance
end GNAVI_Controls_Window_Package;
