with GNAVI_Controls;
with GWindows.Image_Lists;

package body GNAVI_Controls_Window_Package is

   procedure On_Create
     (Window : in out GNAVI_Controls_Window_Type) is separate;

   -------------------------------------------------------------------------
   --  Public Methods
   -------------------------------------------------------------------------

   function Current_Control_Index return Positive is
   begin
      for i in 1 .. GNAVI_Controls_Window.Controls_List.Item_Count loop
         if GNAVI_Controls_Window.Controls_List.Is_Selected (i - 1) then
            return i;
         end if;
      end loop;
      return 1;
   end Current_Control_Index;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

   List : GWindows.Image_Lists.Image_List_Type;

   procedure Do_Create
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GNAVI_Controls, GWindows.Common_Controls;

      This : GNAVI_Controls_Window_Type renames
        GNAVI_Controls_Window_Type (Window);

      icon_width : constant := 16;

   begin
      List.Create ("CONTROL_TOOLBOX", icon_width);
      This.Controls_List.Set_Image_List (Small, List);
      This.Controls_List.Insert_Column ("", 0, This.Client_Area_Width - icon_width - 8);
      This.Controls_List.Set_Extended_Style (Full_Row_Select);

      Dock_Children (This);

      for N in 1 .. Control_Count loop
         if Control_Category (N) = "Windows" then
            exit;
         end if;

         This.Controls_List.Insert_Item
            (Control_Display_Name (N),
             N - 1,
             Icon => Control_Icon (N));
      end loop;

      This.Controls_List.Selected (0, True);

   end Do_Create;

--  GNAVI: Create Global Instance
end GNAVI_Controls_Window_Package;
