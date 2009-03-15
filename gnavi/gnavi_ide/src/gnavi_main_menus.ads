with GWindows.Menus;

package GNAVI_Main_Menus is

   type Base_Menus is
      record
         Main_Menu    : GWindows.Menus.Menu_Type;
         File_Menu    : GWindows.Menus.Menu_Type;
         Edit_Menu    : GWindows.Menus.Menu_Type;
         Search_Menu  : GWindows.Menus.Menu_Type;
         Project_Menu : GWindows.Menus.Menu_Type;
         Window_Menu  : GWindows.Menus.Menu_Type;
         Help_Menu    : GWindows.Menus.Menu_Type;
         Windows_Menu : Positive := 1;
      end record;

   Project_Loaded  : Boolean := False;
   Window_Selected : Boolean := False;
   File_Selected   : Boolean := False;
   --  Menu States

   function Setup_Base_Menus return Base_Menus;

   function Setup_Editor_Menus return Base_Menus;

   function Menu_Description (Command : Integer) return GWindows.GString;

   procedure Set_Menu_States (Menu : in out GWindows.Menus.Menu_Type);

end GNAVI_Main_Menus;
