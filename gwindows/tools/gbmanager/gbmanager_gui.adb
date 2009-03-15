------------------------------------------------------------------------------
--                                                                          --
--                 GBManager - Win32 COM Binding Manager                    --
--                                                                          --
--                       G B M A N A G E R _ G U I                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Application;
with GWindows.Base;
with GWindows.Windows.Main;
with GWindows.Menus;
with GWindows.Common_Dialogs;

with GBManager_Tree;
with GBManager_Lib;

package body GBManager_GUI is
   use GBManager_Tree;
   use GWindows.Windows.Main;
   use GWindows.Menus;

   ID_Exit  : constant := 100;
   ID_About : constant := 101;
   ID_Bind  : constant := 102;
   ID_Set   : constant := 103;
   ID_Look  : constant := 104;

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer);
   --  Handle menu selections;

   Main_Window : Main_Window_Type;
   Lib_Tree    : GBManager_Tree_Control_Type;

   ---------------
   -- Start_GUI --
   ---------------

   procedure Start_GUI is

      Main_Menu   : Menu_Type := Create_Menu;
      File_Menu   : Menu_Type := Create_Popup;

   begin
      Create (Main_Window,
              "GNATCOM GUI Binding Manager - Binding Repository at: " &
                GBManager_Lib.Get_Binding_Location);

      Append_Item (File_Menu, "&Bind", ID_Bind);
      Append_Item (File_Menu, "&View Library", ID_Look);
      Append_Item (File_Menu, "&Set Binding Repository Location", ID_Set);
      Append_Separator (File_Menu);
      Append_Item (File_Menu, "E&xit", ID_Exit);

      Append_Menu (Main_Menu, "&Files", File_Menu);
      Menu (Main_Window, Main_Menu);
      On_Menu_Select_Handler (Main_Window, Do_Menu_Select'Access);

      Large_Icon (Main_Window, "Main_Icon");

      Create (Lib_Tree, Main_Window,
              10, 10, 300, 300);
      Border (Lib_Tree);
      Dock (Lib_Tree, GWindows.Base.Fill);

      Visible (Main_Window, True);
      Dock_Children (Main_Window);

      GWindows.Application.Message_Loop;
   end Start_GUI;

   procedure Do_Menu_Select
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Item   : in     Integer)
   is
      pragma Warnings (Off, Window);
   begin
      case Item is
         when ID_Exit =>
            Close (Main_Window);
         when ID_Bind =>
            Do_Binding (Lib_Tree);
         when ID_Look =>
            Do_Look (Lib_Tree);
         when ID_Set =>
            GBManager_Lib.Set_Binding_Location
              (GWindows.Common_Dialogs.Get_Directory
                 (Main_Window, "Repository Location"));
            Text (Main_Window,
                  "GNATCOM GUI Binding Manager - Binding Repository at: " &
                    GBManager_Lib.Get_Binding_Location);
         when ID_About =>
            null;
         when others =>
            null;
      end case;
   end Do_Menu_Select;

end GBManager_GUI;
