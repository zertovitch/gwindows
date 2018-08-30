------------------------------------------------------------------------------
--                                                                          --
--          GNATREG - Win32 GNAT Standard Library Registry Tool             --
--                                                                          --
--                          G N A T R E G _ G U I                           --
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

with GWindows.Application; use GWindows.Application;
with GWindows.Base; use GWindows.Base;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.List_Boxes; use GWindows.List_Boxes;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Constants; use GWindows.Constants;
with GWindows.GStrings;

with GNATREG_Lib;

package body GNATREG_GUI is
   --
   --  NB: 32 bit and 64 bit binaries .coff are uncompatible!
   --
   --  pragma Linker_Options ("gnatreg.coff");

   Main_Window   : Main_Window_Type;
   Lib_List      : List_Box_Type;
   Lib_Box       : Edit_Box_Type;
   Path_Box      : Edit_Box_Type;
   Apply_Button  : Default_Button_Type;
   Insert_Button : Button_Type;
   Delete_Button : Button_Type;
   Done_Button   : Cancel_Button_Type;

   procedure Fill_Lib_List;

   procedure Do_Menu_Select
     (Window : in out Base_Window_Type'Class;
      Item   : in     Integer);
   --  Handle menu exit option

   procedure Do_Selection_Change (Window : in out Base_Window_Type'Class);
   --  Selection changed, update Path_Box

   procedure Do_Apply (Window : in out Base_Window_Type'Class);
   --  Do Apply

   procedure Do_Insert (Window : in out Base_Window_Type'Class);
   --  Do Insert

   procedure Do_Delete (Window : in out Base_Window_Type'Class);
   --  Do Delete

   ---------------
   -- Start_GUI --
   ---------------

   procedure Start_GUI is
      Win_Font  : Font_Type;
      Label     : Label_Access;
   begin
      Detach_From_Console;
      Create_As_Dialog (Main_Window, "GNATREG", Width => 525, Height => 180);
      Small_Icon (Main_Window, "Main_Icon");
      Large_Icon (Main_Window, "Main_Icon");
      Menu (Main_Window, "Main_Menu");
      Accelerator_Table (Main_Window, "Main_Menu");
      Show (Main_Window);
      On_Menu_Select_Handler (Main_Window, Do_Menu_Select'Access);
      Create_Stock_Font (Win_Font, Default_GUI);
      Set_Font (Main_Window, Win_Font);
      Keyboard_Support (Main_Window);

      Create (Lib_List, Main_Window, 0, 0, 200, 0);
      On_Selection_Change_Handler (Lib_List, Do_Selection_Change'Access);
      Dock (Lib_List, At_Left);

      Label := new Label_Type;
      Create (Label.all, Main_Window, "Library Name", 210, 0, 300, 22,
              Is_Dynamic => True);
      Create (Lib_Box, Main_Window, "", 210, 20, 300, 22);

      Label := new Label_Type;
      Create (Label.all, Main_Window, "Library Path", 210, 49, 300, 22,
              Is_Dynamic => True);
      Create (Path_Box, Main_Window, "", 210, 68, 300, 22);

      Create (Apply_Button, Main_Window, "&Apply", 210, 100, 60, 25,
              ID => IDOK);
      On_Click_Handler (Apply_Button, Do_Apply'Access);

      Create (Insert_Button, Main_Window, "&Insert", 280, 100, 60, 25);
      On_Click_Handler (Insert_Button, Do_Insert'Access);

      Create (Delete_Button, Main_Window, "De&lete", 350, 100, 60, 25);
      On_Click_Handler (Delete_Button, Do_Delete'Access);

      Create (Done_Button, Main_Window, "&Done", 420, 100, 60, 25,
              ID => IDCANCEL);

      Fill_Lib_List;
      Selected (Lib_List, 1);
      Fire_On_Selection_Change (Lib_List);
      Focus (Lib_List);

      Dock_Children (Main_Window);

      Message_Loop;
   end Start_GUI;

   -------------------
   -- Fill_Lib_List --
   -------------------

   procedure Fill_Lib_List is
      List : GNATREG_Lib.Library_Array :=
        GNATREG_Lib.Get_Libraries;
   begin
      Clear (Lib_List);

      for N in List'Range loop
         Add (Lib_List,
              GWindows.GStrings.To_GString_From_Unbounded (List (N)));
      end loop;

   end Fill_Lib_List;

   -------------------------
   -- Do_Selection_Change --
   -------------------------

   procedure Do_Selection_Change (Window : in out Base_Window_Type'Class) is
      pragma Warnings (Off, Window);
   begin
      Text (Lib_Box, Text (Lib_List));
      Text (Path_Box, GNATREG_Lib.Get_Path (Text (Lib_List)));
   end Do_Selection_Change;

   --------------
   -- Do_Apply --
   --------------

   procedure Do_Apply (Window : in out Base_Window_Type'Class)
   is
      pragma Warnings (Off, Window);
   begin
      GNATREG_Lib.Delete_Library (Text (Lib_List));
      GNATREG_Lib.Set_Library (Text (Lib_Box), Text (Path_Box));
      Fill_Lib_List;
      Text (Lib_List, Text (Lib_Box));
   end Do_Apply;

   ---------------
   -- Do_Insert --
   ---------------

   procedure Do_Insert (Window : in out Base_Window_Type'Class)
   is
      pragma Warnings (Off, Window);
   begin
      GNATREG_Lib.Set_Library (Text (Lib_Box), Text (Path_Box));
      Fill_Lib_List;
      Text (Lib_List, Text (Lib_Box));
   end Do_Insert;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete (Window : in out Base_Window_Type'Class)
   is
      pragma Warnings (Off, Window);
   begin
      GNATREG_Lib.Delete_Library (Text (Lib_List));
      Fill_Lib_List;
      Selected (Lib_List, 1);
      Fire_On_Selection_Change (Lib_List);
   end Do_Delete;

   --------------------
   -- Do_Menu_Select --
   --------------------

   procedure Do_Menu_Select
     (Window : in out Base_Window_Type'Class;
      Item   : in     Integer)
   is
      ID_APP_EXIT : constant := 100;
   begin
      case Item is
         when ID_APP_EXIT =>
            Close (Window);
         when others =>
            null;
      end case;
   end Do_Menu_Select;

end GNATREG_GUI;
