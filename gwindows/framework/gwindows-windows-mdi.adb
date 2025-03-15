------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--                 G W I N D O W S . W I N D O W S . M D I                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2022 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------
--  Window types to ease in the creation of Multi Document Interface
--  Applications

with GWindows.Application;

package body GWindows.Windows.MDI is

   --------------
   -- MDI_Menu --
   --------------

   procedure MDI_Menu
     (Window      : in out MDI_Main_Window_Type;
      Menu        : in     GWindows.Menus.Menu_Type;
      Window_Menu : in     Positive)
   is
   begin
      Window.Top_Menu := Menu;
      Window.Window_Menu_Location := Window_Menu;
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Window), Menu, Window_Menu);
   end MDI_Menu;

   --------------
   -- MDI_Menu --
   --------------

   procedure MDI_Menu
     (Window      : in out MDI_Child_Window_Type;
      Menu        : in     GWindows.Menus.Menu_Type;
      Window_Menu : in     Positive)
   is
   begin
      Window.Child_Menu := Menu;
      Window.Window_Menu_Location := Window_Menu;
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Controlling_Parent (Window).all),
         Menu,
         Window_Menu);
   end MDI_Menu;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out MDI_Main_Window_Type) is
   begin
      GWindows.Menus.Destroy_Menu (Window.Top_Menu);
      GWindows.Application.End_Application;
   end On_Destroy;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out MDI_Child_Window_Type) is
   begin
      GWindows.Menus.Destroy_Menu (Window.Child_Menu);
   end On_Destroy;

   ---------------------
   -- On_MDI_Activate --
   ---------------------

   procedure On_MDI_Activate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent_Win : constant Window_Access :=
        Window_Access (Controlling_Parent (Window));
   begin
      GWindows.Windows.MDI_Menu
        (Parent_Win.all, Window.Child_Menu, Window.Window_Menu_Location);
   end On_MDI_Activate;

   -----------------------
   -- On_MDI_Deactivate --
   -----------------------

   procedure On_MDI_Deactivate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent_Win : constant MDI_Main_Window_Access :=
        MDI_Main_Window_Access (Controlling_Parent (Window));
   begin
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Access (Parent_Win).all,
         Parent_Win.Top_Menu, Parent_Win.Window_Menu_Location);
   end On_MDI_Deactivate;

   -------------------
   -- MDI_Close_All --
   -------------------

   procedure MDI_Close_All (Window : in out MDI_Main_Window_Type)
   is
   begin
      GWindows.Windows.MDI_Menu
        (GWindows.Windows.Window_Type (Window),
         Window.Top_Menu,
         Window.Window_Menu_Location);
      GWindows.Windows.MDI_Close_All (GWindows.Windows.Window_Type (Window));
   end MDI_Close_All;

   ------------------------
   -- Count_MDI_Children --
   ------------------------

   function Count_MDI_Children (Window : in out MDI_Main_Window_Type)
   return Natural
   is
      Count : Natural := 0;
      procedure Count_MDI_Child_Window
        (Child_Window : GWindows.Base.Pointer_To_Base_Window_Class)
      is
      begin
         if Child_Window.all in MDI_Child_Window_Type'Class then
            Count := Count + 1;
         end if;
      end Count_MDI_Child_Window;
   begin
      GWindows.Base.Enumerate_Children (
         MDI_Client_Window (Window).all,
         Count_MDI_Child_Window'Unrestricted_Access
      );
      return Count;
   end Count_MDI_Children;

   --------------
   -- Activate --
   --------------

   procedure Activate (Window : in out MDI_Child_Window_Type) is
      pragma Warnings (Off, Window);
      Parent : GWindows.Windows.Window_Type renames
        GWindows.Windows.Window_Type (Controlling_Parent (Window).all);
   begin
      GWindows.Windows.MDI_Active_Window (Parent, Window);
   end Activate;

end GWindows.Windows.MDI;
