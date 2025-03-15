------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--                 G W I N D O W S . W I N D O W S . M D I                  --
--                                                                          --
--                                 S p e c                                  --
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

with GWindows.Menus;

package GWindows.Windows.MDI is

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type
   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type is a top level MDI window that when closed will
   --  also close the current message loop

   type MDI_Main_Window_Type is new Window_Type with private;
   type MDI_Main_Window_Access is access all MDI_Main_Window_Type;
   type Pointer_To_MDI_Main_Window_Class is
     access all MDI_Main_Window_Type'Class;

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Properties
   -------------------------------------------------------------------------

   procedure MDI_Menu (Window      : in out MDI_Main_Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive);
   --  Sets the MDI Top window menu and uses the sub menu Window_Menu for
   --  MDI window list

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Properties
   -------------------------------------------------------------------------

   procedure MDI_Close_All (Window : in out MDI_Main_Window_Type);
   --  Closes all MDI child windows

   -------------------------------------------------------------------------
   --  MDI_Main_Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Destroy (Window : in out MDI_Main_Window_Type);
   --  Handles closing down the message loop when the window is closed
   --  and destroying the menu

   function Count_MDI_Children (Window : in out MDI_Main_Window_Type)
   return Natural;
   --  Count child windows that are in the MDI_Child_Window_Type class

   -------------------------------------------------------------------------
   --  MDI_Child_Window_Type
   -------------------------------------------------------------------------
   --  MDI_Child_Window_Type is a sub-window of a MDI_Main_Window_Type

   type MDI_Child_Window_Type is new Window_Type with private;
   type MDI_Child_Window_Access is access all MDI_Child_Window_Type;
   type Pointer_To_MDI_Child_Window_Class is
     access all MDI_Child_Window_Type'Class;

   procedure MDI_Menu (Window      : in out MDI_Child_Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive);
   --  Sets the MDI Top window menu and uses the sub menu Window_Menu for
   --  MDI window list

   procedure Activate (Window : in out MDI_Child_Window_Type);
   --  Make this the active MDI window

   -------------------------------------------------------------------------
   --  MDI_Child_Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_MDI_Activate (Window : in out MDI_Child_Window_Type);
   procedure On_MDI_Deactivate (Window : in out MDI_Child_Window_Type);
   --  Handles switching menus

   procedure On_Destroy (Window : in out MDI_Child_Window_Type);
   --  Handles destroying the menu

private

   type MDI_Main_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         Top_Menu             : GWindows.Menus.Menu_Type;
         Window_Menu_Location : Positive;
      end record;

   type MDI_Child_Window_Type is
     new GWindows.Windows.Window_Type with
      record
         Child_Menu             : GWindows.Menus.Menu_Type;
         Window_Menu_Location : Positive;
      end record;

end GWindows.Windows.MDI;
