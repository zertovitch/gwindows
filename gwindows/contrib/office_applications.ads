------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                   O F F I C E   A P P L I C A T I O N S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2022 - 2023 Gautier de Montmollin          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

--  Office_Applications facilitates the production of "Office"-like
--  applications with the GWindows framework.
--
--  Office_Applications is used in the following open-source projects:
--
--    AZip   : https://azip.sourceforge.io/
--    LEA    : https://l-e-a.sourceforge.io/
--    TeXCAD : https://texcad.sourceforge.io/
--
--  Mirrors of those projects are located here: https://github.com/zertovitch

with GWindows.Common_Controls,
     GWindows.Image_Lists,
     GWindows.Menus,
     GWindows.Windows.MDI;

package Office_Applications is

   function Shorten_File_Name
      (S : GWindows.GString; Max_Length : Positive) return GWindows.GString;

   --------------------------------------------
   --  MRU (Most Recently Used) files names  --
   --------------------------------------------

   subtype MRU_Range is Integer range 1 .. 9;

   type MRU_Item is record
      Name : GWindows.GString_Unbounded := GWindows.Null_GString_Unbounded;
      Line : Natural := 0;
   end record;

   type MRU_List is array (MRU_Range) of MRU_Item;
   type IDM_MRU_List is array (MRU_Range) of Natural;

   type MRU_Info is record
      Item    : MRU_List;
      ID_Menu : IDM_MRU_List;
   end record;

   --  Add a file name and an eventual line
   --  number (for text documents) to a MRU list.
   --
   procedure Add_MRU
      (MRU            : in out MRU_Info;
       Name           :        GWindows.GString;
       Line           :        Integer := -1;
       Add_To_Desktop :        Boolean := True);

   --  Update a menu, having entries ID matching those in MRU.ID_Menu,
   --  with data contained in the MRU.Item array.
   --
   procedure Update_MRU_Menu
      (MRU  : MRU_Info;
       Menu : GWindows.Menus.Menu_Type);

   -------------------------------------------------------------------
   --                "Classic Office" window layout                 --
   -------------------------------------------------------------------
   --  Multiple sub-windows within a main window.                   --
   --  Eventually the sub-windows are maximized, so only one        --
   --  sub-window is visible at a time.                             --
   --  For instance, in Notepad++, the sub-windows are always       --
   --  maximized.                                                   --
   -------------------------------------------------------------------

   type Classic_Main_Tool_Bar_Type is
      new GWindows.Common_Controls.Toolbar_Control_Type with
   record
      Images       : GWindows.Image_Lists.Image_List_Type;
      String_Count : Natural := 0;
   end record;

   --  Handle clicks on toolbar.
   --  They activate the menu commands associated through
   --  Add_Button to the buttons on tool bar's creation.
   overriding procedure On_Button_Select
      (Control : in out Classic_Main_Tool_Bar_Type;
       Item    : in     Integer);

   ------------------------------------------------
   --  Main "Classic Office" application window  --
   ------------------------------------------------

   type Classic_Main_Window_Type is
      new GWindows.Windows.MDI.MDI_Main_Window_Type with
   record
      Tool_Bar : Classic_Main_Tool_Bar_Type;
      MRU      : MRU_Info;
   end record;

   type Classic_Main_Window_Access is access all Classic_Main_Window_Type;
   type Access_To_Classic_Main_Window_Class is
      access all Classic_Main_Window_Type'Class;

   --  Close the document (usually blank) that might be created on
   --  application's startup. If that document was modified by the
   --  user, we give up closing it.
   --
   procedure Close_Initial_Document
     (Main_Window : in out Classic_Main_Window_Type);

   ----------------------------------------------
   --  Child "Classic Office" document window  --
   ----------------------------------------------

   type Classic_Document_Window_Type is
      abstract new GWindows.Windows.MDI.MDI_Child_Window_Type with
         record
            --  Set Extra_First_Doc = True for the new document
            --  that is appearing on startup and should be closed
            --  if it was kept virgin at the moment the user opened
            --  another document:
            Extra_First_Doc : Boolean := False;
         end record;

   function Is_Document_Modified
     (Child_Window : Classic_Document_Window_Type) return Boolean is abstract;

end Office_Applications;
