------------------------------------------------------------------------------
--                                                                          --
--                 GBManager - Win32 COM Binding Manager                    --
--                                                                          --
--                     G B M A N A G E R _ T R E E                          --
--                                                                          --
--                                 S p e c                                  --
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

with GWindows.Common_Controls;

package GBManager_Tree is

   -------------------------------------------------------------------------
   --  GBManager_Tree_Control_Type
   -------------------------------------------------------------------------

   type GBManager_Tree_Control_Type is
     new GWindows.Common_Controls.Tree_View_Control_Type with null record;
   type GBManager_Tree_Control_Access is
     access all GBManager_Tree_Control_Type;
   type Pointer_To_GBManager_Tree_Control_Class is
     access all GBManager_Tree_Control_Type'Class;

   procedure Do_Binding (Control : in out GBManager_Tree_Control_Type);
   --  Generate COM bindings

   procedure Do_Look (Control : in out GBManager_Tree_Control_Type);
   --  Generate COM documentation

   procedure On_Create (Window : in out GBManager_Tree_Control_Type);
   --  Fill in tree

   procedure On_Double_Click (Control : in out GBManager_Tree_Control_Type);

   procedure On_Return (Control : in out GBManager_Tree_Control_Type);

end GBManager_Tree;
