------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                           G N A V I _ I C G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--                  Copyright (C) 1999-2004 David Botton                    --
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
-- More information about GNAVI and the most current version can            --
-- be located on the web at http://www.gnavi.org                            --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Input_Sources.File;

with DOM.Core;
with DOM.Readers;

package GNAVI_ICG is

   type GNAVI_Project_Type is
      record
         XML_File            : Input_Sources.File.File_Input;
         Project_Tree        : DOM.Readers.Tree_Reader;
         Project_Document    : DOM.Core.Document;
         Project_Root        : DOM.Core.Element;
         Application_File    : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Load_Project (Project       : in out GNAVI_Project_Type;
                           XML_File_Name :        String);
   --  Load project from XML

   procedure Close_Project (Project : in out GNAVI_Project_Type);
   --  Close project

   procedure Update_Project (Project : in out GNAVI_Project_Type);
   --  Update project, create files, handlers, etc. where needed
   --     recompile sep. Create procedures

   GNAVI_Gen_App_Withs : Boolean := True;
   --  If set to true the application.adb will always contain a 'with'
   --  for every window in project

private

   function Create_Params (Object_Node : DOM.Core.Element) return String;
   --  Used for assembling the create params

end GNAVI_ICG;
