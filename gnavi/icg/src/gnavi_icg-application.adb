------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                G N A V I _ I C G . A P P L I C A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories,
     Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with GNAT.Case_Util;
with GNAT.OS_Lib;

with GNAVI_Templates;
with Templates_Parser;

package body GNAVI_ICG.Application is

   procedure Update_Application (Project : in out GNAVI_Project_Type)
   is
      use DOM.Core;

      NL : constant Node_List :=
        Elements.Get_Elements_By_Tag_Name (Project.Project_Root,
                                           "application");
      procedure Proceed is
         use Templates_Parser;
         App_Node               : constant DOM.Core.Element := Nodes.Item (NL, 0);

         Application_Name       : constant String :=
           Elements.Get_Attribute (App_Node, "name");

         Application_Lower_Name : constant String :=
           GNAT.Case_Util.To_Lower (Application_Name);

         App_File_Name          : constant String := Application_Lower_Name & ".adb";
         GPR_File_Name          : constant String := Application_Lower_Name & ".gpr";
         RC_File_Name           : constant String := Application_Lower_Name & ".rc";
         Manifest_File_Name     : constant String := "res\manifest.xml";

         Trans                  : constant Translate_Table :=
           (1 => Assoc ("Application_Name",       Application_Name),
            2 => Assoc ("Application_Lower_Name", Application_Lower_Name));

         procedure Create_Missing
            (file_name : String;
             template  : GNAVI_Templates.Template_Kind) is
         begin
            if not GNAT.OS_Lib.Is_Regular_File (file_name) then
               GNAVI_Templates.Execute (file_name, template, Trans);
            end if;
         end Create_Missing;

      begin
         Project.Application_File :=
           Ada.Strings.Unbounded.To_Unbounded_String (App_File_Name);

         Create_Missing (App_File_Name, GNAVI_Templates.application_template);
         Create_Missing (GPR_File_Name, GNAVI_Templates.gnat_project_file_template);
         Create_Missing (RC_File_Name,  GNAVI_Templates.resource_compiler_file_template);
         Ada.Directories.Create_Path ("res");
         --  Create the strange file that, included to the binary resources, will
         --  enable the post Windows-2000 "visual styles":
         Create_Missing (Manifest_File_Name, GNAVI_Templates.manifest_template);
      end Proceed;

   begin
      --  First, load settings from XML project
      if Nodes.Length (NL) > 0 then
         Proceed;
      end if;
   end Update_Application;

end GNAVI_ICG.Application;
