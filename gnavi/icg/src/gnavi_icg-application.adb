------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                G N A V I _ I C G . A P P L I C A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.2 $
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with GNAT.Case_Util;
with GNAT.OS_Lib;

with Templates;
with Templates_Parser;

package body GNAVI_ICG.Application is

   procedure Update_Application (Project : in out GNAVI_Project_Type)
   is
      use DOM.Core;

      NL : constant Node_List :=
        Elements.Get_Elements_By_Tag_Name (Project.Project_Root,
                                           "application");
   begin
      --  First load settings from XML project

      if Nodes.Length (NL) > 0 then
         declare
            use Templates_Parser;

            App_Node         : constant DOM.Core.Element := Nodes.Item (NL, 0);

            Application_Name : constant String :=
              Elements.Get_Attribute (App_Node, "name");

            App_File_Name    : String := Application_Name & ".adb";

            Trans            : constant Translate_Table :=
              (1 => Assoc ("Application_Name", Application_Name));
         begin

            --  Next see if application_name.adb exists if not create it.

            GNAT.Case_Util.To_Lower (App_File_Name);

            Project.Application_File :=
              Ada.Strings.Unbounded.To_Unbounded_String (App_File_Name);

            if not GNAT.OS_Lib.Is_Regular_File (App_File_Name) then
               Templates.Execute (App_File_Name, "application.adb", Trans);
            end if;
         end;
      end if;
   end Update_Application;

end GNAVI_ICG.Application;
