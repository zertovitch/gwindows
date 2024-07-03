------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                           G N A V I _ I C G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.5 $
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

with Ada.Text_IO;

with DOM.Core.Documents;
with DOM.Core.Nodes;

with GNAVI_ICG.Application;
with GNAVI_ICG.Windows;

with GNAT.Case_Util;
with GNAT.Directory_Operations;

with GNAVI_Templates;

package body GNAVI_ICG is

   procedure Load_Project (Project       : in out GNAVI_Project_Type;
                           XML_File_Name :        String)
   is
      use DOM.Core;
   begin
      Ada.Text_IO.Put_Line ("Processing Application : " & XML_File_Name);

      Input_Sources.File.Open (XML_File_Name, Project.XML_File);
      DOM.Readers.Parse (Project.Project_Tree, Project.XML_File);
      Project.Project_Document := DOM.Readers.Get_Tree (Project.Project_Tree);
      Project.Project_Root := Documents.Get_Element (Project.Project_Document);
   end Load_Project;

   procedure Close_Project (Project : in out GNAVI_Project_Type)
   is
   begin
      Ada.Text_IO.Put_Line ("Processing completed.");
      DOM.Readers.Free (Project.Project_Tree);
      Input_Sources.File.Close (Project.XML_File);
   end Close_Project;

   procedure Update_Project (Project : in out GNAVI_Project_Type)
   is
   begin
      GNAVI_ICG.Application.Update_Application (Project);
      GNAVI_ICG.Windows.Update_Windows (Project);
   end Update_Project;

   procedure Generate_All
      (XML_File_Name       : String;
       Templates_Directory : String := "")
   is
      Prj : GNAVI_Project_Type;
   begin
      if Templates_Directory /= "" then
         GNAVI_Templates.Template_Dir (Templates_Directory);
      end if;

      GNAT.Directory_Operations.Change_Dir
        (GNAT.Directory_Operations.Dir_Name (XML_File_Name));

      GNAVI_ICG.Load_Project
        (Prj,
         GNAT.Directory_Operations.Base_Name (XML_File_Name));
      GNAVI_ICG.Update_Project (Prj);
      GNAVI_ICG.Close_Project (Prj);

   end Generate_All;


   function Create_Params (Object_Node : DOM.Core.Element) return String is
      use DOM.Core;

      Indent : constant String := "      ";

      Attrs     : constant DOM.Core.Named_Node_Map := Nodes.Attributes (Object_Node);

      Param_Num : Natural := 0;

      function GWindows_Casing (S : String) return String is
      (if S = "id" then "ID" elsif S = "progid" then "ProgID" else GNAT.Case_Util.To_Mixed (S));

      function Do_Params (S : String) return String;
      --  Parse out parameters

      function Do_Params (S : String) return String is
         N :          String := Nodes.Node_Name (Nodes.Item (Attrs, Param_Num));
         V : constant String := Nodes.Node_Value (Nodes.Item (Attrs, Param_Num));
      begin
         GNAT.Case_Util.To_Lower (N);

         Param_Num := Param_Num + 1;

         if  N = "type" or N = "name" then
            if Param_Num = Nodes.Length (Attrs) then
               return S;
            else
               return Do_Params (S);
            end if;
         end if;

         declare
            NS : constant String := S & "," & GNAVI_Templates.NL &
              Indent & GWindows_Casing (N) & " => " & V;
         begin
            if Param_Num = Nodes.Length (Attrs) then
               return NS;
            else
               return Do_Params (NS);
            end if;
         end;

      end Do_Params;

   begin
      if Nodes.Length (Attrs) > 2 then
         return Do_Params ("");
      else
         return "";
      end if;
   end Create_Params;

end GNAVI_ICG;
