------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                   G N A V I _ I C G . W I N D O W S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.7 $
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

with Ada.Exceptions;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with Sax.Readers;

with GNAVI_ICG.Window;

with Templates;
with Templates_Parser;

with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.IO;

package body GNAVI_ICG.Windows is

   function Create_Control_Block (File_Spec     : String;
                                  Controls_List : DOM.Core.Node_List)
                                 return String;
   --  Create the list of controls to be inserted in to package spec


   function Create_Control_Block (File_Spec     : String;
                                  Controls_List : DOM.Core.Node_List)
                                 return String
   is
      use DOM.Core;
      use Templates;

      Indent        : constant String := "         ";
      Control_Num   : Natural := 0;
      Controls_Node : Element;
      Control_List  : Node_List;

      function Create_Block (S : String) return String;
      --  Discover controls

      function Create_Block (S : String) return String is
         Control_Node    : constant Element := Nodes.Item (Control_List, Control_Num);

         Control_Name    : constant String := Elements.Get_Attribute (Control_Node,
                                                          "name");
         Control_Type    : constant String := Elements.Get_Attribute (Control_Node,
                                                          "type");
         Control_Package : constant String := With_Of (Control_Type);
      begin
         Templates.Check_For_With (File_Spec, Control_Package);

         Control_Num := Control_Num + 1;

         declare
            Line : constant String := Indent & Control_Name & " : aliased " &
              Control_Type & ";" & NL;
         begin
            if Control_Num = Nodes.Length (Control_List) then
               return S & Line;
            else
               return Create_Block (S & Line);
            end if;
         end;

      end Create_Block;

   begin
      if Nodes.Length (Controls_List) > 0 then
         Controls_Node := Nodes.Item (Controls_List, 0);
         Control_List := Elements.Get_Elements_By_Tag_Name (Controls_Node,
                                                            "control");

         if Nodes.Length (Control_List) > 0 then
            return Create_Block ("");
         end if;
      end if;

      return Indent & "null;" & NL; --  BUG: If there is user data and no
                                    --       controls.
   end Create_Control_Block;

   procedure Update_Windows (Project : in out GNAVI_Project_Type)
   is
      use DOM.Core;

      NL : constant Node_List :=
        Elements.Get_Elements_By_Tag_Name (Project.Project_Root,
                                           "windows");
   begin
      if Nodes.Length (NL) > 0 then
         declare
            Windows_Node : constant DOM.Core.Element := Nodes.Item (NL, 0);
            NL : constant Node_List :=
              Elements.Get_Elements_By_Tag_Name (Windows_Node,
                                                 "window");
         begin
            for N in 0 .. Nodes.Length (NL) - 1 loop
               declare
                  Window_Node : constant DOM.Core.Element := Nodes.Item (NL, N);
                  File_Name   : constant String := Elements.Get_Attribute (Window_Node,
                                                                  "file");
                  Window_File : Input_Sources.File.File_Input;
                  Window_Tree : DOM.Readers.Tree_Reader;
                  Window_Doc  : DOM.Core.Document;
                  Window_Root : DOM.Core.Element;
               begin
                  Input_Sources.File.Open (File_Name, Window_File);
                  DOM.Readers.Parse (Window_Tree, Window_File);
                  Window_Doc := DOM.Readers.Get_Tree (Window_Tree);
                  Window_Root := Documents.Get_Element (Window_Doc);

                  declare
                     NL : constant Node_List :=
                       Elements.Get_Elements_By_Tag_Name (Window_Root,
                                                          "window");
                  begin
                     if Nodes.Length (NL) > 0 then
                        declare
                           use Templates_Parser;

                           Window_Node    : constant DOM.Core.Element :=
                             Nodes.Item (NL, 0);

                           Window_Name    : constant String :=
                             Elements.Get_Attribute (Window_Node, "name");

                           Window_Package : constant String :=
                             Window_Name & "_Package";

                           Window_Spec    : String :=
                             Window_Package & ".ads";

                           Window_Body    : String :=
                             Window_Package & ".adb";

                           Window_Type    : constant String :=
                                     Elements.Get_Attribute (Window_Node,
                                                             "type");

                           Window_Base    : constant String :=
                             Templates.With_Of (Window_Type);

                           Controls_Node_List  : constant Node_List :=
                             Elements.Get_Elements_By_Tag_Name (Window_Node,
                                                                "controls");

                           Trans          : constant Translate_Table :=
                             (Assoc ("Window_Name", Window_Name),
                              Assoc ("Window_Type", Window_Type),
                              Assoc ("Window_Type_Package", Window_Base),
                              Assoc ("Create_Params",
                                     Create_Params (Window_Node)));

                           New_File       : Boolean := False;
                        begin
                           --  Generate base window package if it does
                           --  not exist

                           GNAT.Case_Util.To_Lower (Window_Spec);
                           GNAT.Case_Util.To_Lower (Window_Body);

                           if
                             not GNAT.OS_Lib.Is_Regular_File (Window_Spec)
                           then
                              New_File := True;

                              Templates.Execute (Window_Spec,
                                                 "window_package.ads", Trans);
                           else
                              --  If file was already created should double
                              --  check that the withs for base types were not
                              --  deleted by user.
                              --
                              --  BUG: If with is not followed by exactly one
                              --       space it is not detected with
                              --       Templates.Check_For_With
                              --
                              --  BUG: Will miss fixing problem if there
                              --       is a with that starts with Base_Package
                              --       or GWindows.Base and is not a child
                              --       package of theirs.

                              Templates.Check_For_With
                                (Window_Spec, "GWindows.Base");

                              Templates.Check_For_With
                                (Window_Spec, Window_Base);

                           end if;


                           if
                             not GNAT.OS_Lib.Is_Regular_File (Window_Body)
                           then
                              Templates.Execute (Window_Body,
                                                 "window_package.adb", Trans);
                           end if;

                           if New_File or GNAVI_Gen_App_Withs then
                              --  Add 'with' for window to application.adb
                              --  if not there when a new window_package is
                              --  is created, i.e. its specs and/or the setting
                              --  GNAVI_Gen_App_Withs is on

                              Templates.Check_For_With
                                (Ada.Strings.Unbounded.To_String
                                   (Project.Application_File),
                                 Window_Name & "_Package");
                           end if;

                           --  Process Controls

                           Templates.Set_Control_Block
                             (Window_Spec,
                              Create_Control_Block (Window_Spec,
                                                    Controls_Node_List));

                           --  Process Window Handlers
                           GNAVI_ICG.Window.Update_Window (Window_Node);
                        end;

                     else
                        raise GNAVI_ICG_MISSING_WINDOW_TAG;
                     end if;
                  end;

                  DOM.Readers.Free (Window_Tree);
                  Input_Sources.File.Close (Window_File);

               exception
                  when E : Sax.Readers.XML_Fatal_Error =>
                     GNAT.IO.Put_Line ("Error in XML : " & File_Name);
                     GNAT.IO.Put_Line (Ada.Exceptions.Exception_Name (E));
                     GNAT.IO.Put_Line (Ada.Exceptions.Exception_Message (E));
               end;
            end loop;
         end;
      end if;
   end Update_Windows;

end GNAVI_ICG.Windows;
