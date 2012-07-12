------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                     G N A V I _ I C G . W I N D O W                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.13 $
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

with Templates;
with Templates_Parser;

package body GNAVI_ICG.Window is
   use Ada.Strings.Unbounded;

   Indent : constant String := "   ";

   procedure Process_Control
     (O_String     : in out Ada.Strings.Unbounded.Unbounded_String;
      Parent_Name  : in     String;
      Window_Name  : in     String;
      Control_Node : in     DOM.Core.Element);
   --  Setup control in -on_create sep. procedure


   procedure Process_Control
     (O_String     : in out Ada.Strings.Unbounded.Unbounded_String;
      Parent_Name  : in     String;
      Window_Name  : in     String;
      Control_Node : in     DOM.Core.Element)
   is
      use DOM.Core;
      use Templates;

      Control_Name    : constant String := "Window." &
        Elements.Get_Attribute (Control_Node, "name");

      Control_Type    : constant String :=  Elements.Get_Attribute (Control_Node,
                                                           "type");

      Control_Package : constant String := With_Of (Control_Type);

      Window_Package  : constant String := Window_Name & "_Package";

      NI              : Node := Nodes.First_Child (Control_Node);

      Create_String   : constant String :=
        Indent & Control_Package & ".Create (" &
        Control_Name & "," & NL &
        Indent & Indent & "Parent => " & Parent_Name &
        Create_Params (Control_Node) & ");" & NL;

      Created         : Boolean := False;

      procedure Check_Create;
      --  Check to see if create has been written yet and if not do so.

      procedure Check_Create is
      begin
         if not Created then
            O_String := O_String & Create_String;
            Created := True;
         end if;
      end Check_Create;

   begin
      --  It is important that the Create happen after setting the handlers
      --  to insure that an On_Create style handler will be called at
      --  creation of the control

      while NI /= null loop
         if Nodes.Node_Name (NI) = "init" then

            Check_Create;

            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     if Elements.Get_Attribute (NI2, "value") /= "" then
                        O_String := O_String &
                          Indent & Control_Package & "." &
                          Nodes.Node_Name (NI2) & " (" &
                          Control_Name & ", " &
                          Elements.Get_Attribute (NI2, "value") & ");" & NL;
                     end if;
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;
         elsif Nodes.Node_Name (NI) = "controls" then
            Check_Create;

            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     Process_Control
                       (O_String, Control_Name, Window_Name, NI2);
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;

         elsif Nodes.Node_Name (NI) = "handlers" then
            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     O_String := O_String &
                       Indent &
                       Control_Package & "." &
                       Elements.Get_Attribute (NI2, "event") &
                       "_Handler (" & Control_Name & ", " &
                       Elements.Get_Attribute (NI2, "name") &
                       "'Access);" & NL;

                     Templates.Check_For_Handler
                       (Window_Package,
                        Elements.Get_Attribute (NI2, "name"),
                        Elements.Get_Attribute (NI2, "type"));
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;
         end if;

         NI := Nodes.Next_Sibling (NI);
      end loop;

      Check_Create;

   end Process_Control;

   procedure Update_Window (Window_Node : DOM.Core.Element)
   is
      use DOM.Core;
      use Templates;
      use Templates_Parser;



      O_String       : Ada.Strings.Unbounded.Unbounded_String;

      Window_Name    : constant String := Elements.Get_Attribute (Window_Node,
                                                      "name");
      Window_Type    : constant String :=  Elements.Get_Attribute (Window_Node,
                                                          "type");

      Window_Base    : constant String := Templates.With_Of (Window_Type);

      Window_Package : constant String := Window_Name & "_Package";

      Create_File    : String := Window_Name & "_package-on_create.adb";

      Trans          : constant Translate_Table :=
        (1 => Assoc ("Window_Name", Window_Name));

      NI : Node := Nodes.First_Child (Window_Node);

   begin
      GNAT.Case_Util.To_Lower (Create_File);
      O_String := O_String &
        Translate (Load_Template ("on_create.adb"), Trans);


      --  Set Window Handlers
      --  Process Init
      --  Set Control Handlers
      --  Process Controls

      while NI /= null loop
         if Nodes.Node_Name (NI) = "init" then
            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     O_String := O_String &
                       Indent & Nodes.Node_Name (NI2) & " (" &
                       "Window" & ", " &
                       Elements.Get_Attribute (NI2, "value") &
                       ");" & NL;
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;
         elsif Nodes.Node_Name (NI) = "controls" then
            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     Process_Control (O_String, "Window", Window_Name, NI2);
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;

         elsif Nodes.Node_Name (NI) = "handlers" then
            declare
               NI2 : Node := Nodes.First_Child (NI);
            begin
               while NI2 /= null loop
                  if NI2.Node_Type = Element_Node then
                     O_String := O_String &
                       Elements.Get_Attribute (NI2, "event") &
                       "_Handler (" & "Window" & ", " &
                       Elements.Get_Attribute (NI2, "name") &
                       "'Access);" & NL;

                     Templates.Check_For_Handler
                       (Window_Package,
                        Elements.Get_Attribute (NI2, "name"),
                        Elements.Get_Attribute (NI2, "type"));
                  end if;

                  NI2 := Nodes.Next_Sibling (NI2);
               end loop;
            end;
         end if;

         NI := Nodes.Next_Sibling (NI);
      end loop;


      O_String := O_String &
        Indent & Window_Base & ".On_Create (" &
        Window_Type & " (Window));" & NL;

      O_String := O_String & "end On_Create;" & NL;

      Update_File (Create_File, To_String (O_String));
   end Update_Window;

end GNAVI_ICG.Window;
