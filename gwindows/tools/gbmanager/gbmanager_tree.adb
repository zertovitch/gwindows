------------------------------------------------------------------------------
--                                                                          --
--                 GBManager - Win32 COM Binding Manager                    --
--                                                                          --
--                     G B M A N A G E R _ T R E E                          --
--                                                                          --
--                                 B o d y                                  --
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

with GWindows.Message_Boxes; use GWindows.Message_Boxes;

with GWindows.Base;
with GWindows.Windows;
with GWindows.Static_Controls;
with GWindows.GStrings;
with GWindows.GStrings.IO;
with GWindows.Registry;
with GWindows.Application;
with GWindows.Cursors;

with GBManager_Lib;
with GWindows.List_Boxes;

--  GNATCOM tool components
with Bind_COM;
with COM_Scope;
with Source_Buffer;

package body GBManager_Tree is

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out GBManager_Tree_Control_Type) is
      use GWindows.Common_Controls;

      Key_List : GWindows.Registry.Key_Name_Array :=
        GBManager_Lib.List_Type_Libraries;
      Root_Node : Tree_Item_Node;
   begin
      Insert_Item (Window, "Type Libraries", 0, Root_Node, As_A_Root);
      Focus (Window);

      for N in Key_List'First .. Key_List'Last loop
         declare
            Lib_ID  : constant GWindows.GString :=
              GWindows.GStrings.To_GString_From_Unbounded (Key_List (N));
            Versions : GWindows.Registry.Key_Name_Array :=
              GBManager_Lib.List_Type_Library_Versions (Lib_ID);
            Node     : Tree_Item_Node;
            LNode    : Tree_Item_Node;
            LINode   : Tree_Item_Node;
         begin
            Insert_Item (Window,
                         GBManager_Lib.Type_Library_Name
                           (Lib_ID,
                            GWindows.GStrings.To_GString_From_Unbounded
                              (Versions (Versions'Last))),
                         Root_Node, Node, Sort);
            Insert_Item (Window, "LIBID", Node, LNode, Last);
            Insert_Item (Window, Lib_ID, LNode, LINode, Last);

            for NN in Versions'First .. Versions'Last loop
               declare
                  Version : constant GWindows.GString :=
                    GWindows.GStrings.To_GString_From_Unbounded
                      (Versions (NN));
                  VNode   : Tree_Item_Node;
                  ANode   : Tree_Item_Node;
               begin
                  Insert_Item (Window, Version, Node, VNode, Last);
                  Insert_Item (Window,
                               "Name: " &
                                 GBManager_Lib.Type_Library_Name (Lib_ID,
                                                                  Version),
                               VNode, ANode, Last);
                  Insert_Item (Window,
                               "Location: " &
                                 GBManager_Lib.Type_Library_Location (Lib_ID,
                                                                      Version),
                              VNode, ANode, Last);
               end;
            end loop;
         exception
            when others =>
               GWindows.GStrings.IO.Put_Line
                 ("Error inserting LIBID :" & Lib_ID);
         end;

         Expand (Window, Root_Node);
      end loop;
   end On_Create;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (Control : in out GBManager_Tree_Control_Type)
   is
   begin
      Do_Binding (Control);
   end On_Double_Click;

   ---------------
   -- On_Return --
   ---------------

   procedure On_Return (Control : in out GBManager_Tree_Control_Type)
   is
   begin
      Do_Binding (Control);
   end On_Return;

   ----------------
   -- Do_Binding --
   ----------------

   procedure Do_Binding (Control : in out GBManager_Tree_Control_Type)
   is
      use GWindows.Common_Controls;

      Current_Node : Tree_Item_Node := Selected_Item (Control);
      Node_Text    : constant GWindows.GString := Text (Control, Current_Node);
   begin
      if Node_Text'Length > 10 then
         Current_Node := Get_Parent_Item (Control, Current_Node);
      end if;

      if
        Current_Node /= 0
        and
        Current_Node /= Get_Root_Item (Control)
        and
        Text (Control, Current_Node) /= "LIBID"
      then
         declare
            use GWindows.GStrings;

            Version : constant String :=
              To_String (Text (Control, Current_Node));
            LNode   : Tree_Item_Node;

            Prefix  : GWindows.GString_Unbounded;
            Success : Boolean;
         begin
            LNode := Get_Parent_Item (Control, Current_Node);
            LNode := Get_First_Child_Item (Control, LNode);
            LNode := Get_First_Child_Item (Control, LNode);

            Input_Box (Controlling_Parent (Control).all,
                       "Bind Library",
                       "Please enter base package name to use",
                       Prefix,
                       Success,
                       Width => 300,
                       Height => 150);

            if Success then
               declare
                  use GWindows.Windows;
                  use GWindows.Static_Controls;
                  use GWindows.Cursors;

                  Wait_Win : Window_Type;

                  task Wait_Task is
                     entry Start;
                  end Wait_Task;

                  task body Wait_Task is
                     Label : Label_Type;
                  begin
                     accept Start;

                     Create_As_Dialog (Wait_Win, "Wait...",
                                       Width => 200,
                                       Height => 75);
                     Center (Wait_Win, Controlling_Parent (Control).all);

                     Create (Label, Wait_Win, "Binding in progress",
                             0, 0, 180, 30, Alignment => Center);
                     Center (Label);

                     Disable (Wait_Win);

                     GWindows.Application.Show_Modal (Wait_Win);
                  end Wait_Task;

               begin
                  Set_Cursor (Load_System_Cursor (IDC_WAIT));
                  Wait_Task.Start;

                  GBManager_Lib.Change_To_Binding_Location;
                  Bind_COM.Bind (To_String (Text (Control, LNode)),
                                 Version (1 .. 1),
                                 Version (3 .. 3),
                                 To_String
                                   (To_GString_From_Unbounded (Prefix)));

                  Close (Wait_Win);
                  Set_Cursor (Load_System_Cursor (IDC_ARROW));
               end;
            end if;
         end;
      end if;
   end Do_Binding;

   -------------
   -- Do_Look --
   -------------

   procedure Do_Look (Control : in out GBManager_Tree_Control_Type)
   is
      use GWindows.Common_Controls;

      Current_Node : Tree_Item_Node := Selected_Item (Control);
      Node_Text    : constant GWindows.GString := Text (Control, Current_Node);
   begin
      if Node_Text'Length > 10 then
         Current_Node := Get_Parent_Item (Control, Current_Node);
      end if;

      if
        Current_Node /= 0
        and
        Current_Node /= Get_Root_Item (Control)
        and
        Text (Control, Current_Node) /= "LIBID"
      then
         declare
            use GWindows.GStrings;

            LNode   : Tree_Item_Node;
         begin
            LNode := Get_First_Child_Item (Control, Current_Node);
            LNode := Get_Next_Item (Control, LNode);

            declare
               use GWindows.Windows;
               use GWindows.List_Boxes;
               use GWindows.Cursors;

               procedure Write_Line (S : String);
               --  Output lines to text box

               File_Text : constant GWindows.GString := Text (Control, LNode);
               File_Name : constant String :=
                 To_String
                 (File_Text (File_Text'First + 10 .. File_Text'Last));
               Buffer    : aliased Source_Buffer.Source_Buffer_Type;
               LWin      : constant Window_Access := new Window_Type;
               LBox      : constant List_Box_Access := new List_Box_Type;

               procedure Write_Line (S : String)
               is
               begin
                  GWindows.List_Boxes.Add
                    (LBox.all, To_GString_From_String (S));
               end Write_Line;

               procedure Dump_Buffer is
                  new Source_Buffer.Write_Buffer (Write_Line);

            begin
               GWindows.Windows.Create
                 (LWin.all, To_GString_From_String (File_Name),
                  Is_Dynamic => True);
               GWindows.List_Boxes.Create
                 (LBox.all, LWin.all, 1, 1, 1, 1,
                  Sort => False,
                  Is_Dynamic => True);

               Vertical_Scroll_Bar (LBox.all, True);

               Dock (LBox.all, GWindows.Base.Fill);
               Dock_Children (LWin.all);
               Visible (LWin.all);

               Set_Cursor (Load_System_Cursor (IDC_WAIT));
               COM_Scope.Write (File_Name, Buffer);
               Dump_Buffer (Buffer);

               Set_Cursor (Load_System_Cursor (IDC_ARROW));
            end;
         end;
      end if;
   end Do_Look;

end GBManager_Tree;
