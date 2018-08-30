------------------------------------------------------------------------------
--                                                                          --
--                  GBManager - The GNATCOM Binding Manager                 --
--                                                                          --
--                             G B M A N A G E R                            --
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

with Ada.Command_Line;

with GWindows.Registry;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.GStrings.IO; use GWindows.GStrings.IO;

with GBManager_Lib;
with GBManager_GUI;

with Bind_COM;

procedure GBManager is
   --
   --  NB: 32 bit and 64 bit binaries .coff are uncompatible!
   --
   --  pragma Linker_Options ("gbmanager.coff");

   procedure Display_Usage;
   --  Displays program usage information

   procedure List_Libraries;
   --  List available type libraries

   procedure Display_Usage is
   begin
      Put_Line ("GBManager");
      Put_Line ("(c) 2002 David Botton");
      Put_Line ("For more information see " &
                "http://www.adapower.com/gwindows");
      New_Line;
      Put_Line ("This is free software;  you can  redistribute it  " &
                "and/or modify it under");
      Put_Line ("terms of the  GNU General Public License as " &
                "published  by the Free Soft-");
      Put_Line ("ware  Foundation;  either version 2,  or (at your" &
                " option) any later ver-");
      Put_Line ("sion. It is distributed in the hope that it will be " &
                "useful, but WITHOUT");
      Put_Line ("ANY WARRANTY;  without even the  implied warranty of" &
                " MERCHANTABILITY or");
      Put_Line ("FITNESS FOR A PARTICULAR PURPOSE.    See the GNU " &
                "General Public License");
      Put_Line ("for  more details.  You should have  received" &
                "  a copy of the GNU General");
      Put_Line ("Public License  distributed with this;  see file" &
                " COPYING.  If not, write");
      Put_Line ("to  the Free Software Foundation,  59 Temple Place" &
                " - Suite 330,  Boston,");
      Put_Line ("MA 02111-1307, USA.");
      New_Line;
      New_Line;
      Put_Line
        ("Usage: GBManager BIND [LIBID] [MAJVER] [MINVER] [PACKAGENAME]");
      Put_Line ("Generate binding");
      New_Line;
      Put_Line ("Usage: GBManager LIST");
      Put_Line ("List all type libraries on system");
      New_Line;
      Put_Line ("Usage: GBManager SET [DIR]");
      Put_Line ("Set DIR to output bindings");
      New_Line;
      Put_Line ("Usage: GBManager");
      Put_Line ("Display GUI");
      New_Line;
   end Display_Usage;

   procedure List_Libraries is
      Key_List : GWindows.Registry.Key_Name_Array :=
        GBManager_Lib.List_Type_Libraries;
   begin
      for N in Key_List'First .. Key_List'Last loop
         declare
            Lib_IID  : constant GWindows.GString :=
              GWindows.GStrings.To_GString_From_Unbounded (Key_List (N));
            Versions : GWindows.Registry.Key_Name_Array :=
              GBManager_Lib.List_Type_Library_Versions (Lib_IID);
         begin
            Put_Line ("Lib_IID : " & Lib_IID);
            for NN in Versions'First .. Versions'Last loop
               declare
                  Version : constant GWindows.GString :=
                    GWindows.GStrings.To_GString_From_Unbounded
                      (Versions (NN));
               begin
                  Put_Line ("   Version  : " & Version);
                  Put_Line ("   Name     : " &
                              GBManager_Lib.Type_Library_Name (Lib_IID,
                                                               Version));
                  Put_Line ("   Location : " &
                              GBManager_Lib.Type_Library_Location (Lib_IID,
                                                                   Version));
               end;
            end loop;
         end;
      end loop;
   end List_Libraries;

begin
   case Ada.Command_Line.Argument_Count is
      when 0 =>
         GBManager_GUI.Start_GUI;

      when 1 =>
         declare
            COMMAND  : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (1));
         begin
            To_Upper (COMMAND);
            if COMMAND = "LIST" then
               List_Libraries;
            else
               Display_Usage;
            end if;
         end;

      when 2 =>
         declare
            COMMAND  : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (1));
         begin
            To_Upper (COMMAND);
            if COMMAND = "SET" then
               GBManager_Lib.Set_Binding_Location
                 (To_GString_From_String (Ada.Command_Line.Argument (2)));
            else
               Display_Usage;
            end if;
         end;

      when 5 =>
         declare
            COMMAND  : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (1));
         begin
            To_Upper (COMMAND);
            if COMMAND = "BIND" then
               GBManager_Lib.Change_To_Binding_Location;
               Bind_COM.Bind (Ada.Command_Line.Argument (2),
                              Ada.Command_Line.Argument (3),
                              Ada.Command_Line.Argument (4),
                              Ada.Command_Line.Argument (5));
            else
               Display_Usage;
            end if;
         end;

      when others =>
         Display_Usage;
   end case;
end GBManager;
