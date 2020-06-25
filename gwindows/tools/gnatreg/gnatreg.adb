--  ! NOTE: Since 2009 or so the registry is NOT used anymore by GNAT.
--  !
--  ! Only environment variables such as PATH (for locating
--  ! active GNAT version) or ADA_PROJECT_PATH are used now.
--  !
--  ! GNATREG would be only useful in the case you are using
--  ! a very old GNAT version, like 3.15p.

------------------------------------------------------------------------------
--                                                                          --
--          GNATREG - Win32 GNAT Standard Library Registry Tool             --
--                                                                          --
--                              G N A T R E G                               --
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

with GNATREG_Lib;
with GNATREG_GUI;

procedure GNATREG is

   procedure Display_Usage;
   --  Displays program usage information

   procedure Display_Usage is
   begin
      Put_Line ("GNATREG");
      Put_Line ("(c) 1999, 2000 David Botton");
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
      Put_Line ("Usage: GNATREG [COMMAND] [LIB_NAME] [DIR]");
      New_Line;
      Put_Line ("If there is no COMMAND, LIB_NAME, or DIR a GUI front" &
                " end is displayed.");
      New_Line;
      Put_Line ("COMMAND  = SET, DELETE or LIST");
      Put_Line ("           SET    : Set [DIR] as path for [LIB_NAME]");
      Put_Line ("           DELETE : Delete [LIB_NAME] from standard" &
                " libraries");
      Put_Line ("           LIST   : List standard libraries and" &
                " their paths");
      Put_Line ("LIB_NAME = Name of standard library");
      Put_Line ("DIR      = Path of standard library, if no DIR" &
                " is specified, the current path");
      Put_Line ("           is used");
   end Display_Usage;

begin
   case Ada.Command_Line.Argument_Count is
      when 0 =>
         GNATREG_GUI.Start_GUI;

      when 1 =>
         declare
            COMMAND  : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (1));
         begin
            To_Upper (COMMAND);
            if COMMAND /= "LIST" then
               Display_Usage;
            else
               declare
                  List : GNATREG_Lib.Library_Array :=
                    GNATREG_Lib.Get_Libraries;
               begin
                  for N in List'Range loop
                     Put_Line (To_GString_From_Unbounded (List (N)) &
                               " => " &
                               GNATREG_Lib.Get_Path
                               (To_GString_From_Unbounded (List (N))));
                  end loop;
               end;
            end if;
         end;

      when 2 =>
         declare
            COMMAND  : GWindows.GString :=
              To_GString_From_String (Ada.Command_Line.Argument (1));
            LIB_NAME : GWindows.GString :=
              To_GString_From_String (Ada.Command_Line.Argument (2));
            DIR      : constant GWindows.GString :=
              GWindows.Registry.Current_Directory;
         begin
            To_Upper (COMMAND);
            To_Upper (LIB_NAME);
            if COMMAND /= "SET" and then COMMAND /= "DELETE" then
               Display_Usage;
            else
               if COMMAND = "SET" then
                  GNATREG_Lib.Set_Library (LIB_NAME,
                                           DIR);
                  Put_Line ("Registered " &
                            LIB_NAME &
                            " at location : " &
                            DIR);
               else
                  GNATREG_Lib.Delete_Library (LIB_NAME);
                  Put_Line ("Deleted " & LIB_NAME);
               end if;
            end if;
         end;

      when 3 =>
         declare
            COMMAND  : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (1));
            LIB_NAME : GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (2));
            DIR      : constant GWindows.GString := To_GString_From_String
              (Ada.Command_Line.Argument (3));
         begin
            To_Upper (COMMAND);
            To_Upper (LIB_NAME);
            if COMMAND /= "SET" then
               Display_Usage;
            else
               GNATREG_Lib.Set_Library (LIB_NAME, DIR);
               Put_Line ("Registered " & LIB_NAME &
                         " at location : " & DIR);
            end if;
         end;

      when others =>
         Display_Usage;
   end case;
end GNATREG;
