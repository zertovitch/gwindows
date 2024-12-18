------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                                 I C G                                    --
--                                                                          --
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAVI_ICG;

procedure ICG is
   use Ada.Text_IO;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line ("ICG - The GNAVI Interactive Code Generator");
      Put_Line ("(c) 1999 - 2024 David Botton");
      Put_Line ("For more information see http://www.gnavi.org");
      New_Line;
      Put_Line ("Usage: " & Ada.Command_Line.Command_Name &
                  " project_name.gnp");
      --
      --  In case icg.exe was launched from Explorer:
      Put ("Press return");
      New_Line;
      Skip_Line;
   else
      GNAVI_ICG.Generate_All (XML_File_Name => Ada.Command_Line.Argument (1));
   end if;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end ICG;
