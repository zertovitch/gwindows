------------------------------------------------------------------------------
--                                                                          --
--       GNAVI - The GNU Ada Visual Interface - Open Source Visual RAD      --
--                                                                          --
--                                 I C G                                    --
--                                                                          --
--                            $Revision: 1.3 $
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

with GNAT.Directory_Operations;

with GNAVI_ICG;
with Templates;

with GNAT.OS_Lib;

procedure ICG is
   use Ada.Text_IO;
   use type GNAT.OS_Lib.String_Access;

   Prj      : GNAVI_ICG.GNAVI_Project_Type;
   ICG_Path : GNAT.OS_Lib.String_Access := null;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
         Put_Line ("ICG - The GNAVI Interactive Code Generator");
         Put_Line ("(c) 1999 - 2004 David Botton");
         Put_Line ("For more information see http://www.gnavi.org");
         New_Line;
         Put_Line ("Usage: " & Ada.Command_Line.Command_Name &
                     " Project_Name.gnp");
   else
      ICG_Path :=
        GNAT.OS_Lib.Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
      if ICG_Path = null then
         ICG_Path := new String'(Ada.Command_Line.Command_Name);
      end if;

      Templates.Template_Dir
        (GNAT.Directory_Operations.Dir_Name (ICG_Path.all) &
         "templates" & GNAT.Directory_Operations.Dir_Separator);

      GNAT.Directory_Operations.Change_Dir
        (GNAT.Directory_Operations.Dir_Name (Ada.Command_Line.Argument (1)));

      GNAVI_ICG.Load_Project
        (Prj,
         GNAT.Directory_Operations.Base_Name (Ada.Command_Line.Argument (1)));
      GNAVI_ICG.Update_Project (Prj);
      GNAVI_ICG.Close_Project (Prj);
   end if;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end ICG;
