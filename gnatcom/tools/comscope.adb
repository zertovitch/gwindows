------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                             C O M S C O P E                              --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  Generate human readable type information from a type library

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.IO; use GNAT.IO;

with COM_Scope;

procedure COMScope is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
         Put_Line ("COMScope");
         Put_Line ("(c) 1999-2004 David Botton");
         Put_Line ("For more information see http://www.gnavi.org/gnatcom");
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
         Put_Line ("Usage: COMScope typelib");
         New_Line;
         Put_Line ("typelib can be a type library file (.tlb) or any file" &
                   " with an embedded type");
         Put_Line ("library such as a COM object (.dll, .ocx, .exe)");
   else
      COM_Scope.Display (Ada.Command_Line.Argument (1));
   end if;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end COMScope;
