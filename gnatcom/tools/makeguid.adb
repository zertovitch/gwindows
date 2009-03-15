------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                             M A K E G U I D                              --
--                                                                          --
--                            $Revision: 1.2 $
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

--  Generates GUIDs

with Ada.Command_Line;

with GNATCOM.Initialize;
with GNATCOM.GUID;
use GNATCOM;

with GNAT.IO;
use GNAT;

procedure Makeguid is
   Count : Natural := 1;
begin
   Initialize.Initialize_COM;

   if Ada.Command_Line.Argument_Count = 1 then
      begin
         Count := Natural'Value (Ada.Command_Line.Argument (1));
      exception
         when others =>
            IO.Put_Line ("Usage: MakeGUID [Number_of_GUIDs]");
      end;
   end if;

   for N in 1 .. Count loop
      IO.Put_Line (GUID.To_String (GUID.Create_GUID));
   end loop;

   Initialize.Uninitialize_COM;
end Makeguid;
