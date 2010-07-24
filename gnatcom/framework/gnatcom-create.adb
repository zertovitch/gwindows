------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                       G N A T C O M . C R E A T E                        --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  Base package for creation of COM objects

with GNATCOM.Utility;

package body GNATCOM.Create is

   procedure CoReleaseServerProcess;
   pragma Import (StdCall, CoReleaseServerProcess, "CoReleaseServerProcess");

   -- Can_Close --

   procedure Can_Close is
      use type Interfaces.C.long;
   begin
      if
        (InProcServer = False) and then
        (Server_Lock_Count = 0) and then
        (Component_Count = 0)
      then
         --  Once called any attempt to use this server will return
         --  CO_E_SERVER_STOPPING to prevent deadlocks
         CoReleaseServerProcess;

         GNATCOM.Utility.Post_Quit (Main_Thread_ID);
      end if;
   end Can_Close;

end GNATCOM.Create;
