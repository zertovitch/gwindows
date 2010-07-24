------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--       G N A T C O M . C R E A T E . R E M O T E _ R E G I S T E R        --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.IO; use GNAT.IO;

with GNATCOM.Register;

package body GNATCOM.Create.Remote_Register is

   procedure Display_Help;
   --  Displays instructions on using the Local Server

   function Retrieve_hInstance return Interfaces.C.long;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   -- Display_Help --

   procedure Display_Help is
   begin
      Put_Line ("This is a utility to register a COM object for remote use");
      Put_Line ("and to host the remote type library. To regiser use:");
      Put_Line ("utilityname ServerName");
      New_Line;
      Put_Line ("To unregister this server use:");
      Put_Line ("servername /UnregServer");
      New_Line;
   end Display_Help;

   -- Init_Object --

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID) is
   begin
      --  Check command line for ServerName or UnRegServer

      if Argument_Count /= 1 then
         Display_Help;
      else
         if
           (Argument (1) = "/UnregServer")
           or
           (Argument (1) = "-UnregServer")
         then
            GNATCOM.Register.Unregister_Type_Library (LIBID);

            for N in
              Factory_Map.all'First .. (Factory_Map.all'Last)
            loop
               GNATCOM.Register.Unregister_Server
                 (CLSID     => Factory_Map (N).CLSID,
                  Name      => To_String (Factory_Map (N).Name),
                  Version   => To_String (Factory_Map (N).Version));
            end loop;

            Put_Line ("Unregistered remote COM Object");
         else
            GNATCOM.Register.Register_Type_Library (Retrieve_hInstance);

            for N in
              Factory_Map.all'First .. (Factory_Map.all'Last)
            loop
               GNATCOM.Register.Register_Remote_Server
                 (CLSID          => Factory_Map (N).CLSID,
                  Name           => To_String (Factory_Map (N).Name),
                  Version        => To_String (Factory_Map (N).Name),
                  Description    => To_String (Factory_Map (N).Name),
                  Remote_Machine => Argument (1));
            end loop;
            Put_Line ("Registered Remote COM Object on " & Argument (1));
         end if;
      end if;
   end Init_Object;

end GNATCOM.Create.Remote_Register;
