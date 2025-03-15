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
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
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

   function Retrieve_hInstance return Interfaces.C.ptrdiff_t;
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
