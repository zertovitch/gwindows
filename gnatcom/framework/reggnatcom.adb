------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                          R E G G N A T C O M                             --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  This utility will find the location of itself which should be in the
--  directory of the GNATCOM sources and register the GNATCOM sources
--  as a standard GNAT library in the Win32 Registry

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;
with Interfaces.C;

with GNATCOM.Register; use GNATCOM.Register;
with GNATCOM.Types;

with GNAT.IO; use GNAT.IO;

procedure RegGNATCOM is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");
   use type Interfaces.C.int;

   FILE_NAME_ERROR : exception;

   function GetModuleFileName
     (hInst        : in     Interfaces.C.long;
      lpszFileName : access Interfaces.C.char;
      cbFileName   : in     Interfaces.C.int)
     return Interfaces.C.int;
   pragma Import (StdCall, GetModuleFileName, "GetModuleFileNameA");

   procedure GetShortPathName
     (lpszLongPath  : Interfaces.C.char_array;
      lpszShortPath : Interfaces.C.char_array;
      cchBuffer     : GNATCOM.Types.DWORD);
   pragma Import (StdCall, GetShortPathName, "GetShortPathNameA");

   function Retrieve_hInstance return Interfaces.C.long;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   MAX_PATH   : constant := 1024;
   ServerPath : aliased Interfaces.C.char_array (1 .. MAX_PATH);
   KeyName    : constant String :=
     "SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries";
   Name       : constant String := "GNATCOM";

begin
   if
     GetModuleFileName (Retrieve_hInstance,
                        ServerPath (ServerPath'First)'Access,
                        MAX_PATH) < 0
   then
      raise FILE_NAME_ERROR;
   end if;

   GetShortPathName (ServerPath, ServerPath, MAX_PATH);

   declare
      Tmp   : constant String := Interfaces.C.To_Ada (ServerPath);
      Value : constant String := Tmp (1 .. Index (Tmp, "\", Backward) - 1);
   begin
      Register (KeyName, Name, Value, HKEY_LOCAL_MACHINE);
      Put_Line ("Registered GNATCOM at location : " & Value);
   end;

end RegGNATCOM;
