------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                     G N A T C O M . R E G I S T E R                      --
--                                                                          --
--                                S p e c                                   --
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

--  COM Registry helpers

with Interfaces.C;
with GNATCOM.Types;

package GNATCOM.Register is

   --  Root Registry Keys

   HKEY_CLASSES_ROOT     : constant := -2147483648;
   HKEY_CURRENT_USER     : constant := -2147483647;
   HKEY_LOCAL_MACHINE    : constant := -2147483646;
   HKEY_USERS            : constant := -2147483645;
   HKEY_PERFORMANCE_DATA : constant := -2147483644;
   HKEY_CURRENT_CONFIG   : constant := -2147483643;
   HKEY_DYN_DATA         : constant := -2147483642;

   procedure Register (KeyName, Name, Value : in String;
                       Root_Key             : in Interfaces.C.long :=
                         HKEY_CLASSES_ROOT);
   --  Place a Name / Value pair in the Windows NT Registry
   --  A blank name implies the default value for the key

   procedure Unregister (KeyName  : in String;
                         Root_Key : in Interfaces.C.long := HKEY_CLASSES_ROOT);
   --  Removes a key and its Name / Value pairs from the Windows NT Registry
   --  All child keys of a KeyName must first be removed before Unregister
   --  will remove a KeyName from the registry.

   procedure Register_Type_Library (hInstance : in Interfaces.C.long);
   --  Register the Type Library embedded as a resource in the application
   --  as:
   --         1 TypeLib "XXXXXX.tlb"

   procedure Register_Type_Library (Path  : in GNATCOM.Types.BSTR;
                                    Clear : in Boolean            := True);

   procedure Unregister_Type_Library (LIBID : in GNATCOM.Types.GUID);
   --  Unregister the Type Library with LIBID

   procedure Register_Inproc_Server (hInstance    : in Interfaces.C.long;
                                     CLSID        : in GNATCOM.Types.GUID;
                                     Name         : in String;
                                     Version      : in String;
                                     Description  : in String;
                                     Thread_Model : in String := "Apartment");
   --  Register COM object contained in an Inproc (DLL) Server

   procedure Register_Local_Server (hInstance    : in Interfaces.C.long;
                                    CLSID        : in GNATCOM.Types.GUID;
                                    Name         : in String;
                                    Version      : in String;
                                    Description  : in String);
   --  Register COM object contained in a Local (EXE) Server

   procedure Register_Remote_Server (CLSID          : in GNATCOM.Types.GUID;
                                     Name           : in String;
                                     Version        : in String;
                                     Description    : in String;
                                     Remote_Machine : in String);
   --  Register type library and settings to access a COM object remotely

   procedure Unregister_Server (CLSID   : in GNATCOM.Types.GUID;
                                Name    : in String;
                                Version : in String);
   --  Remove COM object settings in registry

   FILE_NAME_ERROR : exception;
   --  Unable to determine the file name of the server, perhaps the hInstance
   --  is invalid.

   IO_ERROR : exception;
   --  Unable to open, read, or write to the type library or the type library
   --  is in an invalid format

   REGISTRY_ERROR : exception;
   --  Unable to open system registry
end GNATCOM.Register;
