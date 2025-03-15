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
                       Root_Key             : in Interfaces.C.ptrdiff_t :=
                         HKEY_CLASSES_ROOT);
   --  Place a Name / Value pair in the Windows NT Registry
   --  A blank name implies the default value for the key

   procedure Unregister (KeyName  : in String;
                         Root_Key : in Interfaces.C.ptrdiff_t :=
                           HKEY_CLASSES_ROOT);
   --  Removes a key and its Name / Value pairs from the Windows NT Registry
   --  All child keys of a KeyName must first be removed before Unregister
   --  will remove a KeyName from the registry.

   procedure Register_Type_Library (hInstance : in Interfaces.C.ptrdiff_t);
   --  Register the Type Library embedded as a resource in the application
   --  as:
   --         1 TypeLib "XXXXXX.tlb"

   procedure Register_Type_Library (Path  : in GNATCOM.Types.BSTR;
                                    Clear : in Boolean            := True);

   procedure Unregister_Type_Library (LIBID : in GNATCOM.Types.GUID);
   --  Unregister the Type Library with LIBID

   procedure Register_Inproc_Server (hInstance    : in Interfaces.C.ptrdiff_t;
                                     CLSID        : in GNATCOM.Types.GUID;
                                     Name         : in String;
                                     Version      : in String;
                                     Description  : in String;
                                     Thread_Model : in String := "Apartment";
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>));
   --  Register COM object contained in an Inproc (DLL) Server

   procedure Register_Local_Server (hInstance    : in Interfaces.C.ptrdiff_t;
                                    CLSID        : in GNATCOM.Types.GUID;
                                    Name         : in String;
                                    Version      : in String;
                                    Description  : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>);
      Service_Name : in String := "";
      APPID        : in GNATCOM.Types.GUID := GNATCOM.Types.GUID_NULL);
   --  Register COM object contained in a Local (EXE) Server

   procedure Register_Remote_Server (CLSID          : in GNATCOM.Types.GUID;
                                     Name           : in String;
                                     Version        : in String;
                                     Description    : in String;
                                     Remote_Machine : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>));
   --  Register type library and settings to access a COM object remotely

   procedure Unregister_Server (CLSID   : in GNATCOM.Types.GUID;
                                Name    : in String;
                                Version : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>));
   --  Remove COM object settings in registry

   procedure Register_Component_Category
     (Categories : access GNATCOM.Types.CATEGORYINFO_Array);
   --  Register Component Category.

   FILE_NAME_ERROR : exception;
   --  Unable to determine the file name of the server, perhaps the hInstance
   --  is invalid.

   IO_ERROR : exception;
   --  Unable to open, read, or write to the type library or the type library
   --  is in an invalid format

   REGISTRY_ERROR : exception;
   --  Unable to open system registry
end GNATCOM.Register;
