------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                       GNATCOM BINDING COMPONENTS                         --
--                                                                          --
--               G N A T C O M . C R E A T E . I N P R O C                  --
--                                                                          --
--                                 S p e c                                  --
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

--  Provides implementation of the Inproc Server (the DLL) that will host
--  the COM objects

with Ada.Strings.Unbounded;

with GNATCOM.Types;
with GNATCOM.Create.Factory;

package GNATCOM.Create.Inproc is

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID);
   --  Initialize Object containter parameters

   function DllGetClassObject
     (clsid : in     GNATCOM.Types.Pointer_To_GUID;
      riid  : in     GNATCOM.Types.Pointer_To_GUID;
      ppv   : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Export (Stdcall, DllGetClassObject, "DllGetClassObject");
   --  Used to expose the Class Factory Object's IClassFactory interface to
   --  the SCM that can create objects of the CLSID riid. The SCM (a part of
   --  the OS) then delivers this interface through proxies were needed to
   --  the client application or to the Win32 API function CoCreateInstance

   function DllCanUnloadNow return GNATCOM.Types.HRESULT;
   pragma Export (Stdcall, DllCanUnloadNow, "DllCanUnloadNow");
   --  This is called by the OS to determine if the Dll can be unloaded.
   --  This returns true if there are no instance of the COM object in use
   --  and there are no locks placed on the server through
   --  IClassFactory::LockServer

   function DllRegisterServer return GNATCOM.Types.HRESULT;
   pragma Export (Stdcall, DllRegisterServer, "DllRegisterServer");
   --  Is called by RegSvr32 (or other application) to request the DLL to
   --  register its COM objects in the registry

   function DllUnregisterServer return GNATCOM.Types.HRESULT;
   pragma Export (Stdcall, DllUnregisterServer, "DllUnregisterServer");
   --  Is called by RegSvr32 (or other application) to request the DLL to
   --  unregister its COM objects from the registry.

   type Factory_Record is
      record
         CLSID       : aliased GNATCOM.Types.GUID;
         Create      : GNATCOM.Create.Factory.Creation_Function := null;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Version     : Ada.Strings.Unbounded.Unbounded_String;
         Description : Ada.Strings.Unbounded.Unbounded_String;
         Implemented_Categories : GNATCOM.Types.GUID_Array_Pointer := null;
      end record;
   --  Map for CLSIDs to COM object creation functions

   type Factory_Record_Array is array (Natural range <>) of Factory_Record;
   type Factory_Record_Array_Pointer is access all Factory_Record_Array;
   --  The Factory_Record_Array is an array of Factory_Records
   --  A factory when constructed is passed the Create function for
   --  the COM object of CLSID.

   Factory_Map : Factory_Record_Array_Pointer;
   --  Pointer to map of COM objects in container

end GNATCOM.Create.Inproc;
