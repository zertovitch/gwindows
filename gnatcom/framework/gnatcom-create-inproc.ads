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
