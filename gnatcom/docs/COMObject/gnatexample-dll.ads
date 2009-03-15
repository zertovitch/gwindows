with Win32.Windef;

package GNATExample.DLL is

   function DllGetClassObject
     (clsid : in     Win32.Objbase.REFCLSID;
      riid  : in     Win32.Objbase.REFIID;
      ppv   : access Win32.PVOID)
     return HRESULT;
   pragma Export (Stdcall, DllGetClassObject, "DllGetClassObject");
   --  Used to expose the Class Factory Object's IClassFactory interface to
   --  the SCM that can create objects of the CLSID riid. The SCM (a part of
   --  the OS) then delivers this interface through proxies were needed to
   --  the client application or to the Win32 API function CoCreateInstance

   function DllCanUnloadNow return HRESULT;
   pragma Export (Stdcall, DllCanUnloadNow, "DllCanUnloadNow");
   --  This is called by the OS to determine if the Dll can be unloaded.
   --  This returns true if there are no instance of the COM object in use
   --  and there are no locks placed on the server through
   --  IClassFactory::LockServer

   function DllRegisterServer return HRESULT;
   pragma Export (Stdcall, DllRegisterServer, "DllRegisterServer");
   --  Is called by RegSvr32 (or other application) to request the DLL to
   --  register its COM objects in the registry

   function DllUnregisterServer return HRESULT;
   pragma Export (Stdcall, DllUnregisterServer, "DllUnregisterServer");
   --  Is called by RegSvr32 (or other application) to request the DLL to
   --  unregister its COM objects from the registry.

   hInstance : Win32.Windef.HINSTANCE;
   --  Contains the intstance handle of the DLL once loaded.

end GNATExample.DLL;
