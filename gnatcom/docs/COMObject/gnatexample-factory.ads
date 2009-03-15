package GNATExample.Factory is
   --  Access function types for IClassFactory

   type Af_IClassFactory_CreateInstance is access
     function (This        : access Interface;
               pUnkOuter   : in     Win32.Objbase.LPUNKNOWN;
               riid        : in     Win32.Objbase.REFIID;
               ppvObject   : access Win32.PVOID)
     return HRESULT;
   pragma Convention (Stdcall, Af_IClassFactory_CreateInstance);
   --  Create an instance of the COM object returning a pointer to an interface
   --  with the IID riid.

   type Af_IClassFactory_LockServer is access
     function (This  : access interface;
               fLock : in     Win32.BOOL)
     return HRESULT;
   pragma Convention (Stdcall, Af_IClassFactory_LockServer);
   --  if fLock is true add a lock to prevent the host server from unloading
   --  from memory.

   --  Functions for IUnknown

   function IUnknown_QueryInterface
     (This      : access Interface;
      riid      : in     Win32.Objbase.REFIID;
      ppvObject : access Win32.PVOID)
     return HRESULT;
   pragma Convention (Stdcall, IUnknown_QueryInterface);

   function IUnknown_AddRef (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, IUnknown_AddRef);

   function IUnknown_Release (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, IUnknown_Release);

   --  Functions for IClassFactory

   function IClassFactory_CreateInstance
     (This        : access Interface;
      pUnkOuter   : in     Win32.Objbase.LPUNKNOWN;
      riid        : in     Win32.Objbase.REFIID;
      ppvObject   : access Win32.PVOID)
     return HRESULT;
   pragma Convention (Stdcall, IClassFactory_CreateInstance);

   function IClassFactory_LockServer
     (This  : access Interface;
      fLock : in     Win32.BOOL)
     return HRESULT;
   pragma Convention (Stdcall, IClassFactory_LockServer);

   type IClassFactory_Vtbl_Record is
      record
         --  IUnknown
         QueryInterface   : Af_IUnknown_QueryInterface
           := IUnknown_QueryInterface'Access;
         AddRef           : Af_IUnknown_AddRef
           := IUnknown_AddRef'Access;
         Release          : Af_IUnknown_Release
           := IUnknown_Release'Access;
         --  IClassFactory
         CreateInstance   : Af_IClassFactory_CreateInstance
           := IClassFactory_CreateInstance'Access;
         LockServer       : Af_IClassFactory_LockServer
           := IClassFactory_LockServer'Access;
      end record;
   pragma Convention (C, IClassFactory_Vtbl_Record);
   type IClassFactory_Vtbl_Pointer is access all IClassFactory_Vtbl_Record;

   --  Create Vtbls in memory

   IClassFactory_Vtbl : aliased IClassFactory_Vtbl_Record;

   ClassFactory_Object : aliased Interface :=
     (Vtbl      => IClassFactory_Vtbl'Address,
      Ref_Count => 1,
      CoClass   => System.Null_Address);

end GNATExample.Factory;
