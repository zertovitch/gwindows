with Ada.Unchecked_Conversion;

with Win32.Rpcdce;
with Win32.Objbase;
with Win32.Winerror;
with Win32.Winbase;

with GNATExample.GNATCOMClass;

package body GNATExample.Factory is

   -----------------------------
   -- IUnknown_QueryInterface --
   -----------------------------

   function IUnknown_QueryInterface
     (This      : access Interface;
      riid      : in     Win32.Objbase.REFIID;
      ppvObject : access Win32.PVOID)
     return HRESULT
   is
      use type Win32.Rpcdce.Guid;
   begin
      if riid.all = IID_IUnknown then
         ppvObject.all := This.all'Address;
      elsif Riid.all = IID_IClassFactory then
         ppvObject.all := This.all'Address;
      else
         ppvObject.all := System.Null_Address;
         return Win32.Winerror.E_NOINTERFACE;
      end if;

      return Win32.Winerror.S_OK;
   end IUnknown_QueryInterface;

   ---------------------
   -- IUnknown_AddRef --
   ---------------------

   function IUnknown_AddRef (This : access Interface)
     return Win32.ULONG
   is
   begin
      return 1;
   end IUnknown_AddRef;

   ----------------------
   -- IUnknown_Release --
   ----------------------

   function IUnknown_Release (This : access Interface)
     return Win32.ULONG
   is
   begin
      return 1;
   end IUnknown_Release;

   ----------------------------------
   -- IClassFactory_CreateInstance --
   ----------------------------------

   function IClassFactory_CreateInstance
     (This        : access Interface;
      pUnkOuter   : in     Win32.Objbase.LPUNKNOWN;
      riid        : in     Win32.Objbase.REFIID;
      ppvObject   : access Win32.PVOID)
     return HRESULT
   is
      use type Win32.Objbase.LPUNKNOWN;

      New_Object : Interface_Pointer;
      Hr         : Win32.Objbase.HRESULT;
      LResult    : Win32.LONG;
      Refcount   : Win32.ULONG;

      function To_IUnknown is
         new Ada.Unchecked_Conversion (Interface_Pointer,
                                       Win32.Objbase.LPUNKNOWN);

   begin
      if pUnkOuter /= null then
         return Win32.Winerror.CLASS_E_NOAGGREGATION;
      end if;

      --  Create new COM object
      New_Object := GNATCOMClass.New_Object;

      Hr := To_IUnknown (New_Object).lpvtbl.QueryInterface
        (To_IUnknown (New_Object),
         riid,
         ppvObject);

      Refcount :=
        To_IUnknown (New_Object).lpvtbl.Release (To_IUnknown (New_Object));

      LResult := Win32.Winbase.InterlockedIncrement (Component_Count'Access);

      return HRESULT (Hr);

   end IClassFactory_CreateInstance;

   ------------------------------
   -- IClassFactory_LockServer --
   ------------------------------

   function IClassFactory_LockServer
     (This  : access Interface;
      fLock : in     Win32.BOOL)
     return HRESULT
   is
      use type Win32.BOOL;
      lResult : Win32.LONG;
   begin
      if fLock = Win32.TRUE then
         lResult :=
           Win32.Winbase.InterlockedIncrement (Server_Lock_Count'Access);
      else
         lResult :=
           Win32.Winbase.InterlockedDecrement (Server_Lock_Count'Access);
      end if;

      --  Check to see if server can close down
      Can_Close;

      return Win32.Winerror.S_OK;
   end IClassFactory_LockServer;

end GNATExample.Factory;
