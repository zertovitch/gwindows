with Ada.Unchecked_Conversion;
with Ada.Characters.Handling;

with Win32.Rpcdce;
with Win32.Winerror;
with Win32.Winbase;
with Win32.Winreg;

with GNATExample.Factory;

package body GNATExample.DLL is

   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("GNATExamplerc.coff");

   package C renames Interfaces.C;

   function DllMain (hinstDLL    : Win32.Windef.HINSTANCE;
                     fdwReason   : Win32.DWORD;
                     lpvReserved : Win32.LPVOID)
     return Win32.BOOL;
   pragma Export (StdCall, DllMain, "DllMain");
   --  Main entry point for DLLs

   procedure Register (KeyName, ValueName, Value : String);
   --  Adds entries to the NT Registry

   procedure Unregister (KeyName : String);
   --  Removes entries from the NT Registry

   function UnregisterTypeLib (libid     : access Win32.Objbase.IID;
                              wVerMajor : Win32.WORD;
                              wVerMinor : Win32.WORD;
                              lcid      : Win32.Winnt.LCID;
                              syskind   : Win32.Oleauto.SYSKIND)
                              return HRESULT;
   pragma Import (Stdcall, UnRegisterTypeLib, "UnRegisterTypeLib");
   --  Not defined in Win32Api, but is in oleaut32.a

   CLSID               : constant String :=
     "{45F9F481-787C-11D3-821C-52544C1913DE}";
   Object_Name        : constant String := "GNATCOMLibrary.GNATCOMClass";
   Library_Version     : constant String := "1";
   Library_Description : constant String := "GNATExample Class";
   --  Registration information

   function To_PCBYTE is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCBYTE);
   function To_PCCH is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCCH);
   function To_PCHAR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCHAR);
   function To_PCWSTR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCWSTR);
   function To_PWSTR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PWSTR);
   --  Type conversions

   -------------
   -- DllMain --
   -------------

   function DllMain (hinstDLL    : Win32.Windef.HINSTANCE;
                     fdwReason   : Win32.DWORD;
                     lpvReserved : Win32.LPVOID)
     return Win32.BOOL
   is
      procedure Adainit;
      pragma Import (C, Adainit);

      procedure Adafinal;
      pragma Import (C, Adafinal);

      DLL_PROCESS_DETACH : constant := 0;
      DLL_PROCESS_ATTACH : constant := 1;
   begin
      case fdwReason is
         when DLL_PROCESS_ATTACH =>
            AdaInit;
            hInstance := hinstDll;
            return 1;
         when DLL_PROCESS_DETACH =>
            AdaFinal;
            return 1;
         when others =>
            return 1;
      end case;
   end DllMain;

   -----------------------
   -- DllGetClassObject --
   -----------------------

   function DllGetClassObject
     (clsid : in     Win32.Objbase.REFCLSID;
      riid  : in     Win32.Objbase.REFIID;
      ppv   : access Win32.PVOID)
     return HRESULT
   is
      use type Win32.Rpcdce.GUID;

      function To_IUnknown is
         new Ada.Unchecked_Conversion (Interface_Pointer,
                                       Win32.Objbase.LPUNKNOWN);
      Factory  : Interface_Pointer :=
        GNATExample.Factory.ClassFactory_Object'Access;
      Hr       : Win32.Objbase.HRESULT;
      Refcount : Win32.ULONG;
   begin
      --  This server only handles this object
      if clsid.all /= CLSID_GNATCOMClass then
         return Win32.Winerror.CLASS_E_CLASSNOTAVAILABLE;
      end if;

      Hr :=
        To_IUnknown (Factory).lpvtbl.QueryInterface (To_IUnknown (Factory),
                                                      riid,
                                                      ppv);
      Refcount :=
        To_IUnknown (Factory).lpvtbl.Release (To_IUnknown (Factory));

      return HRESULT (Hr);

   end DllGetClassObject;

   ---------------------
   -- DllCanUnloadNow --
   ---------------------

   function DllCanUnloadNow return HRESULT is
      use type Interfaces.C.Long;
   begin
      if (Server_Lock_Count = 0) and (Component_Count = 0) then
         return Win32.Winerror.S_OK;
      else
         return Win32.Winerror.S_FALSE;
      end if;
   end DllCanUnloadNow;

   -----------------------
   -- DllRegisterServer --
   -----------------------

   function DllRegisterServer return HRESULT is
      hr        : Win32.Objbase.HRESULT;
      dr        : Win32.DWORD;
      MAX_PATH  : constant := 1024;
      DllPath   : aliased C.char_array (1 .. MAX_PATH);
      pTypeLib  : aliased Win32.OleAuto.LPTYPELIB;
      refcount  : Win32.Ulong;
   begin
      dr := Win32.Winbase.GetModuleFileName (hInstance,
                                             To_PCHAR (DllPath'Address),
                                             MAX_PATH);
      declare
         wFilePath : aliased C.wchar_array := C.To_C
            (Ada.Characters.Handling.To_Wide_String (C.To_Ada (DLLPath)));
      begin
         hr := Win32.OleAuto.LoadTypeLib (To_PCWSTR (wFilePath'Address),
                                          pTypeLib'Unchecked_Access);
         hr := Win32.OleAuto.RegisterTypeLib (pTypeLib,
                                              To_PWSTR (wFilePath'Address),
                                              To_PWSTR (System.Null_Address));
         refcount := pTypeLib.lpVtbl.Release (pTypeLib);
      end;

      Register ("CLSID\" & CLSID, "", Library_Description);
      --  CLSID = CLASS ID of the COM Object (CoClass GUID)
      --  Library_Description = Description of object to appear in COM
      --  browsers

      Register ("CLSID\" & CLSID, "AppID", CLSID);
      --  This entry links together the COM information (under \CLSID)
      --  with the remoting information for DCOM (under \APPID)

      Register ("CLSID\" & CLSID & "\InProcServer32", "", C.To_Ada (DllPath));
      --  Path to InProcServer 32 bit implementation of the COM object (a DLL)

      Register ("CLSID\" & CLSID & "\InProcServer32",
                "ThreadingModel",
                "Apartment");
      --  Tells the operating system that this object is aware of threading
      --  issues, but the OS should synchronize access to the object.
      --  This does not affect the type of clients that can use this object,
      --  ie. a client using CoInitializeEx(CLSCTX_MULTI_THREADED) can
      --  create this object and the OS will handle synchronization of calls
      --  made by each thread trying to access the object.


      Register ("CLSID\" & CLSID & "\ProgID",
                "",
                Object_Name & "." & Library_Version);
      --  Links the CLSID to the PROGID
      --  The ProgID is the human readable name of the COM object used
      --  by VB and other languages instead of the GUID.
      --  The PROGID also contains a dot followed by the version number.

      Register ("CLSID\" & CLSID & "\VersionIndependentProgID",
                "",
                Object_Name);
      --  Links the CLSID to the PROGID with out the version number

      Register (Object_Name, "", Library_Description);
      --  Creates the version independant PROGID entry

      Register (Object_Name & "\CLSID", "", CLSID);
      --  Links the PROGID to the CLSID of the object

      Register (Object_Name & "\CurVer",
                "",
                Object_Name & "." & Library_Version);
      --  Links the version independant PROGID to the version
      --  dependant PROGID

      Register (Object_Name & "." & Library_Version,
                "",
                Object_Name);
      --  Creates the PROGID entry

      Register (Object_Name & "." & Library_Version & "\CLSID",
                "",
                CLSID);
      --  Links the PROGID to the CLSID

      Register ("AppID\" & CLSID, "", Library_Description);
      --  Creates the AppID entry where remoting infromation about the
      --  COM object is stored.

      Register ("AppID\" & CLSID, "DllSurrogate", "");
      --  Tells the opearting system that if a request is made to run
      --  the COM object as a DCOM object or as a separate process
      --  (LocalServer) to provide this for the dll using a built in NT
      --  application DLLHOST.EXE

      return Win32.Winerror.S_OK;
   end DllRegisterServer;

   -------------------------
   -- DllUnregisterServer --
   -------------------------

   function DllUnregisterServer return HRESULT is
      hr : HRESULT;
   begin
      hr := UnregisterTypeLib (LIBID_GNATCOMLibrary'Unchecked_Access,
                              1,
                              0,
                              Win32.Winnt.LANG_NEUTRAL,
                              Win32.OleAuto.SYS_WIN32);

      Unregister ("CLSID\" & CLSID & "\InProcServer32");
      Unregister ("CLSID\" & CLSID & "\ProgID");
      Unregister ("CLSID\" & CLSID & "\VersionIndependentProgID");
      Unregister ("CLSID\" & CLSID);
      Unregister (Object_Name & "\CLSID");
      Unregister (Object_Name & "\CurVer");
      Unregister (Object_Name);
      Unregister (Object_Name & "." & Library_Version & "\CLSID");
      Unregister (Object_Name & "." & Library_Version);
      Unregister ("AppID\" & CLSID);
      return Win32.Winerror.S_OK;
   end DllUnregisterServer;

   --------------
   -- Register --
   --------------

   procedure Register (KeyName, ValueName, Value : String) is
      hkey       : aliased Win32.Winreg.HKEY;
      err        : Win32.Long;
      cKeyName   : aliased C.char_array := C.To_C (KeyName);
      cValueName : aliased C.char_array := C.To_C (ValueName);
      cValue     : aliased C.char_array := C.To_C (Value);
   begin
      err := Win32.Winreg.RegCreateKey (Win32.Winreg.HKEY_CLASSES_ROOT,
                                        To_PCCH (cKeyName'Address),
                                        hkey'Unchecked_Access);

      err := Win32.Winreg.RegSetValueEx (hkey, To_PCCH (cValueName'Address),
                                         0,
                                         Win32.Winnt.REG_SZ,
                                         To_PCBYTE (cValue'Address),
                                         cValue'Length);

      err := Win32.Winreg.RegCloseKey (hkey);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (KeyName : String) is
      err        : Win32.Long;
      cKeyName   : aliased C.char_array := C.To_C (KeyName);
   begin
      err := Win32.Winreg.RegDeleteKey (Win32.Winreg.HKEY_CLASSES_ROOT,
                                       To_PCCH (cKeyName'Address));
   end Unregister;

end GNATExample.DLL;
