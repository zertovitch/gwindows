with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Ada.Characters.Handling;

with Win32.Rpcdce;
with Win32.Winerror;
with Win32.Winbase;
with Win32.Winreg;
with Win32.Winuser;
with Win32.WinMain;

with GNATExample.Factory;

procedure GNATExample.Exe is
   use Interfaces.C;
   use type Win32.Objbase.HRESULT;
   use type Win32.BOOL;

   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("gnatexamplerc.coff");

   procedure Display_Help;
   --  Displays instructions on using the Local Server

   procedure Register (KeyName, ValueName, Value : String);
   --  Adds entries to the NT Registry

   procedure Unregister (KeyName : String);
   --  Removes entries from the NT Registry

   function UnregisterTypeLib (libid     : access Win32.Objbase.IID;
                               wVerMajor : Win32.WORD;
                               wVerMinor : Win32.WORD;
                               lcid      : Win32.Winnt.LCID;
                               syskind   : Win32.Oleauto.SYSKIND)
     return Win32.Winerror.HRESULT;
   pragma Import (Stdcall, UnRegisterTypeLib, "UnRegisterTypeLib");
   --  Not defined in Win32Api, but is in oleaut32.a

   CLSID               : constant String :=
     "{45F9F481-787C-11D3-821C-52544C1913DE}";
   Object_Name        : constant String := "GNATCOMLibrary.GNATCOMClass";
   Library_Version     : constant String := "1";
   Library_Description : constant String := "GNATExample Class";
   --  Registration information

   function To_LPUNKNOWN is
     new Ada.Unchecked_Conversion (Interface_Pointer, Win32.Objbase.LPUNKNOWN);
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


   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Put_Line ("This is a local server for a COM object");
      Put_Line ("To register this server use:");
      Put_Line ("servername -RegServer");
      New_Line;
      Put_Line ("To unregister this server use:");
      Put_Line ("servername -UnregServer");
      New_Line;
      Put ("To start the server up manually:");
      Put_Line (" (COM will do it for you)");
      Put_Line ("servername -Embedding");
   end Display_Help;

   --------------
   -- Register --
   --------------

   procedure Register (KeyName, ValueName, Value : String) is
      use Interfaces.C;

      hkey       : aliased Win32.Winreg.HKEY;
      err        : Win32.Long;
      cKeyName   : aliased char_array := To_C (KeyName);
      cValueName : aliased char_array := To_C (ValueName);
      cValue     : aliased char_array := To_C (Value);
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
      use Interfaces.C;

      err        : Win32.Long;
      cKeyName   : aliased char_array := To_C (KeyName);
   begin
      err := Win32.Winreg.RegDeleteKey (Win32.Winreg.HKEY_CLASSES_ROOT,
                                       To_PCCH (cKeyName'Address));
   end Unregister;


   ---------------------
   -- Local Variables --
   ---------------------

   hr                : Win32.Winerror.HRESULT;
   lResult           : Win32.LRESULT;
   dwRegister        : aliased Win32.DWORD;
   refcount          : Win32.ULONG;
   tMSG              : aliased Win32.Winuser.MSG;
   MSG               : Win32.Winuser.LPMSG := tMSG'Unchecked_Access;
   argument_error,
   com_error         : exception;

   Factory  : Interface_Pointer :=
     GNATExample.Factory.ClassFactory_Object'Access;

begin

   if Argument_Count /= 1 then
      Display_Help;
   else
      if
        (Argument (1) = "/Embedding")
        or
        (Argument (1) = "-Embedding")
      then
         --  Will be creating a LocalServer
         --  This tells our implementation to check if the exe should
         --  shutdown after every created COM object is destroyed.
         InProcServer := False;

         --  Initialize Com Libraries
         hr := Win32.Objbase.CoInitialize (system.null_address);
         if hr /= Win32.Winerror.S_OK then
            put_line ("Unable to initialize COM libraries.");
            put_line ("HR = " & Win32.Winerror.HRESULT'Image (hr));
            raise com_error;
         end if;

         --  Publish Factory to Running Object Table
         hr := Win32.Objbase.CoRegisterClassObject
           (CLSID_GNATCOMClass'Unchecked_Access,
            To_LPUNKNOWN (Factory),
            4, --  CLSCTX_LOCAL_SERVER
            1, --  REGCLS_MULTIPLEUSE,
            dwRegister'Unchecked_Access);

         if hr /= Win32.WinError.S_OK then
            refcount := GNATExample.Factory.IUnknown_Release (Factory);
            Put_Line ("Unable to register IClassFactory.");
            Put_Line ("HR = " & Win32.Winerror.HRESULT'Image (hr));
            raise com_error;
         end if;

         while
           (Win32.Winuser.GetMessage (MSG,
                                     System.Null_Address,
                                     Win32.UINT (0),
                                     Win32.UINT (0)) /= 0)
         loop
            lResult :=
              Win32.Winuser.DispatchMessage (Win32.WinUser.ac_MSG_t (MSG));
         end loop;

         --  Stop Factory
         hr := Win32.Objbase.CoRevokeClassObject (dwRegister);
         refcount := GNATExample.Factory.IUnknown_Release (Factory);

         --  Uninitialize the COM libraries
         Win32.Objbase.CoUninitialize;
      elsif
        (Argument (1) = "/RegServer")
        or
        (Argument (1) = "-RegServer")
      then
         declare
            dr        : Win32.DWORD;
            MAX_PATH  : constant := 1024;
            ExePath   : aliased char_array (1 .. MAX_PATH);
            pTypeLib  : aliased Win32.OleAuto.LPTYPELIB;
            refcount  : Win32.Ulong;
         begin
            dr := Win32.Winbase.GetModuleFileName (Win32.WinMain.Get_Hinstance,
                                                   To_PCHAR (ExePath'Address),
                                                   MAX_PATH);
            declare
               wFilePath : aliased wchar_array := To_C
                 (ada.characters.handling.to_wide_string (To_Ada (ExePath)));
            begin
               hr :=
                 Win32.OleAuto.LoadTypeLib (To_PCWSTR (wFilePath'Address),
                                            pTypeLib'Unchecked_Access);
               hr :=
                 Win32.OleAuto.RegisterTypeLib
                 (pTypeLib,
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

            Register ("CLSID\" & CLSID & "\LocalServer32",
                      "",
                      To_Ada (ExePath));
            --  Path to InProcServer 32 bit implementation of the COM
            --  object (a DLL)

            Register ("CLSID\" & CLSID & "\LocalServer32",
                      "ThreadingModel",
                      "Apartment");
            --  Tells the operating system that this object is aware
            --  of threading issues, but the OS should synchronize
            --  access to the object.  This does not affect the type of
            --  clients that can use this object, ie. a client using
            --  CoInitializeEx(CLSCTX_MULTI_THREADED) can create this
            --  object and the OS will handle synchronization of calls
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
            --  Creates the AppID entry where remoting infromation
            --  about the COM object is stored.

            Put_Line ("Registered COM Object Server");
         end;
      elsif
        (Argument (1) = "/UnregServer")
        or
        (Argument (1) = "-UnregServer")
      then
         hr := UnRegisterTypeLib (LIBID_GNATCOMLibrary'Unchecked_Access, 1, 0,
                                 Win32.Winnt.LANG_NEUTRAL,
                                 Win32.OleAuto.SYS_WIN32);

         Unregister ("CLSID\" & CLSID & "\LocalServer32");
         Unregister ("CLSID\" & CLSID & "\ProgID");
         Unregister ("CLSID\" & CLSID & "\VersionIndependentProgID");
         Unregister ("CLSID\" & CLSID);
         Unregister (Object_Name & "\CLSID");
         Unregister (Object_Name & "\CurVer");
         Unregister (Object_Name);
         Unregister (Object_Name & "." & Library_Version & "\CLSID");
         Unregister (Object_Name & "." & Library_Version);
         Unregister ("AppID\" & CLSID);

         Put_Line ("Unregistered Beep Object Server");
      else
         Display_Help;
      end if;
   end if;
end GNATExample.Exe;






