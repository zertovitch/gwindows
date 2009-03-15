with Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Win32.Objbase;
with Win32.Oleauto;
with Win32.Winbase;
with Win32.Winmain;
with Win32.Winerror;
with Win32.Winreg;
with Win32.Winnt;

procedure Remote is
   pragma Linker_Options ("gnatexamplerc.coff");
   pragma Linker_Options ("-loleaut32");

   package C renames Interfaces.C;

   CLSID                : constant String :=
     "{45F9F481-787C-11D3-821C-52544C1913DE}";
   Library_Name         : constant String := "GNATCOMLibrary.GNATCOMClass";
   Library_Version      : constant String := "1";
   Library_Description  : constant String := "GNATExample Class";

   LIBID_GNATCOMLibrary : constant Win32.Objbase.IID :=
     (16#45F9F480#, 16#787C#, 16#11D3#,
       (C.char'Val (16#82#), C.char'Val (16#1C#),
        C.char'Val (16#52#), C.char'Val (16#54#), C.char'Val (16#4C#),
        C.char'Val (16#19#), C.char'Val (16#13#), C.char'Val (16#DE#)));

   Library_ID           : aliased Win32.Objbase.IID := LIBID_GNATCOMLibrary;
   --  Registration information

   function To_PCHAR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCHAR);
   function To_PCBYTE is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCBYTE);
   function To_PCCH is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCCH);
   function To_PCWSTR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PCWSTR);
   function To_PWSTR is
     new Ada.Unchecked_Conversion (System.Address, Win32.PWSTR);
   --  Type conversions

   procedure Display_Help;
   --  Displays instructions on using the Local Server

   procedure Register (KeyName, ValueName, Value : String);
   --  Adds entries to the NT Registry

   procedure Unregister (KeyName : String);
   --  Removes entries from the NT Registry

   function UnregisterTypeLib (libid     : access Win32.Objbase.IID;
                               wVerMajor : in     Win32.WORD;
                               wVerMinor : in     Win32.WORD;
                               lcid      : in     Win32.Winnt.LCID;
                               syskind   : in     Win32.Oleauto.SYSKIND)
                              return Win32.Winerror.HRESULT;
   pragma Import (Stdcall, UnRegisterTypeLib, "UnRegisterTypeLib");
   --  Not defined in Win32Api, but is in oleaut32.a

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Put_Line ("This is a registration server for a DCOM object");
      Put_Line ("To register the DCOM object use:");
      Put_Line ("appname DCOMServerName");
      New_Line;
      Put_Line ("To unregister this server use:");
      Put_Line ("servername -UnregServer");
      New_Line;
   end Display_Help;

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

   ---------------------
   -- Local Variables --
   ---------------------

   hr                : Win32.Winerror.HRESULT;

begin

   if Argument_Count /= 1 then
      Display_Help;
   elsif
     (Argument (1) = "/UnregServer")
     or
     (Argument (1) = "-UnregServer")
   then
      hr := UnRegisterTypeLib (Library_ID'Unchecked_Access, 1, 0,
                               Win32.Winnt.LANG_NEUTRAL,
                               Win32.OleAuto.SYS_WIN32);

      Unregister ("CLSID\" & CLSID & "\LocalServer32");
      Unregister ("CLSID\" & CLSID & "\ProgID");
      Unregister ("CLSID\" & CLSID & "\VersionIndependentProgID");
      Unregister ("CLSID\" & CLSID);
      Unregister (Library_Name & "\CLSID");
      Unregister (Library_Name & "\CurVer");
      Unregister (Library_Name);
      Unregister (Library_Name & "." & Library_Version & "\CLSID");
      Unregister (Library_Name & "." & Library_Version);
      Unregister ("AppID\" & CLSID);

      Put_Line ("Unregistered DCOM Object " & Library_Name);
   else
      declare
         dr        : Win32.DWORD;
         MAX_PATH  : constant := 1024;
         ExePath   : aliased C.Char_Array (1 .. MAX_PATH);
         pTypeLib  : aliased Win32.OleAuto.LPTYPELIB;
         refcount  : Win32.Ulong;
      begin
         dr := Win32.Winbase.GetModuleFileName (Win32.WinMain.Get_Hinstance,
                                                To_PCHAR (ExePath'Address),
                                                MAX_PATH);
         declare
            wFilePath : aliased C.wchar_array := C.To_C (
              ada.characters.handling.to_wide_string (
                C.To_Ada (ExePath)));
         begin
            hr :=
              Win32.OleAuto.LoadTypeLib (To_PCWSTR (wFilePath'Address),
                                         pTypeLib'Unchecked_Access);
            hr :=
              Win32.OleAuto.RegisterTypeLib (pTypeLib,
                                             To_PWSTR (wFilePath'Address),
                                             To_PWSTR (System.Null_Address));
            refcount := pTypeLib.lpVtbl.Release (pTypeLib);
         end;

         Register ("CLSID\" & CLSID, "", Library_Description);
         Register ("CLSID\" & CLSID, "AppID", CLSID);
         Register ("CLSID\" & CLSID & "\ProgID",
                   "",
                   Library_Name & "." & Library_Version);
         Register ("CLSID\" & CLSID & "\VersionIndependentProgID",
                   "",
                   Library_Name);
         Register (Library_Name, "", Library_Description);
         Register (Library_Name & "\CLSID", "", CLSID);
         Register (Library_Name & "\CurVer",
                   "",
                   Library_Name & "." & Library_Version);
         Register (Library_Name & "." & Library_Version,
                   "",
                   Library_Name);
         Register (Library_Name & "." & Library_Version & "\CLSID",
                   "",
                   CLSID);
         Register ("AppID\" & CLSID, "", Library_Description);
         Register ("AppID\" & CLSID, "RemoteServerName", Argument (1));

         Put_Line ("Registered DCOM Object");
         Put_Line ("This application contains information need for remote");
         Put_Line ("use of the object. If you change the location of this");
         Put_Line ("application, you must reregister the DCOM object.");
      end;
   end if;

end Remote;





