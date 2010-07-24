------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                     G N A T C O M . R E G I S T E R                      --
--                                                                          --
--                                B o d y                                   --
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

with Ada.Exceptions;

with GNATCOM.Errors;
with GNATCOM.BSTR;
with GNATCOM.GUID;

package body GNATCOM.Register is

   REG_SZ : constant := 1;
   subtype EREGTYPE is Interfaces.C.long;

   SYS_WIN32 : constant := 1;
   subtype SYSKIND is Interfaces.C.long;

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);

   function RegCreateKey
     (hKey      : in     Interfaces.C.long;
      lpSubKey  : in     Interfaces.C.char_array;
      phkResult : access Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegCreateKey, "RegCreateKeyA");

   function RegSetValueEx
     (hKey        : Interfaces.C.long;
      lpValueName : Interfaces.C.char_array;
      reserved    : Interfaces.C.unsigned_long;
      dwType      : EREGTYPE;
      lpData      : Interfaces.C.char_array;
      cbData      : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegSetValueEx, "RegSetValueExA");

   function RegDeleteKey
     (hKey     : Interfaces.C.long;
      lpSubKey : Interfaces.C.char_array)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegDeleteKey, "RegDeleteKeyA");

   function GetModuleFileName
     (hInst        : in     Interfaces.C.long;
      lpszFileName : access Interfaces.C.char;
      cbFileName   : in     Interfaces.C.int)
     return Interfaces.C.int;
   pragma Import (StdCall, GetModuleFileName, "GetModuleFileNameA");

   function LoadTypeLib
     (wszFile : in     GNATCOM.Types.BSTR;
      ppTLib  : access GNATCOM.Types.Pointer_To_ITypeLib)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, LoadTypeLib, "LoadTypeLib");

   function RegisterTypeLib
     (ptlib       : GNATCOM.Types.Pointer_To_ITypeLib;
      wszFullPath : GNATCOM.Types.BSTR;
      wszHelpDir  : GNATCOM.Types.BSTR)
    return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegisterTypeLib, "RegisterTypeLib");

   function UnregisterTypeLib
     (libid     : access GNATCOM.Types.GUID;
      wVerMajor : Interfaces.C.unsigned_short;
      wVerMinor : Interfaces.C.unsigned_short;
      lcid      : Interfaces.C.unsigned_long;
      syskind   : GNATCOM.Register.SYSKIND)
     return GNATCOM.Types.HRESULT;
   pragma Import (Stdcall, UnregisterTypeLib, "UnRegisterTypeLib");

   --------------
   -- Register --
   --------------

   procedure Register (KeyName, Name, Value : in String;
                       Root_Key             : in Interfaces.C.long :=
                         HKEY_CLASSES_ROOT)
   is
      use type Interfaces.C.unsigned_long;

      Key : aliased Interfaces.C.long;
   begin

      Error_Check
        (RegCreateKey (Root_Key,
                       Interfaces.C.To_C (KeyName),
                       Key'Access));

      Error_Check
        (RegSetValueEx (Key,
                        Interfaces.C.To_C (Name),
                        0,
                        REG_SZ,
                        Interfaces.C.To_C (Value),
                        Value'Length + 1));         -- 1 added for C Null
   end Register;

   ----------------------------
   -- Register_Inproc_Server --
   ----------------------------

   procedure Register_Inproc_Server
     (hInstance    : in Interfaces.C.long;
      CLSID        : in GNATCOM.Types.GUID;
      Name         : in String;
      Version      : in String;
      Description  : in String;
      Thread_Model : in String := "Apartment")
   is
      use type Interfaces.C.int;

      MAX_PATH   : constant := 1024;
      ServerPath : aliased Interfaces.C.char_array (1 .. MAX_PATH);
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      if
        GetModuleFileName (hInstance, ServerPath (1)'Access, MAX_PATH) < 0
      then
         raise FILE_NAME_ERROR;
      end if;

      Register ("CLSID\" & Class_ID, "", Description);
      Register ("CLSID\" & Class_ID, "AppID", Class_ID);
      Register ("CLSID\" & Class_ID & "\InProcServer32", "",
                Interfaces.C.To_Ada (ServerPath));
      Register ("CLSID\" & Class_ID & "\InProcServer32",
                "ThreadingModel",
                Thread_Model);
      Register ("CLSID\" & Class_ID & "\ProgID",
                "",
                Name & "." & Version);
      Register ("CLSID\" & Class_ID & "\VersionIndependentProgID",
                "",
                Name);
      Register (Name, "", Description);
      Register (Name & "\CLSID", "", Class_ID);
      Register (Name & "\CurVer",
                "",
                Name & "." & Version);
      Register (Name & "." & Version,
                "",
                Name);
      Register (Name & "." & Version & "\CLSID",
                "",
                Class_ID);
      Register ("AppID\" & Class_ID, "", Description);
      Register ("AppID\" & Class_ID, "DllSurrogate", "");
   end Register_Inproc_Server;

   ---------------------------
   -- Register_Local_Server --
   ---------------------------

   procedure Register_Local_Server
     (hInstance    : in Interfaces.C.long;
      CLSID        : in GNATCOM.Types.GUID;
      Name         : in String;
      Version      : in String;
      Description  : in String)
   is
      use type Interfaces.C.int;

      MAX_PATH   : constant := 1024;
      ServerPath : aliased Interfaces.C.char_array (1 .. MAX_PATH);
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      if
        GetModuleFileName (hInstance, ServerPath (1)'Access, MAX_PATH) < 0
      then
         raise FILE_NAME_ERROR;
      end if;

      Register ("CLSID\" & Class_ID, "", Description);
      Register ("CLSID\" & Class_ID, "AppID", Class_ID);
      Register ("CLSID\" & Class_ID & "\LocalServer32", "",
                Interfaces.C.To_Ada (ServerPath));
      Register ("CLSID\" & Class_ID & "\ProgID",
                "",
                Name & "." & Version);
      Register ("CLSID\" & Class_ID & "\VersionIndependentProgID",
                "",
                Name);
      Register (Name, "", Description);
      Register (Name & "\CLSID", "", Class_ID);
      Register (Name & "\CurVer",
                "",
                Name & "." & Version);
      Register (Name & "." & Version,
                "",
                Name);
      Register (Name & "." & Version & "\CLSID",
                "",
                Class_ID);
      Register ("AppID\" & Class_ID, "", Description);
   end Register_Local_Server;

   ----------------------------
   -- Register_Remote_Server --
   ----------------------------

   procedure Register_Remote_Server
     (CLSID          : in GNATCOM.Types.GUID;
      Name           : in String;
      Version        : in String;
      Description    : in String;
      Remote_Machine : in String)
   is
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      Register ("CLSID\" & Class_ID, "", "Beep Class");
      Register ("CLSID\" & Class_ID, "AppID", Class_ID);
      Register ("CLSID\" & Class_ID & "\ProgID",
                "",
                Name & "." & Version);
      Register ("CLSID\" & Class_ID & "\VersionIndependentProgID",
                "",
                Name);
      Register (Name, "", Description);
      Register (Name & "\CLSID", "", Class_ID);
      Register (Name & "\CurVer",
                "",
                Name & "." & Version);
      Register (Name & "." & Version,
                "",
                Name);
      Register (Name & "." & Version & "\CLSID",
                "",
                Class_ID);
      Register ("AppID\" & Class_ID, "", Description);
      Register ("AppID\" & Class_ID, "RemoteServerName", Remote_Machine);
   end Register_Remote_Server;

   ---------------------------
   -- Register_Type_Library --
   ---------------------------

   procedure Register_Type_Library (hInstance : in Interfaces.C.long) is
      use type Interfaces.C.int;

      MAX_PATH   : constant := 1024;
      ServerPath : aliased Interfaces.C.char_array (1 .. MAX_PATH);
   begin
      if
        GetModuleFileName (hInstance, ServerPath (1)'Access, MAX_PATH) < 0
      then
         raise FILE_NAME_ERROR;
      end if;

      Register_Type_Library (GNATCOM.BSTR.To_BSTR_From_C (ServerPath));

   end Register_Type_Library;

   ---------------------------
   -- Register_Type_Library --
   ---------------------------

   procedure Register_Type_Library
     (Path  : in GNATCOM.Types.BSTR;
      Clear : in Boolean            := True)
   is
      TypeLib    : aliased GNATCOM.Types.Pointer_To_ITypeLib;
      Refcount   : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Refcount);
   begin
      Error_Check (LoadTypeLib (Path, TypeLib'Access));
      Error_Check (RegisterTypeLib (TypeLib, Path, null));

      Refcount := TypeLib.Vtbl.Release (TypeLib);

      if Clear then
         GNATCOM.BSTR.Free (Path);
      end if;
   end Register_Type_Library;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (KeyName  : in String;
                         Root_Key : in Interfaces.C.long := HKEY_CLASSES_ROOT)
   is
   begin
      Error_Check
        (RegDeleteKey (Root_Key,
                       Interfaces.C.To_C (KeyName)));
   end Unregister;

   -----------------------
   -- Unregister_Server --
   -----------------------

   procedure Unregister_Server
     (CLSID   : in GNATCOM.Types.GUID;
      Name    : in String;
      Version : in String)
   is
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      Unregister ("CLSID\" & Class_ID & "\InProcServer32");
      Unregister ("CLSID\" & Class_ID & "\LocalServer32");
      Unregister ("CLSID\" & Class_ID & "\ProgID");
      Unregister ("CLSID\" & Class_ID & "\VersionIndependentProgID");
      Unregister ("CLSID\" & Class_ID);
      Unregister (Name & "\CLSID");
      Unregister (Name & "\CurVer");
      Unregister (Name);
      Unregister (Name & "." & Version & "\CLSID");
      Unregister (Name & "." & Version);
      Unregister ("AppID\" & Class_ID);
   end Unregister_Server;

   -----------------------------
   -- Unregister_Type_Library --
   -----------------------------

   procedure Unregister_Type_Library (LIBID : in GNATCOM.Types.GUID) is
      New_LIBID : aliased GNATCOM.Types.GUID := LIBID;
   begin
      Error_Check (UnregisterTypeLib (New_LIBID'Access,
                                      1,
                                      0,
                                      0,
                                      SYS_WIN32));
   end Unregister_Type_Library;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      if GNATCOM.Errors.FAILED (Result) then
         declare
            Message : constant String := GNATCOM.Errors.To_String (Result);
         begin
            case Result is
               when TYPE_E_IOERROR =>
                  Ada.Exceptions.Raise_Exception
                    (IO_ERROR'Identity,
                     Message);
               when TYPE_E_INVALIDSTATE =>
                  Ada.Exceptions.Raise_Exception
                    (IO_ERROR'Identity,
                     Message);
               when TYPE_E_INVDATAREAD =>
                  Ada.Exceptions.Raise_Exception
                    (IO_ERROR'Identity,
                     Message);
               when  TYPE_E_UNSUPFORMAT =>
                  Ada.Exceptions.Raise_Exception
                    (IO_ERROR'Identity,
                     Message);
               when TYPE_E_CANTLOADLIBRARY =>
                  Ada.Exceptions.Raise_Exception
                    (IO_ERROR'Identity,
                     Message);
               when TYPE_E_REGISTRYACCESS =>
                  Ada.Exceptions.Raise_Exception
                    (REGISTRY_ERROR'Identity,
                     Message);
               when others =>
                  GNATCOM.Errors.Error_Check (Result);
            end case;
         end;
      end if;
   end Error_Check;

end GNATCOM.Register;
