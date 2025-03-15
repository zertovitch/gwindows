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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNATCOM.Errors;
with GNATCOM.BSTR;
with GNATCOM.GUID;
with GNATCOM.Iinterface;

package body GNATCOM.Register is

   REG_SZ : constant := 1;
   subtype EREGTYPE is Interfaces.C.long;

   SYS_WIN32 : constant := 1;
   subtype SYSKIND is Interfaces.C.long;

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);

   function RegCreateKey
     (hKey      : in     Interfaces.C.ptrdiff_t;
      lpSubKey  : in     Interfaces.C.char_array;
      phkResult : access Interfaces.C.ptrdiff_t)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegCreateKey, "RegCreateKeyA");

   function RegSetValueEx
     (hKey        : Interfaces.C.ptrdiff_t;
      lpValueName : Interfaces.C.char_array;
      reserved    : Interfaces.C.unsigned_long;
      dwType      : EREGTYPE;
      lpData      : Interfaces.C.char_array;
      cbData      : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegSetValueEx, "RegSetValueExA");

   function RegDeleteKey
     (hKey     : Interfaces.C.ptrdiff_t;
      lpSubKey : Interfaces.C.char_array)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, RegDeleteKey, "RegDeleteKeyA");

   function GetModuleFileName
     (hInst        : in     Interfaces.C.ptrdiff_t;
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
                       Root_Key             : in Interfaces.C.ptrdiff_t :=
                         HKEY_CLASSES_ROOT)
   is
      use type Interfaces.C.unsigned_long;

      Key : aliased Interfaces.C.ptrdiff_t;
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

   -------------------------------------
   -- Register_Implemented_Categories --
   -------------------------------------

   procedure Register_Implemented_Categories
     (CLSID                  : access GNATCOM.Types.GUID;
      Implemented_Categories : in GNATCOM.Types.GUID_Array)
   is
      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_ICatRegister,
         GNATCOM.Types.Pointer_To_Pointer_To_Void);
      Cat_Mgr    : GNATCOM.Iinterface.Interface_Type;
      Cat_Reg    : aliased GNATCOM.Types.Pointer_To_ICatRegister;
      Hr         : GNATCOM.Types.HRESULT;
      Ignored    : Interfaces.C.unsigned_long;
   begin
      GNATCOM.Iinterface.Create
        (Cat_Mgr, GNATCOM.Types.CLSID_StdComponentCategoriesMgr);
      Hr := GNATCOM.Iinterface.QueryInterface
        (Cat_Mgr, GNATCOM.Types.IID_ICatRegister,
         To_Pointer_To_Pointer_To_Void (Cat_Reg'Unchecked_Access));
      GNATCOM.Errors.Error_Check (Hr);
      Hr := Cat_Reg.Vtbl.RegisterClassImplCategories
        (Cat_Reg,
         CLSID,
         Implemented_Categories'Length,
         Implemented_Categories
           (Implemented_Categories'First)'Unrestricted_Access);
      Ignored := Cat_Reg.Vtbl.Release (Cat_Reg);
      GNATCOM.Errors.Error_Check (Hr);
   end Register_Implemented_Categories;

   ----------------------------
   -- Register_Inproc_Server --
   ----------------------------

   procedure Register_Inproc_Server
     (hInstance    : in Interfaces.C.ptrdiff_t;
      CLSID        : in GNATCOM.Types.GUID;
      Name         : in String;
      Version      : in String;
      Description  : in String;
      Thread_Model : in String := "Apartment";
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>))
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
      Register_Implemented_Categories
        (CLSID'Unrestricted_Access, Implemented_Categories);
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
     (hInstance    : in Interfaces.C.ptrdiff_t;
      CLSID        : in GNATCOM.Types.GUID;
      Name         : in String;
      Version      : in String;
      Description  : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>);
      Service_Name : in String := "";
      APPID        : in GNATCOM.Types.GUID := GNATCOM.Types.GUID_NULL)
   is
      use type Interfaces.C.int;
      use type GNATCOM.Types.GUID;

      MAX_PATH   : constant := 1024;
      ServerPath : aliased Interfaces.C.char_array (1 .. MAX_PATH);
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
      App_ID    : constant String :=
        GNATCOM.GUID.To_String
          ((if APPID = GNATCOM.Types.GUID_NULL then CLSID else APPID));
      Lib_ID : String renames App_ID;
      --  In ATL LIBID and APPID map to the same value.
      Prog_ID   : constant String := Name & "." & Version;
   begin
      if
        GetModuleFileName (hInstance, ServerPath (1)'Access, MAX_PATH) < 0
      then
         raise FILE_NAME_ERROR;
      end if;

      declare
         Server_Exe : constant String := Interfaces.C.To_Ada (ServerPath);
         Exe_Name   : constant String :=
           Ada.Directories.Simple_Name (Server_Exe);
      begin
         Register ("AppID\" & App_ID, "", Exe_Name);
         Register ("AppID\" & Exe_Name, "AppID", App_ID);

         Register ("CLSID\" & Class_ID, "", Description);
         Register ("CLSID\" & Class_ID, "AppID", App_ID);

         Register
           ("CLSID\" & Class_ID & "\LocalServer32", "",
            """" & Server_Exe & """");
         Register
           ("CLSID\" & Class_ID & "\LocalServer32", "ServerExecutable",
            Server_Exe);
         Register ("CLSID\" & Class_ID & "\ProgID", "", Prog_ID);
         Register ("CLSID\" & Class_ID & "\Programmable", "", "");
         Register ("CLSID\" & Class_ID & "\TypeLib", "", Lib_ID);
         Register ("CLSID\" & Class_ID & "\Version", "", Version);
         Register
           ("CLSID\" & Class_ID & "\VersionIndependentProgID", "", Name);

         Register (Name, "", Description);
         Register (Name & "\CurVer", "", Prog_ID);
         Register (Prog_ID, "", Description);
         Register (Prog_ID & "\CLSID", "", Class_ID);

         Register_Implemented_Categories
           (CLSID'Unrestricted_Access, Implemented_Categories);

         if Service_Name /= "" then
            Register ("AppID\" & App_ID, "LocalService", Service_Name);
            Register ("AppID\" & App_ID, "ServiceParameters", "/Embedding");
         end if;
      end;

   end Register_Local_Server;

   ----------------------------
   -- Register_Remote_Server --
   ----------------------------

   procedure Register_Remote_Server
     (CLSID          : in GNATCOM.Types.GUID;
      Name           : in String;
      Version        : in String;
      Description    : in String;
      Remote_Machine : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>))
   is
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      Register ("CLSID\" & Class_ID, "", "Beep Class");
      Register ("CLSID\" & Class_ID, "AppID", Class_ID);
      Register_Implemented_Categories
        (CLSID'Unrestricted_Access, Implemented_Categories);
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

   procedure Register_Type_Library (hInstance : in Interfaces.C.ptrdiff_t) is
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
                         Root_Key : in Interfaces.C.ptrdiff_t :=
                           HKEY_CLASSES_ROOT)
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
      Version : in String;
      Implemented_Categories : in GNATCOM.Types.GUID_Array := (2 .. 1 => <>))
   is
      Class_ID  : constant String := GNATCOM.GUID.To_String (CLSID);
   begin
      Unregister ("CLSID\" & Class_ID & "\InProcServer32");
      Unregister ("CLSID\" & Class_ID & "\LocalServer32");
      Unregister ("CLSID\" & Class_ID & "\ProgID");
      Unregister ("CLSID\" & Class_ID & "\VersionIndependentProgID");

      for CATID of Implemented_Categories loop
         declare
            Category_ID : constant String := GNATCOM.GUID.To_String (CATID);
         begin
            Unregister
              ("CLSID\" & Class_ID & "\Implemented Categories\" & Category_ID);
         end;
      end loop;
      Unregister ("CLSID\" & Class_ID & "\Implemented Categories");

      Unregister ("CLSID\" & Class_ID);
      Unregister (Name & "\CLSID");
      Unregister (Name & "\CurVer");
      Unregister (Name);
      Unregister (Name & "." & Version & "\CLSID");
      Unregister (Name & "." & Version);
      Unregister ("AppID\" & Class_ID);
   end Unregister_Server;

   ---------------------------------
   -- Register_Component_Category --
   ---------------------------------

   procedure Register_Component_Category
     (Categories : access GNATCOM.Types.CATEGORYINFO_Array)
   is
      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_ICatRegister,
         GNATCOM.Types.Pointer_To_Pointer_To_Void);
      Cat_Mgr    : GNATCOM.Iinterface.Interface_Type;
      Cat_Reg    : aliased GNATCOM.Types.Pointer_To_ICatRegister;
      Hr         : GNATCOM.Types.HRESULT;
      Ignored    : Interfaces.C.unsigned_long;
   begin
      if Categories /= null then
         GNATCOM.Iinterface.Create
           (Cat_Mgr, GNATCOM.Types.CLSID_StdComponentCategoriesMgr);
         Hr := GNATCOM.Iinterface.QueryInterface
           (Cat_Mgr,
            GNATCOM.Types.IID_ICatRegister,
            To_Pointer_To_Pointer_To_Void (Cat_Reg'Unchecked_Access));
         GNATCOM.Errors.Error_Check (Hr);
         Hr := Cat_Reg.Vtbl.RegisterCategories
           (Cat_Reg,
            Categories'Length,
            Categories (Categories'First)'Access);
         Ignored := Cat_Reg.Vtbl.Release (Cat_Reg);
         GNATCOM.Errors.Error_Check (Hr);
      end if;
   end Register_Component_Category;

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
