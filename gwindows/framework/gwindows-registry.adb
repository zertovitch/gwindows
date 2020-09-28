------------------------------------------------------------------------------
--                                                                          --
--           GWINDOWS - Ada 95 Framework for Windows Development            --
--                                                                          --
--                   G W I N D O W S . R E G I S T R Y                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2020 David Botton                   --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;

with Interfaces.C;

with GWindows.GStrings;
with GWindows.Errors;
with GWindows.Internal;
with GWindows.Types;

package body GWindows.Registry is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return GString is
      use Ada.Strings.Fixed;
      use Ada.Strings;

      MAX_PATH    : constant := 1024;
      Server_Path : Interfaces.C.char_array (1 .. MAX_PATH);

      function GetModuleFileName
        (hInst        : GWindows.Types.Handle :=
           GWindows.Internal.Current_hInstance;
         lpszFileName : access Interfaces.C.char := Server_Path (1)'Access;
         cbFileName   : in     Integer           := MAX_PATH)
        return Integer;
      pragma Import (StdCall, GetModuleFileName, "GetModuleFileNameA");
   begin
      if GetModuleFileName < 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            "GetModuleFileName failed");
      end if;

      declare
         Tmp   : constant String := Interfaces.C.To_Ada (Server_Path);
         Value : constant String := Tmp (1 .. Index (Tmp, "\", Backward) - 1);
      begin
         return GWindows.GStrings.To_GString_From_String (Value);
      end;
   end Current_Directory;

   ------------------------------
   -- Get_Short_Directory_Name --
   ------------------------------

   function Get_Short_Directory_Name
     (Long_Directory_Name : GString)
      return GString
   is
      Server_Path : GString_C :=
        GWindows.GStrings.To_GString_C (Long_Directory_Name);

      procedure GetShortPathName
        (lpszLongPath  : access GChar_C :=
           Server_Path (Server_Path'First)'Access;
         lpszShortPath : access GChar_C :=
           Server_Path (Server_Path'First)'Access;
         cchBuffer     : Integer   := Server_Path'Length);
      pragma Import (StdCall, GetShortPathName,
                       "GetShortPathName" & Character_Mode_Identifier);
   begin
      GetShortPathName;
      return GWindows.GStrings.To_GString_From_C (Server_Path);
   end Get_Short_Directory_Name;

   subtype LSTATUS is Interfaces.C.long;
   subtype HKEY_T is GWindows.Types.Handle;

   procedure RegCloseKey (HKEY : HKEY_T);
   pragma Import (StdCall, RegCloseKey, "RegCloseKey");

   --------------
   -- Register --
   --------------

   procedure Register
     (Key_Name, Name, Value : in GString;
      Root_Key              : in Integer)
   is
      use type Interfaces.C.long, GWindows.Types.DWORD;

      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      Name_Name  : GString_C := GWindows.GStrings.To_GString_C (Name);
      Value_Data : GString_C := GWindows.GStrings.To_GString_C (Value);
      Key        : aliased HKEY_T;
      REG_SZ     : constant := 1;

      function RegCreateKey
        (hKey      : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey  : access GChar_C := C_Key (C_Key'First)'Access;
         phkResult : access HKEY_T  := Key'Access)
        return LSTATUS;
      pragma Import (StdCall, RegCreateKey,
                       "RegCreateKey" & Character_Mode_Identifier);

      function RegSetValueEx
        (hKey        : in     HKEY_T               := Key;
         lpValueName : access GChar_C              :=
           Name_Name (Name_Name'First)'Access;
         reserved    : in     GWindows.Types.DWORD := 0;
         dwType      : in     GWindows.Types.DWORD := REG_SZ;
         lpData      : access GChar_C              :=
           Value_Data (Value_Data'First)'Access;
         cbData      : in     GWindows.Types.DWORD :=
           Value_Data'Length * (GCharacter'Size / 8))
        return LSTATUS;
      pragma Import (StdCall, RegSetValueEx,
                       "RegSetValueEx" & Character_Mode_Identifier);

      Result : LSTATUS;
   begin
      Result := RegCreateKey;

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

      Result := RegSetValueEx;
      RegCloseKey (Key);

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Key_Name : in GString;
      Root_Key : in Integer)
   is
      C_Key : GString_C := GWindows.GStrings.To_GString_C (Key_Name);

      function RegDeleteKey
        (hKey     : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey : access GChar_C := C_Key (C_Key'First)'Access)
        return LSTATUS;
      pragma Import (StdCall, RegDeleteKey,
                       "RegDeleteKey" & Character_Mode_Identifier);
      Result : LSTATUS;
      use type LSTATUS;
   begin
      Result := RegDeleteKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;
   end Unregister;

   ------------------
   -- Delete_Value --
   ------------------

   procedure Delete_Value (Key_Name, Name : in GString;
                           Root_Key       : in Integer)
   is
      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      Name_Name  : GString_C := GWindows.GStrings.To_GString_C (Name);
      Key        : aliased HKEY_T;

      function RegCreateKey
        (hKey      : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey  : access GChar_C := C_Key (C_Key'First)'Access;
         phkResult : access HKEY_T  := Key'Access)
        return LSTATUS;
      pragma Import (StdCall, RegCreateKey,
                       "RegCreateKey" & Character_Mode_Identifier);

      function RegDeleteValue
        (hKey   : in     HKEY_T  := Key;
         D_Name : access GChar_C :=
           Name_Name (Name_Name'First)'Access)
        return LSTATUS;
      pragma Import (StdCall, RegDeleteValue,
                       "RegDeleteValue" & Character_Mode_Identifier);

      Result : LSTATUS;
      use type LSTATUS;
   begin
      Result := RegCreateKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

      Result := RegDeleteValue;
      RegCloseKey (Key);

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

   end Delete_Value;

   ---------------------
   -- Get_Value_Names --
   ---------------------

   function Get_Value_Names (Key_Name : in GString;
                             Root_Key : in Integer)
                            return Value_Name_Array
   is
      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      Key        : aliased HKEY_T;
      Num_Vals   : aliased GWindows.Types.DWORD := 0;
      Max_Name   : aliased GWindows.Types.DWORD := 0;

      function RegOpenKey
        (hKey      : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey  : access GChar_C := C_Key (C_Key'First)'Access;
         phkResult : access HKEY_T  := Key'Access)
        return LSTATUS;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY             :        HKEY_T  := Key;
         A, B, C, D, E, F : access GWindows.Types.DWORD := null;
         NVALS            : access GWindows.Types.DWORD := Num_Vals'Access;
         MNAME            : access GWindows.Types.DWORD := Max_Name'Access;
         G, H, I          : access GWindows.Types.DWORD := null);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : LSTATUS;
      use type LSTATUS;
   begin
      Result := RegOpenKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

      RegQueryInfoKey;

      declare
         Name_Buf  : GString_C (0 .. Interfaces.C.size_t (Max_Name));

         Tmp_Max    : aliased GWindows.Types.DWORD;

         procedure RegEnumValue
           (HKEY       :        HKEY_T               := Key;
            dwIndex    :        GWindows.Types.DWORD;
            Name       : access GChar_C              :=
              Name_Buf (Name_Buf'First)'Access;
            MNAME      : access GWindows.Types.DWORD := Tmp_Max'Access;
            A, B, C, D : access Integer              := null);
         pragma Import (StdCall, RegEnumValue,
                          "RegEnumValue" & Character_Mode_Identifier);

         Results : Value_Name_Array (1 .. Integer (Num_Vals));
         use type GWindows.Types.DWORD;
      begin
         for N in 1 .. Num_Vals loop
            Tmp_Max := Max_Name + 1;
            RegEnumValue (dwIndex => N - 1);
            Results (Integer (N)) :=
              GWindows.GStrings.To_GString_Unbounded
              (GWindows.GStrings.To_GString_From_C (Name_Buf));
         end loop;

         RegCloseKey (Key);
         return Results;
      end;

   end Get_Value_Names;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Key_Name, Name : in GString;
                       Root_Key       : in Integer)
                      return GString
   is
      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      C_Name     : GString_C := GWindows.GStrings.To_GString_C (Name);
      Key        : aliased HKEY_T;
      Max_Value  : aliased GWindows.Types.DWORD := 0;

      function RegOpenKey
        (hKey      : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey  : access GChar_C := C_Key (C_Key'First)'Access;
         phkResult : access HKEY_T  := Key'Access)
        return LSTATUS;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY         :        HKEY_T               := Key;
         A, B, C, D,
         E, F, G, H   : access GWindows.Types.DWORD := null;
         MVAL         : access GWindows.Types.DWORD := Max_Value'Access;
         I, J         : access GWindows.Types.DWORD := null);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : LSTATUS;
      use type GWindows.Types.DWORD, LSTATUS;
   begin
      Result := RegOpenKey;

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

      RegQueryInfoKey;

      Max_Value := Max_Value + 1;

      declare
         C_Result : GString_C (0 .. Interfaces.C.size_t (Max_Value));

         procedure RegQueryValueEx
           (HKEY  : in     HKEY_T               := Key;
            Name  : access GChar_C              :=
              C_Name (C_Name'First)'Access;
            A     : access GWindows.Types.DWORD := null;
            B     : access GWindows.Types.DWORD := null;
            Value : access GChar_C              := C_Result (0)'Access;
            Max   : access GWindows.Types.DWORD := Max_Value'Access);
         pragma Import (StdCall, RegQueryValueEx,
                          "RegQueryValueEx" & Character_Mode_Identifier);
      begin
         RegQueryValueEx;
         RegCloseKey (Key);

         return GWindows.GStrings.To_GString_From_C (C_Result);
      end;
   end Get_Value;

   ------------------
   -- Get_Sub_Keys --
   ------------------

   function Get_Sub_Keys (Key_Name : in GString;
                          Root_Key : in Integer)
                         return Key_Name_Array
   is
      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      Key        : aliased HKEY_T;
      Num_Keys   : aliased GWindows.Types.DWORD := 0;
      Max_Name   : aliased GWindows.Types.DWORD := 0;

      function RegOpenKey
        (hKey      : in     HKEY_T  := GWindows.Types.To_Handle (Root_Key);
         lpSubKey  : access GChar_C := C_Key (C_Key'First)'Access;
         phkResult : access HKEY_T  := Key'Access)
        return LSTATUS;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY             :        HKEY_T               := Key;
         A, B, C          : access GWindows.Types.DWORD := null;
         NKEYS            : access GWindows.Types.DWORD := Num_Keys'Access;
         MNAME            : access GWindows.Types.DWORD := Max_Name'Access;
         D, E, F, G, H, I : access GWindows.Types.DWORD := null);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : LSTATUS;
      use type Interfaces.C.long;
   begin
      Result := RegOpenKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Integer (Result))));
      end if;

      RegQueryInfoKey;

      declare
         Name_Buf  : GString_C (0 .. Interfaces.C.size_t (Max_Name));

         Tmp_Max    : GWindows.Types.DWORD;

         procedure RegEnumKey
           (HKEY    :        HKEY_T               := Key;
            dwIndex :        GWindows.Types.DWORD;
            lpName  : access GChar_C              := Name_Buf (0)'Access;
            cchName :        GWindows.Types.DWORD := Tmp_Max);
         pragma Import (StdCall, RegEnumKey,
                          "RegEnumKey" & Character_Mode_Identifier);

         Results : Key_Name_Array (1 .. Integer (Num_Keys));
         use type GWindows.Types.DWORD;
      begin
         for N in 1 .. Num_Keys loop
            Tmp_Max := Max_Name + 1;
            RegEnumKey (dwIndex => N - 1);
            Results (Integer (N)) :=
              GWindows.GStrings.To_GString_Unbounded
              (GWindows.GStrings.To_GString_From_C (Name_Buf));
         end loop;

         RegCloseKey (Key);

         return Results;
      end;
   end Get_Sub_Keys;

end GWindows.Registry;
