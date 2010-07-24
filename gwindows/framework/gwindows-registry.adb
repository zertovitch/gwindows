------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                   G W I N D O W S . R E G I S T R Y                      --
--                                                                          --
--                                 B o d y                                  --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
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
   --  Operating System Imports
   -------------------------------------------------------------------------

   procedure RegCloseKey (HKEY : Interfaces.C.long);
   pragma Import (StdCall, RegCloseKey, "RegCloseKey");

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

   --------------
   -- Register --
   --------------

   procedure Register
     (Key_Name, Name, Value : in GString;
      Root_Key              : in Integer)
   is
      use type Interfaces.C.unsigned_long;

      C_Key      : GString_C := GWindows.GStrings.To_GString_C (Key_Name);
      Name_Name  : GString_C := GWindows.GStrings.To_GString_C (Name);
      Value_Data : GString_C := GWindows.GStrings.To_GString_C (Value);
      Key        : aliased Interfaces.C.long;
      REG_SZ     : constant := 1;

      function RegCreateKey
        (hKey      : in     Integer           := Root_Key;
         lpSubKey  : access GChar_C           := C_Key (C_Key'First)'Access;
         phkResult : access Interfaces.C.long := Key'Access)
        return Integer;
      pragma Import (StdCall, RegCreateKey,
                       "RegCreateKey" & Character_Mode_Identifier);

      function RegSetValueEx
        (hKey        : in     Interfaces.C.long          := Key;
         lpValueName : access GChar_C                    :=
           Name_Name (Name_Name'First)'Access;
         reserved    : in     Interfaces.C.unsigned_long := 0;
         dwType      : in     Integer                    := REG_SZ;
         lpData      : access GChar_C                    :=
           Value_Data (Value_Data'First)'Access;
         cbData      : in     Interfaces.C.unsigned_long :=
           Value_Data'Length * (GCharacter'Size / 8))
        return Integer;
      pragma Import (StdCall, RegSetValueEx,
                       "RegSetValueEx" & Character_Mode_Identifier);

      Result : Integer;
   begin
      Result := RegCreateKey;

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
      end if;

      Result := RegSetValueEx;
      RegCloseKey (Key);

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
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
        (hKey     : in     Integer := Root_Key;
         lpSubKey : access GChar_C := C_Key (C_Key'First)'Access)
        return Integer;
      pragma Import (StdCall, RegDeleteKey,
                       "RegDeleteKey" & Character_Mode_Identifier);
      Result : Integer;
   begin
      Result := RegDeleteKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
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
      Key        : aliased Interfaces.C.long;

      function RegCreateKey
        (hKey      : in     Integer           := Root_Key;
         lpSubKey  : access GChar_C           := C_Key (C_Key'First)'Access;
         phkResult : access Interfaces.C.long := Key'Access)
        return Integer;
      pragma Import (StdCall, RegCreateKey,
                       "RegCreateKey" & Character_Mode_Identifier);

      function RegDeleteValue
        (hKey   : in     Interfaces.C.long := Key;
         D_Name : access GChar_C           :=
           Name_Name (Name_Name'First)'Access)
        return Integer;
      pragma Import (StdCall, RegDeleteValue,
                       "RegDeleteValue" & Character_Mode_Identifier);

      Result : Integer;
   begin
      Result := RegCreateKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
      end if;

      Result := RegDeleteValue;
      RegCloseKey (Key);

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
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
      Key        : aliased Interfaces.C.long;
      Num_Vals   : aliased Integer := 0;
      Max_Name   : aliased Integer := 0;

      function RegOpenKey
        (hKey      : in     Integer           := Root_Key;
         lpSubKey  : access GChar_C           := C_Key (C_Key'First)'Access;
         phkResult : access Interfaces.C.long := Key'Access)
        return Integer;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY             :        Interfaces.C.long := Key;
         A, B, C, D, E, F :        Integer           := 0;
         NVALS            : access Integer           := Num_Vals'Access;
         MNAME            : access Integer           := Max_Name'Access;
         G, H, I          :        Integer           := 0);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : Integer;
   begin
      Result := RegOpenKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
      end if;

      RegQueryInfoKey;

      declare
         Name_Buf  : GString_C (0 .. Interfaces.C.size_t (Max_Name));

         Tmp_Max    : aliased Integer;

         procedure RegEnumValue
           (HKEY       :        Interfaces.C.long := Key;
            Index      :        Integer;
            Name       : access GChar_C           :=
              Name_Buf (Name_Buf'First)'Access;
            MNAME      : access Integer           := Tmp_Max'Access;
            A, B, C, D :        Integer           := 0);
         pragma Import (StdCall, RegEnumValue,
                          "RegEnumValue" & Character_Mode_Identifier);

         Results : Value_Name_Array (1 .. Num_Vals);
      begin
         for N in 1 .. Num_Vals loop
            Tmp_Max := Max_Name + 1;
            RegEnumValue (Index => N - 1);
            Results (N) :=
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
      Key        : aliased Interfaces.C.long;
      Max_Value  : aliased Integer := 0;

      function RegOpenKey
        (hKey      : in     Integer           := Root_Key;
         lpSubKey  : access GChar_C           := C_Key (C_Key'First)'Access;
         phkResult : access Interfaces.C.long := Key'Access)
        return Integer;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY                   :        Interfaces.C.long := Key;
         A, B, C, D, E, F, G, H :        Integer           := 0;
         MVAL                   : access Integer           := Max_Value'Access;
         I, J                   :        Integer           := 0);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : Integer;
   begin
      Result := RegOpenKey;

      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
      end if;

      RegQueryInfoKey;

      Max_Value := Max_Value + 1;

      declare
         C_Result : GString_C (0 .. Interfaces.C.size_t (Max_Value));

         procedure RegQueryValueEx
           (HKEY  : in     Interfaces.C.long := Key;
            Name  : access GChar_C           := C_Name (C_Name'First)'Access;
            A     : in     Integer           := 0;
            B     : in     Integer           := 0;
            Value : access GChar_C           := C_Result (0)'Access;
            Max   : access Integer           := Max_Value'Access);
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
      Key        : aliased Interfaces.C.long;
      Num_Keys   : aliased Integer := 0;
      Max_Name   : aliased Integer := 0;

      function RegOpenKey
        (hKey      : in     Integer           := Root_Key;
         lpSubKey  : access GChar_C           := C_Key (C_Key'First)'Access;
         phkResult : access Interfaces.C.long := Key'Access)
        return Integer;
      pragma Import (StdCall, RegOpenKey,
                       "RegOpenKey" & Character_Mode_Identifier);

      procedure RegQueryInfoKey
        (HKEY             :        Interfaces.C.long := Key;
         A, B, C          :        Integer           := 0;
         NKEYS            : access Integer           := Num_Keys'Access;
         MNAME            : access Integer           := Max_Name'Access;
         D, E, F, G, H, I :        Integer           := 0);
      pragma Import (StdCall, RegQueryInfoKey,
                       "RegQueryInfoKey" & Character_Mode_Identifier);

      Result : Integer;
   begin
      Result := RegOpenKey;
      if Result /= 0 then
         Ada.Exceptions.Raise_Exception
           (REGISTRY_ERROR'Identity,
            GWindows.GStrings.To_String
            (GWindows.Errors.Error_To_String (Result)));
      end if;

      RegQueryInfoKey;

      declare
         Name_Buf  : GString_C (0 .. Interfaces.C.size_t (Max_Name));

         Tmp_Max    : aliased Integer;

         procedure RegEnumKey
           (HKEY  :        Interfaces.C.long := Key;
            Index :        Integer;
            Name  : access GChar_C           := Name_Buf (0)'Access;
            MNAME : access Integer           := Tmp_Max'Access);
         pragma Import (StdCall, RegEnumKey,
                          "RegEnumKey" & Character_Mode_Identifier);

         Results   : Key_Name_Array (1 .. Num_Keys);
      begin
         for N in 1 .. Num_Keys loop
            Tmp_Max := Max_Name + 1;
            RegEnumKey (Index => N - 1);
            Results (N) :=
              GWindows.GStrings.To_GString_Unbounded
              (GWindows.GStrings.To_GString_From_C (Name_Buf));
         end loop;

         RegCloseKey (Key);

         return Results;
      end;
   end Get_Sub_Keys;

end GWindows.Registry;
