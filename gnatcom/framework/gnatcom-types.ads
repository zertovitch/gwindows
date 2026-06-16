------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . T Y P E S                         --
--                                                                          --
--                                S p e c                                   --
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

--  Thin bindings to COM Types

with Interfaces.C;
with System;
with Win32_Types;

package GNATCOM.Types is

   package C renames Interfaces.C;

   type HRESULT is new Win32_Types.Unsigned_Long;
   type Pointer_To_HRESULT is access all HRESULT;
   subtype SCODE is HRESULT;
   type Pointer_To_SCODE is access all SCODE;
   --  HRESULTs are used to return the success or failure of Interface
   --  methods and COM APIs. See GNATCOM.Errors

   Size_Of_Pointers : constant := Standard'Address_Size;

   type Void is null record;
   subtype Pointer_To_Void is System.Address;
   type Pointer_To_Pointer_To_Void is access all Pointer_To_Void;
   pragma No_Strict_Aliasing (Pointer_To_Pointer_To_Void);
   --  C style Void pointers

   type Pointer_To_char is access all Interfaces.C.char;
   type Pointer_To_Pointer_To_char is access all Interfaces.C.char;
   type Pointer_To_wchar_t is access all Win32_Types.wchar_t;
   subtype LPSTR is Pointer_To_char;
   type Pointer_To_LPSTR is access all LPSTR;
   subtype LPWSTR is Pointer_To_wchar_t;
   pragma No_Strict_Aliasing (LPWSTR);
   type Pointer_To_LPWSTR is access all LPWSTR;
   --  C style strings

   function To_C (From : access Win32_Types.wchar_t)
     return Win32_Types.wchar_array;
   function To_Ada (From : access Win32_Types.wchar_t) return String;
   function To_Ada (From : access Win32_Types.wchar_t) return Wide_String;
   --  Convert pointers to C wide strings to usable types

   Size_Of_Currency : constant := 64;

   type CURRENCY is
      record
         Lo : Win32_Types.Unsigned_Long;
         Hi : Win32_Types.Long;
      end record;
   for CURRENCY'Size use Size_Of_Currency;
   type Pointer_To_CURRENCY is access all CURRENCY;
   --  Automation CURRENCY type. It is an 8 byte, two's complement integer
   --  scaled by 10,000 to give a fixed-point number with 15 digits to the
   --  left and 4 to the righ of the decimal point

   type DATE is new Interfaces.C.double;
   type Pointer_To_DATE is access all DATE;
   --  Automation DATE type implemented using an 8-byte floating-point number.
   --  Days are represneted by whole number increments starting from
   --  December 30, 1899

   VARIANT_BOOL_TRUE  : constant := -1;
   VARIANT_BOOL_FALSE : constant := 0;
   type VARIANT_BOOL is new Interfaces.C.short;
   type Pointer_To_VARIANT_BOOL is access all VARIANT_BOOL;
   --  All bits on = True
   --  All bits off = False
   --  All other values are invalid

   --  COM Numeric Types

   Size_Of_LONGLONG : constant := 64;

   type LONGLONG is record
      Lo : Interfaces.C.int;
      Hi : Interfaces.C.int;
   end record;
   for LONGLONG'Size use Size_Of_LONGLONG;
   pragma Convention (C_Pass_By_Copy, LONGLONG);
   type Pointer_To_LONGLONG is access all LONGLONG;

   type DWORD is new Interfaces.C.unsigned;
   type Pointer_To_DWORD is access all DWORD;

   type ULONG is new Win32_Types.Unsigned_Long;
   type Pointer_To_ULONG is access all ULONG;

   type LONG is new Win32_Types.Long;
   type PSECURITY_DESCRIPTOR is new Pointer_To_Void;
   type SOLE_AUTHENTICATION_SERVICE is null record;

   Size_Of_DWORDLONG : constant := 64;

   type DWORDLONG is record
      Lo : Interfaces.C.unsigned;
      Hi : Interfaces.C.unsigned;
   end record;
   for DWORDLONG'Size use Size_Of_DWORDLONG;
   pragma Convention (C_Pass_By_Copy, DWORDLONG);
   type Pointer_To_DWORDLONG is access all DWORDLONG;

   type BYTE is new Interfaces.C.unsigned_char;
   type Pointer_To_BYTE is access all BYTE;

   bool_TRUE  : constant := 1;
   bool_FALSE : constant := 0;
   subtype bool is Win32_Types.Long;
   type Pointer_To_bool is access all bool;

   type DECIMAL is
      record
         wReserved : Interfaces.C.unsigned_short;
         scale     : BYTE;
         sign      : BYTE;
         Hi32      : Win32_Types.Unsigned_Long;
         Lo32      : Win32_Types.Unsigned_Long;
         Mid32     : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, DECIMAL);
   type Pointer_To_DECIMAL is access all DECIMAL;

   Size_Of_BLOB : constant := 2 * Standard'Address_Size;

   type BLOB is
      record
         cbSize    : Win32_Types.Unsigned_Long;
         pBlobData : Pointer_To_BYTE;
      end record;
   pragma Convention (C_Pass_By_Copy, BLOB);
   for BLOB'Size use Size_Of_BLOB;
   type Pointer_To_BLOB is access all BLOB;
   --  Automation type - (B)inary (L)arge (OB)ject

   type SAFEARRAYBOUND is
      record
         cElements : Win32_Types.Unsigned_Long;
         lLbound   : Win32_Types.Long;
      end record;
   pragma Convention (C_Pass_By_Copy, SAFEARRAYBOUND);
   type Pointer_To_SAFEARRAYBOUND is access all SAFEARRAYBOUND;

   type SAFEARRAY is
      record
         cDims      : Interfaces.C.short;
         fFeatures  : Interfaces.C.short;
         cbElements : Win32_Types.Unsigned_Long;
         cLocks     : Win32_Types.Unsigned_Long;
         pvData     : Pointer_To_Void;
         rgsabound  : SAFEARRAYBOUND;
      end record;
   pragma Convention (C_Pass_By_Copy, SAFEARRAY);
   type Pointer_To_SAFEARRAY is access all SAFEARRAY;
   type Pointer_To_Pointer_To_SAFEARRAY is access all Pointer_To_SAFEARRAY;
   --  Automation array type requires use of SAFEARRAY APIs

   Size_Of_GUID : constant := 128;

   type GUID_Data4_Array is
     array (Integer range  0 .. 7) of Interfaces.C.unsigned_char;
   type GUID is
      record
         Data1 : Win32_Types.Unsigned_Long;
         Data2 : Interfaces.C.unsigned_short;
         Data3 : Interfaces.C.unsigned_short;
         Data4 : GUID_Data4_Array;
      end record;
   pragma Convention (C_Pass_By_Copy, GUID);
   for GUID'Size use Size_Of_GUID;

   type Pointer_To_GUID is access all GUID;
   --  GUID - Globaly Unique Identifier
   --  GUIDs are used through in COM to uniquely identify types, interfaces,
   --  objects, and libraries. See GNATCOM.GUID

   type GUID_Array is array (Positive range <>) of GUID;
   type GUID_Array_Pointer is access all GUID_Array;

   GUID_NULL : aliased GUID :=
     (0, 0, 0,
       (C.unsigned_char'Val (0), C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0)));

   subtype BSTR is Pointer_To_wchar_t;
   type Pointer_To_BSTR is access all BSTR;
   --  Basic (or Binary) strings
   --  The word before the pointer contains the length of the string
   --  creation and manipulation of the string requires use of the BSTR
   --  APIs or GNATCOM.BSTR

   type IUnknown;
   type IDispatch;
   type VARIANT;
   type ITypeLib;
   type ITypeInfo;
   type ITypeComp;
   type ICreateTypeInfo;
   type ICreateTypeLib;
   type IProvideClassInfo;
   type IProvideClassInfo2;
   type IClassFactory;
   type IClassFactory2;
   type IConnectionPointContainer;
   type IConnectionPoint;
   type IEnumConnectionPoints;
   type IEnumConnections;
   type IEnumVARIANT;
   type IGlobalInterfaceTable;
   type ICatRegister;
   type IPropertyNotifySink;
   type IServiceProvider;
   --  Forward references to COM Interfaces and types

   type Pointer_To_int is access all Interfaces.C.int;
   type Pointer_To_short is access all Interfaces.C.short;
   type Pointer_To_long is access all Win32_Types.Long;
   type Pointer_To_double is access all Interfaces.C.double;
   type Pointer_To_C_float is access all Interfaces.C.C_float;
   type Pointer_To_unsigned is access all Interfaces.C.unsigned;
   type Pointer_To_unsigned_long is access all Win32_Types.Unsigned_Long;
   type Pointer_To_unsigned_short is access all Interfaces.C.unsigned_short;
   type Pointer_To_IUnknown is access all IUnknown;
   pragma No_Strict_Aliasing (Pointer_To_IUnknown);
   type Pointer_To_IDispatch is access all IDispatch;
   pragma No_Strict_Aliasing (Pointer_To_IDispatch);
   type Pointer_To_Pointer_To_IUnknown is access all Pointer_To_IUnknown;
   type Pointer_To_Pointer_To_IDispatch is access all Pointer_To_IDispatch;
   type Pointer_To_VARIANT is access all VARIANT;
   type Pointer_To_ITypeLib is access all ITypeLib;
   type Pointer_To_Pointer_To_ITypeLib is access all Pointer_To_ITypeLib;
   type Pointer_To_ITypeInfo is access all ITypeInfo;
   pragma No_Strict_Aliasing (Pointer_To_ITypeInfo);
   type Pointer_To_Pointer_To_ITypeInfo is access all Pointer_To_ITypeInfo;
   type Pointer_To_ITypeComp is access all ITypeComp;
   type Pointer_To_Pointer_To_ITypeComp is access all Pointer_To_ITypeComp;
   type Pointer_To_ICreateTypeLib is access all ICreateTypeLib;
   pragma No_Strict_Aliasing (Pointer_To_ICreateTypeLib);
   type Pointer_To_Pointer_To_ICreateTypeLib is
     access all Pointer_To_ICreateTypeLib;
   type Pointer_To_ICreateTypeInfo is access all ICreateTypeInfo;
   pragma No_Strict_Aliasing (Pointer_To_ICreateTypeInfo);
   type Pointer_To_Pointer_To_ICreateTypeInfo is
     access all Pointer_To_ICreateTypeInfo;
   type Pointer_To_IClassFactory is access all IClassFactory;
   type Pointer_To_IClassFactory2 is access all IClassFactory2;
   pragma No_Strict_Aliasing (Pointer_To_IClassFactory2);
   type Pointer_To_IConnectionPointContainer is
     access all IConnectionPointContainer;
   type Pointer_To_Pointer_To_IConnectionPointContainer is
      access all Pointer_To_IConnectionPointContainer;
   pragma No_Strict_Aliasing (Pointer_To_IConnectionPointContainer);
   type Pointer_To_IConnectionPoint is access all IConnectionPoint;
   pragma No_Strict_Aliasing (Pointer_To_IConnectionPoint);
   type Pointer_To_Pointer_To_IConnectionPoint is
     access all Pointer_To_IConnectionPoint;
   type Pointer_To_IEnumConnectionPoints is access all IEnumConnectionPoints;
   type Pointer_To_Pointer_To_IEnumConnectionPoints is
     access all Pointer_To_IEnumConnectionPoints;
   type Pointer_To_IEnumConnections is access all IEnumConnections;
   type Pointer_To_Pointer_To_IEnumConnections is
     access all Pointer_To_IEnumConnections;
   type Pointer_To_IEnumVARIANT is access all IEnumVARIANT;
   pragma No_Strict_Aliasing (Pointer_To_IEnumVARIANT);
   type Pointer_To_Pointer_To_IEnumVARIANT is
     access all Pointer_To_IEnumVARIANT;
   type Pointer_To_IGlobalInterfaceTable is access all IGlobalInterfaceTable;
   pragma No_Strict_Aliasing (Pointer_To_IGlobalInterfaceTable);
   type Pointer_To_ICatRegister is access all ICatRegister;
   type Pointer_To_Pointer_To_ICatRegister is
     access all Pointer_To_ICatRegister;
   type Pointer_To_IPropertyNotifySink is access all IPropertyNotifySink;
   pragma No_Strict_Aliasing (Pointer_To_IPropertyNotifySink);
   type Pointer_To_Pointer_To_IPropertyNotifySink is
     access all Pointer_To_IPropertyNotifySink;
   type Pointer_To_IServiceProvider is access all IServiceProvider;
   --  Pointer types used by COM

   VT_EMPTY           : constant := 0;
   VT_NULL            : constant := 1;
   VT_I2              : constant := 2;
   VT_I4              : constant := 3;
   VT_R4              : constant := 4;
   VT_R8              : constant := 5;
   VT_CY              : constant := 6;
   VT_DATE            : constant := 7;
   VT_BSTR            : constant := 8;
   VT_DISPATCH        : constant := 9;
   VT_ERROR           : constant := 10;
   VT_BOOL            : constant := 11;
   VT_VARIANT         : constant := 12;
   VT_UNKNOWN         : constant := 13;
   VT_DECIMAL         : constant := 14;
   VT_I1              : constant := 16;
   VT_UI1             : constant := 17;
   VT_UI2             : constant := 18;
   VT_UI4             : constant := 19;
   VT_I8              : constant := 20;
   VT_UI8             : constant := 21;
   VT_INT             : constant := 22;
   VT_UINT            : constant := 23;
   VT_VOID            : constant := 24;
   VT_HRESULT         : constant := 25;
   VT_PTR             : constant := 26;
   VT_SAFEARRAY       : constant := 27;
   VT_CARRAY          : constant := 28;
   VT_USERDEFINED     : constant := 29;
   VT_LPSTR           : constant := 30;
   VT_LPWSTR          : constant := 31;
   VT_RECORD          : constant := 36;
   VT_FILETIME        : constant := 64;
   VT_BLOB            : constant := 65;
   VT_STREAM          : constant := 66;
   VT_STORAGE         : constant := 67;
   VT_STREAMED_OBJECT : constant := 68;
   VT_STORED_OBJECT   : constant := 69;
   VT_BLOB_OBJECT     : constant := 70;
   VT_CF              : constant := 71;
   VT_CLSID           : constant := 72;
   VT_BSTR_BLOB       : constant := 4095;
   VT_VECTOR          : constant := 4096;
   VT_ARRAY           : constant := 8192;
   VT_BYREF           : constant := 16384;
   VT_RESERVED        : constant := 32768;
   VT_ILLEGAL         : constant := 65535;
   VT_ILLEGALMASKED   : constant := 4095;
   VT_TYPEMASK        : constant := 4095;
   --  Variant Type Constants used to indetify the contents of a variant
   subtype VARTYPE is Interfaces.C.unsigned_short;
   type Pointer_To_VARTYPE is access all VARTYPE;

   subtype Variant_Range is Positive range 1 .. 41;
   --  NOTE: The Which values are completely bogus and have no significance
   --        The type of the variant can not be determined using it
   --        The values bellow do NOT correspond to correct variant type
   --        constants!
   type Variant_Union (Which : Variant_Range := 1) is
      record
         case Which is
            when 1 =>
               lVal      : Win32_Types.Long;
            when 2 =>
               bVal      : Interfaces.C.unsigned_char;
            when 3 =>
               iVal      : Interfaces.C.short;
            when 4 =>
               fltVal    : Interfaces.C.C_float;
            when 5 =>
               dblVal    : Interfaces.C.double;
            when 6 =>
               boolVal   : VARIANT_BOOL;
            when 7 =>
               bool      : VARIANT_BOOL;
            when 8 =>
               scode     : Types.SCODE;
            when 9 =>
               cyVal     : CURRENCY;
            when 10 =>
               date      : Types.DATE;
            when 11 =>
               bstrVal   : BSTR;
            when 12 =>
               punkVal   : Pointer_To_IUnknown;
            when 13 =>
               pdispVal  : Pointer_To_IDispatch;
            when 14 =>
               parray    : Pointer_To_SAFEARRAY;
            when 15 =>
               pbVal     : Pointer_To_BYTE;
            when 16 =>
               piVal     : Pointer_To_short;
            when 17 =>
               plVal     : Pointer_To_long;
            when 18 =>
               pfltVal   : Pointer_To_C_float;
            when 19 =>
               pdblVal   : Pointer_To_double;
            when 20 =>
               pboolVal  : Pointer_To_VARIANT_BOOL;
            when 21 =>
               pbool     : Pointer_To_VARIANT_BOOL;
            when 22 =>
               pscode    : Pointer_To_SCODE;
            when 23 =>
               pcyVal    : Pointer_To_CURRENCY;
            when 24 =>
               pdate     : Pointer_To_DATE;
            when 25 =>
               pbstrVal  : Pointer_To_BSTR;
            when 26 =>
               ppunkVal  : Pointer_To_Pointer_To_IUnknown;
            when 27 =>
               ppdispVal : Pointer_To_Pointer_To_IDispatch;
            when 28 =>
               pparray   : Pointer_To_Pointer_To_SAFEARRAY;
            when 29 =>
               pvarVal   : Pointer_To_VARIANT;
            when 30 =>
               byref     : Pointer_To_Void;
            when 31 =>
               cVal      : Interfaces.C.char;
            when 32 =>
               uiVal     : Interfaces.C.unsigned_short;
            when 33 =>
               ulVal     : Win32_Types.Unsigned_Long;
            when 34 =>
               intVal    : Interfaces.C.int;
            when 35 =>
               uintVal   : Interfaces.C.unsigned;
            when 36 =>
               pdecVal   : Pointer_To_DECIMAL;
            when 37 =>
               pcVal     : Pointer_To_char;
            when 38 =>
               puiVal    : Pointer_To_unsigned_short;
            when 39 =>
               pulVal    : Pointer_To_unsigned_long;
            when 40 =>
               pintVal   : Pointer_To_int;
            when 41 =>
               puintVal  : Pointer_To_unsigned;
               puintVal2 : Pointer_To_unsigned;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, Variant_Union);
   pragma Unchecked_Union (Variant_Union);

   subtype LCID is DWORD;
   subtype OLECHAR_Array is Win32_Types.wchar_array;

   type CATEGORYINFO is
      record
         catid         : GUID;
         lcid          : GNATCOM.Types.LCID;
         szDescription : OLECHAR_Array (0 .. 127);
      end record
     with Convention => C_Pass_By_Copy;

   type CATEGORYINFO_Array is array (Natural range <>) of aliased CATEGORYINFO;

   Size_Of_VARIANT : constant := 64 + 2 * Standard'Address_Size;

   type VARIANT is
      record
         vt         : VARTYPE;
         wReserved1 : Interfaces.C.unsigned_short;
         wReserved2 : Interfaces.C.unsigned_short;
         wReserved3 : Interfaces.C.unsigned_short;
         u          : Variant_Union;
      end record;
   for VARIANT'Size use Size_Of_VARIANT;
   pragma Convention (C_Pass_By_Copy, VARIANT);
   --  The automation type VARIANT is used to create a variable that
   --  can contain any of the OLEAUTOMATION types. The value of vt
   --  indicates the type currently held in the type union in u
   --  Variants should be manipulated using the Variant APIs or with
   --  GNATCOM.Variant

   VARIANT_MISSING : aliased constant VARIANT :=
     (VT_ERROR, 0, 0, 0, u => (Which => 8, scode => DISP_E_PARAMNOTFOUND));
   PVARIANT_MISSING : Pointer_To_VARIANT :=
      VARIANT_MISSING'Unrestricted_Access;

   VARIANT_TRUE : aliased constant VARIANT :=
     (VT_BOOL, 0, 0, 0, u => (Which => 6, boolVal => VARIANT_BOOL_TRUE));
   PVARIANT_TRUE : Pointer_To_VARIANT := VARIANT_TRUE'Unrestricted_Access;

   VARIANT_FALSE : aliased constant VARIANT :=
     (VT_BOOL, 0, 0, 0, u => (Which => 6, boolVal => VARIANT_BOOL_FALSE));
   PVARIANT_FALSE : Pointer_To_VARIANT := VARIANT_FALSE'Unrestricted_Access;

   VARIANT_NULL : aliased constant VARIANT :=
     (VT_NULL, 0, 0, 0, u => (Which => 1, lVal => 0));
   PVARIANT_NULL : Pointer_To_VARIANT := VARIANT_NULL'Unrestricted_Access;

   MAX_PARAMS : constant := 255;

   type BSTR_PARAM_ARRAY is array
     (Interfaces.C.short range 0 .. MAX_PARAMS) of
     aliased GNATCOM.Types.BSTR;
   pragma Convention (C, BSTR_PARAM_ARRAY);
   type Pointer_To_BSTR_PARAM_ARRAY is access all BSTR_PARAM_ARRAY;

   type VARIANT_PARAM_ARRAY is array
     (Interfaces.C.unsigned range 0 .. MAX_PARAMS) of
     aliased GNATCOM.Types.VARIANT;
   pragma Convention (C, VARIANT_PARAM_ARRAY);
   type Pointer_To_VARIANT_PARAM_ARRAY is access all VARIANT_PARAM_ARRAY;
   pragma No_Strict_Aliasing (Pointer_To_VARIANT_PARAM_ARRAY);

   type DISPID_PARAM_ARRAY is array
     (Interfaces.C.unsigned range 0 .. MAX_PARAMS) of
     aliased Win32_Types.Long;
   pragma Convention (C, DISPID_PARAM_ARRAY);
   type Pointer_To_DISPID_PARAM_ARRAY is access all DISPID_PARAM_ARRAY;
   pragma No_Strict_Aliasing (Pointer_To_DISPID_PARAM_ARRAY);

   --  Parameter helper arrays

   type DISPPARAMS is
      record
         rgvarg            : Pointer_To_VARIANT_PARAM_ARRAY;
         rgdispidNamedArgs : Pointer_To_DISPID_PARAM_ARRAY;
         cArgs             : Interfaces.C.unsigned;
         cNamedArgs        : Interfaces.C.unsigned;
      end record;
   pragma Convention (C_Pass_By_Copy, DISPPARAMS);
   type Pointer_To_DISPPARAMS is access all DISPPARAMS;
   --  Parameters to be passed in to an invocation of an IDispatch method

   type EXCEPINFO is
      record
         wCode             : Interfaces.C.short;
         wReserved         : Interfaces.C.short;
         bstrSource        : BSTR;
         bstrDescription   : BSTR;
         bstrHelpFile      : BSTR;
         dwHelpContext     : Win32_Types.Long;
         pvReserved        : Pointer_To_Void;
         pfnDeferredFillIn : Pointer_To_Void;
         scode             : Types.SCODE;
      end record;
   pragma Convention (C_Pass_By_Copy, EXCEPINFO);
   type Pointer_To_EXCEPINFO is access all EXCEPINFO;
   --  Used to return exception information with in an invocation of an
   --  IDispatch method

   --  COM Object related enums

   CLSCTX_INPROC_SERVER     : constant := 1;
   CLSCTX_INPROC_HANDLER    : constant := 2;
   CLSCTX_LOCAL_SERVER      : constant := 4;
   CLSCTX_INPROC_SERVER16   : constant := 8;
   CLSCTX_REMOTE_SERVER     : constant := 16;
   CLSCTX_INPROC_HANDLER16  : constant := 32;
   CLSCTX_INPROC_SERVERX86  : constant := 64;
   CLSCTX_INPROC_HANDLERX86 : constant := 128;
   CLSCTX_SERVER            : constant := 21;
   CLSCTX_ALL               : constant := 23;
   subtype CLSCTX is Win32_Types.Long;
   type Pointer_To_CLSCTX is access all CLSCTX;
   --  Used to specify what server type to use when creating the COM object

   --  IUnknown Interface
   --  {00000000-0000-0000-C000-000000000046}
   IID_IUnknown : aliased GUID :=
     (0, 0, 0,
       (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_IUnknown_QueryInterface is access
     function (This   : access IUnknown;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IUnknown_QueryInterface);
   pragma Machine_Attribute (af_IUnknown_QueryInterface, "ms_abi");

   type af_IUnknown_AddRef is access
     function (This : access IUnknown) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IUnknown_AddRef);
   pragma Machine_Attribute (af_IUnknown_AddRef, "ms_abi");

   type af_IUnknown_Release is access
     function (This : access IUnknown) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IUnknown_Release);
   pragma Machine_Attribute (af_IUnknown_Release, "ms_abi");

   type IUnknownVtbl;
   type Pointer_To_IUnknownVtbl is access all IUnknownVtbl;

   type IUnknown is
      record
         Vtbl : Pointer_To_IUnknownVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IUnknown);

   type IUnknownVtbl is
      record
         QueryInterface : af_IUnknown_QueryInterface;
         AddRef         : af_IUnknown_AddRef;
         Release        : af_IUnknown_Release;
      end record;
   pragma Convention (C_Pass_By_Copy, IUnknownVtbl);

   --  IDispatch Interface
   --  {00020400-0000-0000-C000-000000000046}
   IID_IDispatch : aliased GUID :=
     (132096, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_IDispatch_QueryInterface is access
     function (This   : access IDispatch;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_QueryInterface);
   pragma Machine_Attribute (af_IDispatch_QueryInterface, "ms_abi");

   type af_IDispatch_AddRef is access
     function (This : access IDispatch) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IDispatch_AddRef);
   pragma Machine_Attribute (af_IDispatch_AddRef, "ms_abi");

   type af_IDispatch_Release is access
     function (This : access IDispatch) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IDispatch_Release);
   pragma Machine_Attribute (af_IDispatch_Release, "ms_abi");

   type af_IDispatch_GetTypeInfoCount is access
     function (This    : access IDispatch;
               pctinfo : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetTypeInfoCount);
   pragma Machine_Attribute (af_IDispatch_GetTypeInfoCount, "ms_abi");

   type af_IDispatch_GetTypeInfo is access
     function (This    : access IDispatch;
               itinfo  : in     Interfaces.C.int;
               lcid    : in     Win32_Types.Long;
               pptinfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetTypeInfo);
   pragma Machine_Attribute (af_IDispatch_GetTypeInfo, "ms_abi");

   type af_IDispatch_GetIDsOfNames is access
     function (This      : access IDispatch;
               riid      : in     Pointer_To_GUID;
               rgszNames : in     Pointer_To_LPWSTR;
               cNames    : in     Interfaces.C.int;
               lcid      : in     Win32_Types.Long;
               rgdispid  : in     Pointer_To_long)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetIDsOfNames);
   pragma Machine_Attribute (af_IDispatch_GetIDsOfNames, "ms_abi");

   type af_IDispatch_Invoke is access
     function (This         : access IDispatch;
               dispidMember : in     Win32_Types.Long;
               riid         : in     Pointer_To_GUID;
               lcid         : in     Win32_Types.Long;
               wFlags       : in     Interfaces.C.short;
               pdispparams  : in     Pointer_To_DISPPARAMS;
               pvarResult   : in     Pointer_To_VARIANT;
               pexcepinfo   : in     Pointer_To_EXCEPINFO;
               puArgErr     : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_Invoke);
   pragma Machine_Attribute (af_IDispatch_Invoke, "ms_abi");

   type IDispatchVtbl;
   type Pointer_To_IDispatchVtbl is access all IDispatchVtbl;

   type IDispatch is
      record
         Vtbl : Pointer_To_IDispatchVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IDispatch);

   type IDispatchVtbl is
      record
         QueryInterface : af_IDispatch_QueryInterface;
         AddRef : af_IDispatch_AddRef;
         Release : af_IDispatch_Release;
         GetTypeInfoCount : af_IDispatch_GetTypeInfoCount;
         GetTypeInfo : af_IDispatch_GetTypeInfo;
         GetIDsOfNames : af_IDispatch_GetIDsOfNames;
         Invoke : af_IDispatch_Invoke;
      end record;
   pragma Convention (C_Pass_By_Copy, IDispatchVtbl);

   --  Type Library related types

   TKIND_ENUM      : constant := 0;
   TKIND_RECORD    : constant := 1;
   TKIND_MODULE    : constant := 2;
   TKIND_INTERFACE : constant := 3;
   TKIND_DISPATCH  : constant := 4;
   TKIND_COCLASS   : constant := 5;
   TKIND_ALIAS     : constant := 6;
   TKIND_UNION     : constant := 7;
   TKIND_MAX       : constant := 8;
   subtype TYPEKIND is Win32_Types.Long;
   type Pointer_To_TYPEKIND is access all TYPEKIND;
   --  Identifies TypeInfo Kind

   SYS_WIN16 : constant := 0;
   SYS_WIN32 : constant := 1;
   SYS_MAC   : constant := 2;
   subtype SYSKIND is Win32_Types.Long;
   type Pointer_TO_SYSKIND is access all SYSKIND;
   --  Identifies system type

   DESCKIND_NONE           : constant := 0;
   DESCKIND_FUNCDESC       : constant := 1;
   DESCKIND_VARDESC        : constant := 2;
   DESCKIND_TYPECOMP       : constant := 3;
   DESCKIND_IMPLICITAPPOBJ : constant := 4;
   DESCKIND_MAX            : constant := 5;
   subtype DESCKIND is Win32_Types.Long;
   type Pointer_To_DESCKIND is access all DESCKIND;

   FUNC_VIRTUAL            : constant := 0;
   FUNC_PUREVIRTUAL        : constant := 1;
   FUNC_NONVIRTUAL         : constant := 2;
   FUNC_STATIC             : constant := 3;
   FUNC_DISPATCH           : constant := 4;
   subtype FUNCKIND is Win32_Types.Long;
   type Pointer_To_FUNCKIND is access all FUNCKIND;

   INVOKE_FUNC             : constant := 1;
   INVOKE_PROPERTYGET      : constant := 2;
   INVOKE_PROPERTYPUT      : constant := 4;
   INVOKE_PROPERTYPUTREF   : constant := 8;
   subtype INVOKEKIND is Win32_Types.Long;
   type Pointer_To_INVOKEKIND is access all INVOKEKIND;

   CC_FASTCALL   : constant := 0;
   CC_CDECL      : constant := 1;
   CC_MSCPASCAL  : constant := 2;
   CC_PASCAL     : constant := 2;
   CC_MACPASCAL  : constant := 3;
   CC_STDCALL    : constant := 4;
   CC_FPFASTCALL : constant := 5;
   CC_SYSCALL    : constant := 6;
   CC_MPWCDECL   : constant := 7;
   CC_MPWPASCAL  : constant := 8;
   CC_MAX        : constant := 9;
   subtype CALLCONV is Win32_Types.Long;
   type Pointer_To_CALLCONV is access all CALLCONV;

   VAR_PERINSTANCE : constant := 0;
   VAR_STATIC      : constant := 1;
   VAR_CONST       : constant := 2;
   VAR_DISPATCH    : constant := 3;
   subtype VARKIND is Win32_Types.Long;
   type Pointer_To_VARKIND is access all VARKIND;

   type TYPEDESC;
   type Pointer_To_TYPEDESC is access all TYPEDESC;

   type ARRAYDESC;
   type Pointer_To_ARRAYDESC is access all ARRAYDESC;

   subtype TYPEDESC_UNION_Range is Positive range 1 .. 3;
   type TYPEDESC_UNION (Which : TYPEDESC_UNION_Range := 1) is
      record
         case Which is
            when 1 =>
               lptdesc : Pointer_To_TYPEDESC;
            when 2 =>
               lpadesc : Pointer_To_ARRAYDESC;
            when 3 =>
               hreftype : Win32_Types.Unsigned_Long;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, TYPEDESC_UNION);
   pragma Unchecked_Union (TYPEDESC_UNION);

   type TYPEDESC is
      record
         u  : TYPEDESC_UNION;
         vt : VARTYPE;
      end record;
   pragma Convention (C_Pass_By_Copy, TYPEDESC);

   type IDLDESC is
      record
         dwReserved : Win32_Types.Unsigned_Long;
         wIDLFlags  : Interfaces.C.short;
      end record;
   pragma Convention (C_Pass_By_Copy, IDLDESC);
   type Pointer_To_IDLDESC is access all IDLDESC;
   type Pointer_To_Pointer_To_IDLDESC is access all Pointer_To_IDLDESC;

   IMPLTYPEFLAG_FDEFAULT     : constant := 1;
   IMPLTYPEFLAG_FSOURCE      : constant := 2;
   IMPLTYPEFLAG_FRESTRICTED  : constant := 4;
   IMPLTYPEFLAG_FDEFAULTVTBL : constant := 8;

   TYPEFLAG_FAPPOBJECT     : constant := 1;
   TYPEFLAG_FCANCREATE     : constant := 2;
   TYPEFLAG_FLICENSED      : constant := 4;
   TYPEFLAG_FPREDECLID     : constant := 8;
   TYPEFLAG_FHIDDEN        : constant := 16;
   TYPEFLAG_FCONTROL       : constant := 32;
   TYPEFLAG_FDUAL          : constant := 64;
   TYPEFLAG_FNONEXTENSIBLE : constant := 128;
   TYPEFLAG_FOLEAUTOMATION : constant := 256;

   type TYPEATTR is
      record
         guid             : Types.GUID;
         lcid             : Win32_Types.Unsigned_Long;
         dwReserved       : Win32_Types.Unsigned_Long;
         memidConstructor : Win32_Types.Long;
         memidDestructor  : Win32_Types.Long;
         lpstrSchema      : LPWSTR;
         cbSizeInstance   : Win32_Types.Unsigned_Long;
         TYPEKIND_Element : TYPEKIND;
         cFuncs           : Interfaces.C.short;
         cVars            : Interfaces.C.short;
         cImplTypes       : Interfaces.C.short;
         cbSizeVft        : Interfaces.C.short;
         cbAlignment      : Interfaces.C.short;
         wTypeFlags       : Interfaces.C.unsigned_short;
         wMajorVerNum     : Interfaces.C.short;
         wMinorVerNum     : Interfaces.C.short;
         tdescAlias       : TYPEDESC;
         idldescType      : IDLDESC;
      end record;
   pragma Convention (C_Pass_By_Copy, TYPEATTR);
   type Pointer_To_TYPEATTR is access all TYPEATTR;
   type Pointer_To_Pointer_To_TYPEATTR is access all Pointer_To_TYPEATTR;

   type SAFEARRAYBOUND_Array is
     array (Interfaces.C.short range 0 .. 255)
     of aliased SAFEARRAYBOUND;
   pragma Convention (C, SAFEARRAYBOUND_Array);

   type ARRAYDESC is
      record
         tdescElem : TYPEDESC;
         cDims     : Interfaces.C.short;
         rgbounds  : SAFEARRAYBOUND_Array;
      end record;
   pragma Convention (C_Pass_By_Copy, ARRAYDESC);

   PARAMFLAG_NONE        : constant := 0;
   PARAMFLAG_FIN         : constant := 1;
   PARAMFLAG_FOUT        : constant := 2;
   PARAMFLAG_FLCID       : constant := 4;
   PARAMFLAG_FRETVAL     : constant := 8;
   PARAMFLAG_FOPT        : constant := 16;
   PARAMFLAG_FHASDEFAULT : constant := 32;

   type PARAMDESC is
      record
         lpVarValue  : Pointer_To_VARIANT;
         wParamFlags : Interfaces.C.unsigned_short;
      end record;
   pragma Convention (C_Pass_By_Copy, PARAMDESC);
   type Pointer_To_PARAMDESC is access all PARAMDESC;

   type ELEMDESC is
      record
         tdesc     : TYPEDESC;
         paramdesc : Types.PARAMDESC;
      end record;
   pragma Convention (C_Pass_By_Copy, ELEMDESC);
   type Pointer_To_ELEMDESC is access all ELEMDESC;

   type ELEMDESC_ARRAY is array (Interfaces.C.short range 0 .. 255) of
     ELEMDESC;
   type Pointer_To_ELEMDESC_ARRAY is access all ELEMDESC_ARRAY;

   type FUNCDESC is
      record
         memid             : Win32_Types.Long;
         lprgscode         : Pointer_To_SCODE;
         lprgelemdescParam : Pointer_To_ELEMDESC_ARRAY;
         funckind          : Types.FUNCKIND;
         invkind           : INVOKEKIND;
         callconv          : Types.CALLCONV;
         cParams           : Interfaces.C.short;
         cParamsOpt        : Interfaces.C.short;
         oVft              : Interfaces.C.short;
         cScodes           : Interfaces.C.short;
         elemdescFunc      : ELEMDESC;
         wFuncFlags        : Interfaces.C.short;
      end record;
   pragma Convention (C_Pass_By_Copy, FUNCDESC);
   type Pointer_To_FUNCDESC is access all FUNCDESC;
   type Pointer_To_Pointer_To_FUNCDESC is access all Pointer_To_FUNCDESC;

   subtype VARDESC_UNION_Range is Positive range 1 .. 2;
   type VARDESC_UNION (Which : VARDESC_UNION_Range := 1) is
      record
         case Which is
            when 1 =>
               oInst : Win32_Types.Unsigned_Long;
            when 2 =>
               lpvarValue : Pointer_To_VARIANT;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, VARDESC_UNION);
   pragma Unchecked_Union (VARDESC_UNION);

   VARFLAG_FREADONLY    : constant := 1;
   VARFLAG_FSOURCE      : constant := 2;
   VARFLAG_FBINDABLE    : constant := 4;
   VARFLAG_FREQUESTEDIT : constant := 8;
   VARFLAG_FDISPLAYBIND : constant := 16;
   VARFLAG_FDEFAULTBIND : constant := 32;
   VARFLAG_FHIDDEN      : constant := 64;

   type VARDESC is
      record
         memid           : Win32_Types.Long;
         lpstrSchema     : LPWSTR;
         u               : VARDESC_UNION;
         elemdescVar     : ELEMDESC;
         wVarFlags       : Interfaces.C.unsigned_short;
         varkind         : Types.VARKIND;
      end record;
   pragma Convention (C_Pass_By_Copy, VARDESC);
   type Pointer_To_VARDESC is access all VARDESC;
   type Pointer_To_Pointer_To_VARDESC is access all Pointer_To_VARDESC;

   type TLIBATTR is
      record
         guid            : Types.GUID;
         lcid            : Win32_Types.Unsigned_Long;
         syskind         : Types.SYSKIND;
         wMajorVerNum    : Interfaces.C.short;
         wMinorVerNum    : Interfaces.C.short;
         wLibFlags       : Interfaces.C.short;
      end record;
   pragma Convention (C_Pass_By_Copy, TLIBATTR);
   type Pointer_To_TLIBATTR is access all TLIBATTR;
   type Pointer_To_Pointer_To_TLIBATTR is access all Pointer_To_TLIBATTR;

   type LICINFO is
      record
         cbLicInfo        : Win32_Types.Long;
         fRuntimeKeyAvail : bool;
         fLicVerified     : bool;
      end record;
   pragma Convention (C_Pass_By_Copy, LICINFO);
   type Pointer_To_LICINFO is access all LICINFO;

   --  ITypeLib Interface
   --  {00020402-0000-0000-C000-000000000046}
   IID_ITypeLib : aliased GUID :=
     (132098, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_ITypeLib_QueryInterface is access
     function (This   : access ITypeLib;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_QueryInterface);
   pragma Machine_Attribute (af_ITypeLib_QueryInterface, "ms_abi");

   type af_ITypeLib_AddRef is access
     function (This : access ITypeLib) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeLib_AddRef);
   pragma Machine_Attribute (af_ITypeLib_AddRef, "ms_abi");

   type af_ITypeLib_Release is access
     function (This : access ITypeLib) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeLib_Release);
   pragma Machine_Attribute (af_ITypeLib_Release, "ms_abi");

   type af_ITypeLib_GetTypeInfoCount is access
     function (This    : access ITypeLib)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoCount);
   pragma Machine_Attribute (af_ITypeLib_GetTypeInfoCount, "ms_abi");

   type af_ITypeLib_GetTypeInfo is access
     function (This    : access ITypeLib;
               index   : in     Interfaces.C.int;
               ppTInfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfo);
   pragma Machine_Attribute (af_ITypeLib_GetTypeInfo, "ms_abi");

   type af_ITypeLib_GetTypeInfoType is access
     function (This   : access ITypeLib;
               index  : in     Interfaces.C.int;
               pTKind : in     Pointer_To_TYPEKIND)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoType);
   pragma Machine_Attribute (af_ITypeLib_GetTypeInfoType, "ms_abi");

   type af_ITypeLib_GetTypeInfoOfGuid is access
     function (This    : access ITypeLib;
               guid    : in     Pointer_To_GUID;
               ppTInfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoOfGuid);
   pragma Machine_Attribute (af_ITypeLib_GetTypeInfoOfGuid, "ms_abi");

   type af_ITypeLib_GetLibAttr is access
     function (This       : access ITypeLib;
               ppTLibAttr : in     Pointer_To_Pointer_To_TLIBATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetLibAttr);
   pragma Machine_Attribute (af_ITypeLib_GetLibAttr, "ms_abi");

   type af_ITypeLib_GetTypeComp is access
     function (This    : access ITypeLib;
               ppTComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeComp);
   pragma Machine_Attribute (af_ITypeLib_GetTypeComp, "ms_abi");

   type af_ITypeLib_GetDocumentation is access
     function (This           : access ITypeLib;
               index          : in     Interfaces.C.int;
               pBstrName      : in     Pointer_To_BSTR;
               pBstrDocString : in     Pointer_To_BSTR;
               pdwHelpContext : in     Pointer_To_unsigned_long;
               pBstrHelpFile  : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetDocumentation);
   pragma Machine_Attribute (af_ITypeLib_GetDocumentation, "ms_abi");

   type af_ITypeLib_IsName is access
     function (This         : access ITypeLib;
               szNameBuf    : in     LPWSTR;
               lHashVal     : in     Win32_Types.Unsigned_Long;
               pfName       : in     Pointer_To_bool)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_IsName);
   pragma Machine_Attribute (af_ITypeLib_IsName, "ms_abi");

   type af_ITypeLib_FindName is access
     function (This         : access ITypeLib;
               szNameBuf    : in     LPWSTR;
               lHashVal     : in     Win32_Types.Unsigned_Long;
               ppTInfo      : in     Pointer_To_Pointer_To_ITypeInfo;
               rgMemId      : in     Pointer_To_long;
               pcFound      : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_FindName);
   pragma Machine_Attribute (af_ITypeLib_FindName, "ms_abi");

   type af_ITypeLib_ReleaseTLibAttr is access
     function (This      : access ITypeLib;
               pTLibAttr : Pointer_To_TLIBATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_ReleaseTLibAttr);
   pragma Machine_Attribute (af_ITypeLib_ReleaseTLibAttr, "ms_abi");

   type ITypeLibVtbl;
   type Pointer_To_ITypeLibVtbl is access all ITypeLibVtbl;
   pragma No_Strict_Aliasing (Pointer_To_ITypeLib);

   type ITypeLib is
      record
         Vtbl : Pointer_To_ITypeLibVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeLib);

   type ITypeLibVtbl is
      record
         QueryInterface    : af_ITypeLib_QueryInterface;
         AddRef            : af_ITypeLib_AddRef;
         Release           : af_ITypeLib_Release;
         GetTypeInfoCount  : af_ITypeLib_GetTypeInfoCount;
         GetTypeInfo       : af_ITypeLib_GetTypeInfo;
         GetTypeInfoType   : af_ITypeLib_GetTypeInfoType;
         GetTypeInfoOfGuid : af_ITypeLib_GetTypeInfoOfGuid;
         GetLibAttr        : af_ITypeLib_GetLibAttr;
         GetTypeComp       : af_ITypeLib_GetTypeComp;
         GetDocumentation  : af_ITypeLib_GetDocumentation;
         IsName            : af_ITypeLib_IsName;
         FindName          : af_ITypeLib_FindName;
        ReleaseTLibAttr   : af_ITypeLib_ReleaseTLibAttr;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeLibVtbl);

   --  ITypeInfo Interface
   --  {00020401-0000-0000-C000-000000000046}
   IID_ITypeInfo : aliased GUID :=
     (132097, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_ITypeInfo_QueryInterface is access
     function (This   : access ITypeInfo;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_QueryInterface);
   pragma Machine_Attribute (af_ITypeInfo_QueryInterface, "ms_abi");

   type af_ITypeInfo_AddRef is access
     function (This : access ITypeInfo) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeInfo_AddRef);
   pragma Machine_Attribute (af_ITypeInfo_AddRef, "ms_abi");

   type af_ITypeInfo_Release is access
     function (This : access ITypeInfo) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeInfo_Release);
   pragma Machine_Attribute (af_ITypeInfo_Release, "ms_abi");

   type af_ITypeInfo_GetTypeAttr is access
     function (This       : access ITypeInfo;
               ppTypeAttr : in     Pointer_To_Pointer_To_TYPEATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetTypeAttr);
   pragma Machine_Attribute (af_ITypeInfo_GetTypeAttr, "ms_abi");

   type af_ITypeInfo_GetTypeComp is access
     function (This    : access ITypeInfo;
               ppTComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetTypeComp);
   pragma Machine_Attribute (af_ITypeInfo_GetTypeComp, "ms_abi");

   type af_ITypeInfo_GetFuncDesc is access
     function (This       : access ITypeInfo;
               index      : in     Interfaces.C.int;
               ppFuncDesc : in     Pointer_To_Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetFuncDesc);
   pragma Machine_Attribute (af_ITypeInfo_GetFuncDesc, "ms_abi");

   type af_ITypeInfo_GetVarDesc is access
     function (This      : access ITypeInfo;
               index     : in     Interfaces.C.int;
               ppVarDesc : in     Pointer_To_Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetVarDesc);
   pragma Machine_Attribute (af_ITypeInfo_GetVarDesc, "ms_abi");

   type af_ITypeInfo_GetNames is access
     function (This        : access ITypeInfo;
               memid       : in     Win32_Types.Long;
               rgBstrNames : in     Pointer_To_BSTR_PARAM_ARRAY;
               cMaxNames   : in     Interfaces.C.int;
               pcNames     : in     Pointer_To_unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetNames);
   pragma Machine_Attribute (af_ITypeInfo_GetNames, "ms_abi");

   type af_ITypeInfo_GetRefTypeOfImplType is access
     function (This     : access ITypeInfo;
               index    : in     Interfaces.C.int;
               pRefType : in     Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetRefTypeOfImplType);
   pragma Machine_Attribute (af_ITypeInfo_GetRefTypeOfImplType, "ms_abi");

   type af_ITypeInfo_GetImplTypeFlags is access
     function (This           : access ITypeInfo;
               index          : in     Interfaces.C.int;
               pImplTypeFlags : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetImplTypeFlags);
   pragma Machine_Attribute (af_ITypeInfo_GetImplTypeFlags, "ms_abi");

   type af_ITypeInfo_GetIDsOfNames is access
     function (This      : access ITypeInfo;
               rgszNames : in     Pointer_To_LPWSTR;
               cNames    : in     Interfaces.C.int;
               pMemId    : in     Pointer_To_long)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetIDsOfNames);
   pragma Machine_Attribute (af_ITypeInfo_GetIDsOfNames, "ms_abi");

   type af_ITypeInfo_Invoke is access
     function (This        : access ITypeInfo;
               pvInstance  : in Pointer_To_Void;
               memid       : in     Win32_Types.Long;
               wFlags      : in     Interfaces.C.short;
               pdispparams : in     Pointer_To_DISPPARAMS;
               pvarResult  : in     Pointer_To_VARIANT;
               pexcepinfo  : in     Pointer_To_EXCEPINFO;
               puArgErr    : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_Invoke);
   pragma Machine_Attribute (af_ITypeInfo_Invoke, "ms_abi");

   type af_ITypeInfo_GetDocumentation is access
     function (This           : access ITypeInfo;
               memid          : in     Win32_Types.Long;
               pBstrName      : in     Pointer_To_BSTR;
               pBstrDocString : in     Pointer_To_BSTR;
               pdwHelpContext : in     Pointer_To_unsigned_long;
               pBstrHelpFile  : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetDocumentation);
   pragma Machine_Attribute (af_ITypeInfo_GetDocumentation, "ms_abi");

   type af_ITypeInfo_GetDllEntry is access
     function (This         : access ITypeInfo;
               memid        : in     Win32_Types.Long;
               invkind      : in     INVOKEKIND;
               pBstrDllName : in     Pointer_To_BSTR;
               pBstrName    : in     Pointer_To_BSTR;
               pwOrdinal    : in     Pointer_To_short)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetDllEntry);
   pragma Machine_Attribute (af_ITypeInfo_GetDllEntry, "ms_abi");

   type af_ITypeInfo_GetRefTypeInfo is access
     function (This     : access ITypeInfo;
               hreftype : in     Win32_Types.Unsigned_Long;
               ppTInfo  : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetRefTypeInfo);
   pragma Machine_Attribute (af_ITypeInfo_GetRefTypeInfo, "ms_abi");

   type af_ITypeInfo_AddressOfMember is access
     function (This    : access ITypeInfo;
               memid   : in     Win32_Types.Long;
               invkind : in     INVOKEKIND;
               ppv     : in     Pointer_To_Pointer_To_Void)
    return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_AddressOfMember);
   pragma Machine_Attribute (af_ITypeInfo_AddressOfMember, "ms_abi");

   type af_ITypeInfo_CreateInstance is access
     function (This   : access ITypeInfo;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_IUnknown)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_CreateInstance);
   pragma Machine_Attribute (af_ITypeInfo_CreateInstance, "ms_abi");

   type af_ITypeInfo_GetMops is access
     function (This      : access ITypeInfo;
               memid     : in     Win32_Types.Long;
               pBstrMops : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetMops);
   pragma Machine_Attribute (af_ITypeInfo_GetMops, "ms_abi");

   type af_ITypeInfo_GetContainingTypeLib is access
     function (This   : access ITypeInfo;
               ppTLib : in     Pointer_To_Pointer_To_ITypeLib;
               pIndex : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetContainingTypeLib);
   pragma Machine_Attribute (af_ITypeInfo_GetContainingTypeLib, "ms_abi");

   type af_ITypeInfo_ReleaseTypeAttr is access
     function (This      : access ITypeInfo;
               pTypeAttr : in     Pointer_To_TYPEATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseTypeAttr);
   pragma Machine_Attribute (af_ITypeInfo_ReleaseTypeAttr, "ms_abi");

   type af_ITypeInfo_ReleaseFuncDesc is access
     function (This      : access ITypeInfo;
               pFuncDesc : in     Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseFuncDesc);
   pragma Machine_Attribute (af_ITypeInfo_ReleaseFuncDesc, "ms_abi");

   type af_ITypeInfo_ReleaseVarDesc is access
     function (This     : access ITypeInfo;
               pVarDesc : in     Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseVarDesc);
   pragma Machine_Attribute (af_ITypeInfo_ReleaseVarDesc, "ms_abi");

   type ITypeInfoVtbl;
   type Pointer_To_ITypeInfoVtbl is access all ITypeInfoVtbl;

   type ITypeInfo is
      record
         Vtbl : Pointer_To_ITypeInfoVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeInfo);

   type ITypeInfoVtbl is
      record
         QueryInterface       : af_ITypeInfo_QueryInterface;
         AddRef               : af_ITypeInfo_AddRef;
         Release              : af_ITypeInfo_Release;
         GetTypeAttr          : af_ITypeInfo_GetTypeAttr;
         GetTypeComp          : af_ITypeInfo_GetTypeComp;
         GetFuncDesc          : af_ITypeInfo_GetFuncDesc;
         GetVarDesc           : af_ITypeInfo_GetVarDesc;
         GetNames             : af_ITypeInfo_GetNames;
         GetRefTypeOfImplType : af_ITypeInfo_GetRefTypeOfImplType;
         GetImplTypeFlags     : af_ITypeInfo_GetImplTypeFlags;
         GetIDsOfNames        : af_ITypeInfo_GetIDsOfNames;
         Invoke               : af_ITypeInfo_Invoke;
         GetDocumentation     : af_ITypeInfo_GetDocumentation;
         GetDllEntry          : af_ITypeInfo_GetDllEntry;
         GetRefTypeInfo       : af_ITypeInfo_GetRefTypeInfo;
         AddressOfMember      : af_ITypeInfo_AddressOfMember;
         CreateInstance       : af_ITypeInfo_CreateInstance;
         GetMops              : af_ITypeInfo_GetMops;
         GetContainingTypeLib : af_ITypeInfo_GetContainingTypeLib;
         ReleaseTypeAttr      : af_ITypeInfo_ReleaseTypeAttr;
         ReleaseFuncDesc      : af_ITypeInfo_ReleaseFuncDesc;
         ReleaseVarDesc       : af_ITypeInfo_ReleaseVarDesc;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeInfoVtbl);

   --  ITypeComp Interface
   --  {00020403-0000-0000-C000-000000000046}
   IID_ITypeComp : aliased GUID :=
     (132099, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_ITypeComp_QueryInterface is access
     function (This   : access ITypeComp;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeComp_QueryInterface);
   pragma Machine_Attribute (af_ITypeComp_QueryInterface, "ms_abi");

   type af_ITypeComp_AddRef is access
     function (This : access ITypeComp) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeComp_AddRef);
   pragma Machine_Attribute (af_ITypeComp_AddRef, "ms_abi");

   type af_ITypeComp_Release is access
     function (This : access ITypeComp) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ITypeComp_Release);
   pragma Machine_Attribute (af_ITypeComp_Release, "ms_abi");

   type af_ITypeComp_Bind is access
     function (This       : access ITypeComp;
               szName     : in     LPWSTR;
               lHashVal   : in     Win32_Types.Unsigned_Long;
               wFlags     : in     Interfaces.C.short;
               ppTInfo    : in     Pointer_To_Pointer_To_ITypeInfo;
               pDescKind  : in     Pointer_To_DESCKIND;
               ppFuncDesc : in     Pointer_To_Pointer_To_FUNCDESC;
               ppVarDesc  : in     Pointer_To_Pointer_To_VARDESC;
               ppTypeComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeComp_Bind);
   pragma Machine_Attribute (af_ITypeComp_Bind, "ms_abi");

   type af_ITypeComp_BindType is access
     function (This     : access ITypeComp;
               SzName   : in     LPWSTR;
               lHashVal : in     Win32_Types.Unsigned_Long;
               ppTInfo  : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeComp_BindType);
   pragma Machine_Attribute (af_ITypeComp_BindType, "ms_abi");

   type ITypeCompVtbl;
   type Pointer_To_ITypeCompVtbl is access all ITypeCompVtbl;

   type ITypeComp is
      record
         Vtbl : Pointer_To_ITypeCompVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeComp);

   type ITypeCompVtbl is
      record
         QueryInterface : af_ITypeComp_QueryInterface;
         AddRef         : af_ITypeComp_AddRef;
         Release        : af_ITypeComp_Release;
         Bind           : af_ITypeComp_Bind;
         BindType       : af_ITypeComp_BindType;
      end record;
   pragma Convention (C_Pass_By_Copy, ITypeCompVtbl);

   --  ICreateTypeInfo
   --  {00020405-0000-0000-C000-000000000046}

   IID_ICreateTypeInfo : GUID :=
     (16#00020405#, 16#0000#, 16#0000#,
      (C.unsigned_char'Val (16#C0#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#46#)));

   type af_ICreateTypeInfo_QueryInterface is access
     function (This   : access ICreateTypeInfo;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_QueryInterface);
   pragma Machine_Attribute (af_ICreateTypeInfo_QueryInterface, "ms_abi");

   type af_ICreateTypeInfo_AddRef is access
     function (This : access ICreateTypeInfo)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddRef);
   pragma Machine_Attribute (af_ICreateTypeInfo_AddRef, "ms_abi");

   type af_ICreateTypeInfo_Release is access
     function (This : access ICreateTypeInfo)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ICreateTypeInfo_Release);
   pragma Machine_Attribute (af_ICreateTypeInfo_Release, "ms_abi");

   type af_ICreateTypeInfo_SetGuid is access
     function (This : access ICreateTypeInfo;
               guid : Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetGuid);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetGuid, "ms_abi");

   type af_ICreateTypeInfo_SetTypeFlags is access
     function (This       : access ICreateTypeInfo;
               uTypeFlags : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeFlags);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetTypeFlags, "ms_abi");

   type af_ICreateTypeInfo_SetDocString is access
     function (This    : access ICreateTypeInfo;
               pStrDoc : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetDocString);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetDocString, "ms_abi");

   type af_ICreateTypeInfo_SetHelpContext is access
     function (This          : access ICreateTypeInfo;
               dwHelpContext : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetHelpContext);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetHelpContext, "ms_abi");

   type af_ICreateTypeInfo_SetVersion is access
     function (This         : access ICreateTypeInfo;
               wMajorVerNum : Interfaces.C.unsigned_short;
               wMinorVerNum : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVersion);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetVersion, "ms_abi");

   type af_ICreateTypeInfo_AddRefTypeInfo is access
     function (This      : access ICreateTypeInfo;
               pTInfo    : Pointer_To_ITypeInfo;
               phRefType : Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddRefTypeInfo);
   pragma Machine_Attribute (af_ICreateTypeInfo_AddRefTypeInfo, "ms_abi");

   type af_ICreateTypeInfo_AddFuncDesc is access
     function (This      : access ICreateTypeInfo;
               index     : Interfaces.C.unsigned;
               pFuncDesc : Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddFuncDesc);
   pragma Machine_Attribute (af_ICreateTypeInfo_AddFuncDesc, "ms_abi");

   type af_ICreateTypeInfo_AddImplType is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               hreftype : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddImplType);
   pragma Machine_Attribute (af_ICreateTypeInfo_AddImplType, "ms_abi");

   type af_ICreateTypeInfo_SetImplTypeFlags is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               implTypeFlags : Interfaces.C.int)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetImplTypeFlags);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetImplTypeFlags, "ms_abi");

   type af_ICreateTypeInfo_SetAlignment is access
     function (This        : access ICreateTypeInfo;
               cbAlignment : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetAlignment);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetAlignment, "ms_abi");

   type af_ICreateTypeInfo_SetSchema is access
     function (This       : access ICreateTypeInfo;
               pStrSchema : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetSchema);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetSchema, "ms_abi");

   type af_ICreateTypeInfo_AddVarDesc is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               pVarDesc : Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddVarDesc);
   pragma Machine_Attribute (af_ICreateTypeInfo_AddVarDesc, "ms_abi");

   type af_ICreateTypeInfo_SetFuncAndParamNames is access
     function (This      : access ICreateTypeInfo;
               index     : Interfaces.C.unsigned;
               rgszNames : Pointer_To_LPWSTR;
               cNames    : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncAndParamNames);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetFuncAndParamNames, "ms_abi");

   type af_ICreateTypeInfo_SetVarName is access
     function (This   : access ICreateTypeInfo;
               index  : Interfaces.C.unsigned;
               szName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarName);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetVarName, "ms_abi");

   type af_ICreateTypeInfo_SetTypeDescAlias is access
     function (This        : access ICreateTypeInfo;
               pTDescAlias : Pointer_To_TYPEDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeDescAlias);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetTypeDescAlias, "ms_abi");

   type af_ICreateTypeInfo_DefineFuncAsDllEntry is access
     function (This       : access ICreateTypeInfo;
               index      : Interfaces.C.unsigned;
               szDllName  : LPWSTR;
               szProcName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_DefineFuncAsDllEntry);
   pragma Machine_Attribute (af_ICreateTypeInfo_DefineFuncAsDllEntry, "ms_abi");

   type af_ICreateTypeInfo_SetFuncDocString is access
     function (This        : access ICreateTypeInfo;
               index       : Interfaces.C.unsigned;
               szDocString : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncDocString);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetFuncDocString, "ms_abi");

   type af_ICreateTypeInfo_SetVarDocString is access
     function (This        : access ICreateTypeInfo;
               index       : Interfaces.C.unsigned;
               szDocString : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarDocString);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetVarDocString, "ms_abi");

   type af_ICreateTypeInfo_SetFuncHelpContext is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               dwHelpContext : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncHelpContext);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetFuncHelpContext, "ms_abi");

   type af_ICreateTypeInfo_SetVarHelpContext is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               dwHelpContext : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarHelpContext);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetVarHelpContext, "ms_abi");

   type af_ICreateTypeInfo_SetMops is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               bstrMops : BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetMops);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetMops, "ms_abi");

   type af_ICreateTypeInfo_SetTypeIdldesc is access
     function (This     : access ICreateTypeInfo;
               pIdlDesc : Pointer_To_IDLDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeIdldesc);
   pragma Machine_Attribute (af_ICreateTypeInfo_SetTypeIdldesc, "ms_abi");

   type af_ICreateTypeInfo_LayOut is access
     function (This : access ICreateTypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_LayOut);
   pragma Machine_Attribute (af_ICreateTypeInfo_LayOut, "ms_abi");

   type ICreateTypeInfoVtbl;
   type Pointer_To_ICreateTypeInfoVtbl is access all ICreateTypeInfoVtbl;

   type ICreateTypeInfo is
      record
         Vtbl : Pointer_To_ICreateTypeInfoVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateTypeInfo);

   type ICreateTypeInfoVtbl is
      record
         QueryInterface       : af_ICreateTypeInfo_QueryInterface;
         AddRef               : af_ICreateTypeInfo_AddRef;
         Release              : af_ICreateTypeInfo_Release;
         SetGuid              : af_ICreateTypeInfo_SetGuid;
         SetTypeFlags         : af_ICreateTypeInfo_SetTypeFlags;
         SetDocString         : af_ICreateTypeInfo_SetDocString;
         SetHelpContext       : af_ICreateTypeInfo_SetHelpContext;
         SetVersion           : af_ICreateTypeInfo_SetVersion;
         AddRefTypeInfo       : af_ICreateTypeInfo_AddRefTypeInfo;
         AddFuncDesc          : af_ICreateTypeInfo_AddFuncDesc;
         AddImplType          : af_ICreateTypeInfo_AddImplType;
         SetImplTypeFlags     : af_ICreateTypeInfo_SetImplTypeFlags;
         SetAlignment         : af_ICreateTypeInfo_SetAlignment;
         SetSchema            : af_ICreateTypeInfo_SetSchema;
         AddVarDesc           : af_ICreateTypeInfo_AddVarDesc;
         SetFuncAndParamNames : af_ICreateTypeInfo_SetFuncAndParamNames;
         SetVarName           : af_ICreateTypeInfo_SetVarName;
         SetTypeDescAlias     : af_ICreateTypeInfo_SetTypeDescAlias;
         DefineFuncAsDllEntry : af_ICreateTypeInfo_DefineFuncAsDllEntry;
         SetFuncDocString     : af_ICreateTypeInfo_SetFuncDocString;
         SetVarDocString      : af_ICreateTypeInfo_SetVarDocString;
         SetFuncHelpContext   : af_ICreateTypeInfo_SetFuncHelpContext;
         SetVarHelpContext    : af_ICreateTypeInfo_SetVarHelpContext;
         SetMops              : af_ICreateTypeInfo_SetMops;
         SetTypeIdldesc       : af_ICreateTypeInfo_SetTypeIdldesc;
         LayOut               : af_ICreateTypeInfo_LayOut;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateTypeInfoVtbl);

   --  ICreateTypeLib
   --  {00020406-0000-0000-C000-000000000046}

   IID_ICreateTypeLib : aliased GUID :=
     (16#00020406#, 16#0000#, 16#0000#,
      (C.unsigned_char'Val (16#C0#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#46#)));

   type af_ICreateTypeLib_QueryInterface is access
     function (This   : access ICreateTypeLib;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_QueryInterface);
   pragma Machine_Attribute (af_ICreateTypeLib_QueryInterface, "ms_abi");

   type af_ICreateTypeLib_AddRef is access
     function (This : access ICreateTypeLib)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ICreateTypeLib_AddRef);
   pragma Machine_Attribute (af_ICreateTypeLib_AddRef, "ms_abi");

   type af_ICreateTypeLib_Release is access
     function (This : access ICreateTypeLib)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ICreateTypeLib_Release);
   pragma Machine_Attribute (af_ICreateTypeLib_Release, "ms_abi");

   type af_ICreateTypeLib_CreateTypeInfo is access
     function (This     : access ICreateTypeLib;
               szName   : LPWSTR;
               tkind    : TYPEKIND;
               ppCTInfo : Pointer_To_Pointer_To_ICreateTypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_CreateTypeInfo);
   pragma Machine_Attribute (af_ICreateTypeLib_CreateTypeInfo, "ms_abi");

   type af_ICreateTypeLib_SetName is access
     function (This   : access ICreateTypeLib;
               szName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetName);
   pragma Machine_Attribute (af_ICreateTypeLib_SetName, "ms_abi");

   type af_ICreateTypeLib_SetVersion is access
     function (This         : access ICreateTypeLib;
               wMajorVerNum : Interfaces.C.unsigned_short;
               wMinorVerNum : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetVersion);
   pragma Machine_Attribute (af_ICreateTypeLib_SetVersion, "ms_abi");

   type af_ICreateTypeLib_SetGuid is access
     function (This : access ICreateTypeLib;
               guid : Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetGuid);
   pragma Machine_Attribute (af_ICreateTypeLib_SetGuid, "ms_abi");

   type af_ICreateTypeLib_SetDocString is access
     function (This  : access ICreateTypeLib;
               szDoc : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetDocString);
   pragma Machine_Attribute (af_ICreateTypeLib_SetDocString, "ms_abi");

   type af_ICreateTypeLib_SetHelpFileName is access
     function (This           : access ICreateTypeLib;
               szHelpFileName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetHelpFileName);
   pragma Machine_Attribute (af_ICreateTypeLib_SetHelpFileName, "ms_abi");

   type af_ICreateTypeLib_SetHelpContext is access
     function (This          : access ICreateTypeLib;
               dwHelpContext : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetHelpContext);
   pragma Machine_Attribute (af_ICreateTypeLib_SetHelpContext, "ms_abi");

   type af_ICreateTypeLib_SetLcid is access
     function (This : access ICreateTypeLib;
               lcid : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetLcid);
   pragma Machine_Attribute (af_ICreateTypeLib_SetLcid, "ms_abi");

   type af_ICreateTypeLib_SetLibFlags is access
     function (This      : access ICreateTypeLib;
               uLibFlags : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetLibFlags);
   pragma Machine_Attribute (af_ICreateTypeLib_SetLibFlags, "ms_abi");

   type af_ICreateTypeLib_SaveAllChanges is access
     function (This : access ICreateTypeLib)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SaveAllChanges);
   pragma Machine_Attribute (af_ICreateTypeLib_SaveAllChanges, "ms_abi");

   type ICreateTypeLibVtbl;
   type Pointer_To_ICreateTypeLibVtbl is access all ICreateTypeLibVtbl;

   type ICreateTypeLib is
      record
         Vtbl : Pointer_To_ICreateTypeLibVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateTypeLib);

   type ICreateTypeLibVtbl is
      record
         QueryInterface  : af_ICreateTypeLib_QueryInterface;
         AddRef          : af_ICreateTypeLib_AddRef;
         Release         : af_ICreateTypeLib_Release;
         CreateTypeInfo  : af_ICreateTypeLib_CreateTypeInfo;
         SetName         : af_ICreateTypeLib_SetName;
         SetVersion      : af_ICreateTypeLib_SetVersion;
         SetGuid         : af_ICreateTypeLib_SetGuid;
         SetDocString    : af_ICreateTypeLib_SetDocString;
         SetHelpFileName : af_ICreateTypeLib_SetHelpFileName;
         SetHelpContext  : af_ICreateTypeLib_SetHelpContext;
         SetLcid         : af_ICreateTypeLib_SetLcid;
         SetLibFlags     : af_ICreateTypeLib_SetLibFlags;
         SaveAllChanges  : af_ICreateTypeLib_SaveAllChanges;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateTypeLibVtbl);

   --  IProvideClassInfo
   --  {B196B283-BAB4-101A-B69C-00AA00341D07}

   IID_IProvideClassInfo : aliased GUID :=
     (16#B196B283#, 16#BAB4#, 16#101A#,
      (C.unsigned_char'Val (16#B6#), C.unsigned_char'Val (16#9C#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#34#),
       C.unsigned_char'Val (16#1D#), C.unsigned_char'Val (16#07#)));

   type af_IProvideClassInfo_QueryInterface is access
     function (This   : access IProvideClassInfo;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT with Convention => Stdcall;

   type af_IProvideClassInfo_AddRef is access
     function (This : access IProvideClassInfo)
     return Win32_Types.Unsigned_Long with Convention => Stdcall;

   type af_IProvideClassInfo_Release is access
     function (This : access IProvideClassInfo)
     return Win32_Types.Unsigned_Long with Convention => Stdcall;

   type af_IProvideClassInfo_GetClassInfo is access
     function (This : access IProvideClassInfo;
               ppTI : Pointer_To_Pointer_To_Void)
               return HRESULT
     with Convention => Stdcall;

   type IProvideClassInfoVtbl is
      record
         QueryInterface  : af_IProvideClassInfo_QueryInterface;
         AddRef          : af_IProvideClassInfo_AddRef;
         Release         : af_IProvideClassInfo_Release;
         GetClassInfo    : af_IProvideClassInfo_GetClassInfo;
      end record
     with Convention => C_Pass_By_Copy;

   type Pointer_To_IProvideClassInfoVtbl is access all IProvideClassInfoVtbl;

   type IProvideClassInfo is
      record
         Vtbl : Pointer_To_IProvideClassInfoVtbl;
      end record
   with Convention => C_Pass_By_Copy;

   --  IProvideClassInfo2
   --  {A6BC3AC0-DBAA-11CE-9DE3-00AA004BB851}

   IID_IProvideClassInfo2 : aliased GUID :=
     (16#A6BC3AC0#, 16#DBAA#, 16#11CE#,
      (C.unsigned_char'Val (16#9D#), C.unsigned_char'Val (16#E3#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#4B#),
       C.unsigned_char'Val (16#B8#), C.unsigned_char'Val (16#51#)));

   type af_IProvideClassInfo2_QueryInterface is access
     function (This   : access IProvideClassInfo2;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT with Convention => Stdcall;

   type af_IProvideClassInfo2_AddRef is access
     function (This : access IProvideClassInfo2)
     return Win32_Types.Unsigned_Long with Convention => Stdcall;

   type af_IProvideClassInfo2_Release is access
     function (This : access IProvideClassInfo2)
     return Win32_Types.Unsigned_Long with Convention => Stdcall;

   type af_IProvideClassInfo2_GetGUID is access
     function (This       : access IProvideClassInfo2;
               dwGuidKind : DWORD;
               pGUID      : Pointer_To_GUID)
               return HRESULT
     with Convention => Stdcall;

   type IProvideClassInfo2Vtbl is
      record
         QueryInterface : af_IProvideClassInfo2_QueryInterface;
         AddRef         : af_IProvideClassInfo2_AddRef;
         Release        : af_IProvideClassInfo2_Release;
         GetGUID        : af_IProvideClassInfo2_GetGUID;
      end record
     with Convention => C_Pass_By_Copy;

   type Pointer_To_IProvideClassInfo2Vtbl is access all IProvideClassInfo2Vtbl;

   type IProvideClassInfo2 is
      record
         Vtbl : Pointer_To_IProvideClassInfo2Vtbl;
      end record
   with Convention => C_Pass_By_Copy;

   --  IClassFactory2 Interface
   --  {B196B28F-BAB4-101A-B69C-00AA00341D07}
   IID_IClassFactory2 : aliased GUID :=
     (16#B196B28F#, 16#BAB4#, 16#101A#,
      (C.unsigned_char'Val (16#B6#), C.unsigned_char'Val (16#9C#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#34#),
       C.unsigned_char'Val (16#1D#), C.unsigned_char'Val (16#07#)));

   type af_IClassFactory2_QueryInterface is access
     function (This   : access IClassFactory2;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IClassFactory2_QueryInterface);
   pragma Machine_Attribute (af_IClassFactory2_QueryInterface, "ms_abi");

   type af_IClassFactory2_AddRef is access
     function (This : access IClassFactory2) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IClassFactory2_AddRef);
   pragma Machine_Attribute (af_IClassFactory2_AddRef, "ms_abi");

   type af_IClassFactory2_Release is access
     function (This : access IClassFactory2) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IClassFactory2_Release);
   pragma Machine_Attribute (af_IClassFactory2_Release, "ms_abi");

   type af_IClassFactory2_CreateInstance is access
     function (This      : access IClassFactory2;
               pUnkOuter : in     Pointer_To_IUnknown;
               riid      : in     Pointer_To_GUID;
               ppvObject : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_CreateInstance);
   pragma Machine_Attribute (af_IClassFactory2_CreateInstance, "ms_abi");

   type af_IClassFactory2_LockServer is access
     function (This  : access IClassFactory2;
               fLock : in     bool)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_LockServer);
   pragma Machine_Attribute (af_IClassFactory2_LockServer, "ms_abi");

   type af_IClassFactory2_GetLicInfo is access
     function (This     : access IClassFactory2;
               pLicInfo : in     Pointer_To_LICINFO)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_GetLicInfo);
   pragma Machine_Attribute (af_IClassFactory2_GetLicInfo, "ms_abi");

   type af_IClassFactory2_RequestLicKey is access
     function (This       : access IClassFactory2;
               dwReserved : in     DWORD;
               pLicInfo   : in     Pointer_To_BSTR)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_RequestLicKey);
   pragma Machine_Attribute (af_IClassFactory2_RequestLicKey, "ms_abi");

   type af_IClassFactory2_CreateInstanceLic is access
     function (This         : access IClassFactory2;
               pUnkOuter    : in     Pointer_To_IUnknown;
               pUnkReserved : in     Pointer_To_IUnknown;
               riid         : in     Pointer_To_GUID;
               bstrKey      : in     BSTR;
               ppv          : in     Pointer_To_Pointer_To_Void)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_CreateInstanceLic);
   pragma Machine_Attribute (af_IClassFactory2_CreateInstanceLic, "ms_abi");

   type IClassFactory2Vtbl;
   type Pointer_To_IClassFactory2Vtbl is access all IClassFactory2Vtbl;

   type IClassFactory2 is
      record
         Vtbl : Pointer_To_IClassFactory2Vtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IClassFactory2);

   type IClassFactory2Vtbl is
      record
         QueryInterface    : af_IClassFactory2_QueryInterface;
         AddRef            : af_IClassFactory2_AddRef;
         Release           : af_IClassFactory2_Release;
         CreateInstance    : af_IClassFactory2_CreateInstance;
         LockServer        : af_IClassFactory2_LockServer;
         GetLicInfo        : af_IClassFactory2_GetLicInfo;
         RequestLicKey     : af_IClassFactory2_RequestLicKey;
         CreateInstanceLic : af_IClassFactory2_CreateInstanceLic;
      end record;
   pragma Convention (C_Pass_By_Copy, IClassFactory2Vtbl);

   --  IClassFactory Interface
   --  {B196B28F-BAB4-101A-B69C-00AA00341D07}
   IID_IClassFactory : aliased GUID :=
     (16#00000001#, 16#0000#, 16#0000#,
      (C.unsigned_char'Val (16#C0#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#00#),
       C.unsigned_char'Val (16#00#), C.unsigned_char'Val (16#46#)));

   type af_IClassFactory_QueryInterface is access
     function (This   : access IClassFactory;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IClassFactory_QueryInterface);
   pragma Machine_Attribute (af_IClassFactory_QueryInterface, "ms_abi");

   type af_IClassFactory_AddRef is access
     function (This : access IClassFactory) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IClassFactory_AddRef);
   pragma Machine_Attribute (af_IClassFactory_AddRef, "ms_abi");

   type af_IClassFactory_Release is access
     function (This : access IClassFactory) return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IClassFactory_Release);
   pragma Machine_Attribute (af_IClassFactory_Release, "ms_abi");

   type af_IClassFactory_CreateInstance is access
     function (This      : access IClassFactory;
               pUnkOuter : in     Pointer_To_IUnknown;
               riid      : in     Pointer_To_GUID;
               ppvObject : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory_CreateInstance);
   pragma Machine_Attribute (af_IClassFactory_CreateInstance, "ms_abi");

   type af_IClassFactory_LockServer is access
     function (This  : access IClassFactory;
               fLock : in     bool)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory_LockServer);
   pragma Machine_Attribute (af_IClassFactory_LockServer, "ms_abi");

   type IClassFactoryVtbl;
   type Pointer_To_IClassFactoryVtbl is access all IClassFactoryVtbl;

   type IClassFactory is
      record
         Vtbl : Pointer_To_IClassFactoryVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IClassFactory);

   type IClassFactoryVtbl is
      record
         QueryInterface    : af_IClassFactory_QueryInterface;
         AddRef            : af_IClassFactory_AddRef;
         Release           : af_IClassFactory_Release;
         CreateInstance    : af_IClassFactory_CreateInstance;
         LockServer        : af_IClassFactory_LockServer;
      end record;
   pragma Convention (C_Pass_By_Copy, IClassFactoryVtbl);

   --  IConnectionPointContainer Interface
   --  {B196B284-BAB4-101A-B69C-00AA00341D07}
   IID_IConnectionPointContainer : aliased GUID :=
     (2979443332, 47796, 4122,
      (C.unsigned_char'Val (182), C.unsigned_char'Val (156),
       C.unsigned_char'Val (0), C.unsigned_char'Val (170),
       C.unsigned_char'Val (0), C.unsigned_char'Val (52),
       C.unsigned_char'Val (29), C.unsigned_char'Val (7)));

   type af_IConnectionPointContainer_QueryInterface is access
     function  (This   : access IConnectionPointContainer;
                riid   :        Pointer_To_GUID;
                ppvObj :        Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPointContainer_QueryInterface);
   pragma Machine_Attribute (af_IConnectionPointContainer_QueryInterface, "ms_abi");

   type af_IConnectionPointContainer_AddRef is access
     function (This : access IConnectionPointContainer)
     return Win32_Types.Unsigned_Long;

   pragma Convention (StdCall, af_IConnectionPointContainer_AddRef);
   pragma Machine_Attribute (af_IConnectionPointContainer_AddRef, "ms_abi");

   type af_IConnectionPointContainer_Release is access
     function (This : access IConnectionPointContainer)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IConnectionPointContainer_Release);
   pragma Machine_Attribute (af_IConnectionPointContainer_Release, "ms_abi");

   type af_IConnectionPointContainer_EnumConnectionPoints is access
     function (This   : access IConnectionPointContainer;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnectionPoints)
     return HRESULT;
   pragma Convention
     (StdCall, af_IConnectionPointContainer_EnumConnectionPoints);

   type af_IConnectionPointContainer_FindConnectionPoint is access
     function (This : access IConnectionPointContainer;
               riid :        Pointer_To_GUID;
               ppCP : Pointer_To_Pointer_To_IConnectionPoint)
     return HRESULT;
   pragma Convention
     (StdCall, af_IConnectionPointContainer_FindConnectionPoint);

   type IConnectionPointContainerVtbl;
   type Pointer_To_IConnectionPointContainerVtbl is
     access all IConnectionPointContainerVtbl;

   type IConnectionPointContainer is
      record
         Vtbl : Pointer_To_IConnectionPointContainerVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IConnectionPointContainer);

   type IConnectionPointContainerVtbl is
      record
         QueryInterface       : af_IConnectionPointContainer_QueryInterface;
         AddRef               : af_IConnectionPointContainer_AddRef;
         Release              : af_IConnectionPointContainer_Release;
         EnumConnectionPoints :
           af_IConnectionPointContainer_EnumConnectionPoints;
         FindConnectionPoint  :
           af_IConnectionPointContainer_FindConnectionPoint;
      end record;
   pragma Convention (C_Pass_By_Copy, IConnectionPointContainerVtbl);

   --  IEnumConnectionPoints Interface
   --  {B196B285-BAB4-101A-B69C-00AA00341D07}
   IID_IEnumConnectionPoints : aliased GUID :=
     (2979443333, 47796, 4122,
      (C.unsigned_char'Val (182), C.unsigned_char'Val (156),
       C.unsigned_char'Val (0), C.unsigned_char'Val (170),
       C.unsigned_char'Val (0), C.unsigned_char'Val (52),
       C.unsigned_char'Val (29), C.unsigned_char'Val (7)));

   type af_IEnumConnectionPoints_QueryInterface is access
     function (This   : access IEnumConnectionPoints;
               riid   :        Pointer_To_GUID;
               ppvObj :        Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_QueryInterface);
   pragma Machine_Attribute (af_IEnumConnectionPoints_QueryInterface, "ms_abi");

   type af_IEnumConnectionPoints_AddRef is access
     function (This : access IEnumConnectionPoints)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumConnectionPoints_AddRef);
   pragma Machine_Attribute (af_IEnumConnectionPoints_AddRef, "ms_abi");

   type af_IEnumConnectionPoints_Release is access
     function (This : access IEnumConnectionPoints)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Release);
   pragma Machine_Attribute (af_IEnumConnectionPoints_Release, "ms_abi");

   type af_IEnumConnectionPoints_Next is access
     function (This         : access IEnumConnectionPoints;
               cConnections :        Win32_Types.Unsigned_Long;
               rgpcn        :        Pointer_To_Pointer_To_IConnectionPoint;
               lpcFetched   :        Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Next);
   pragma Machine_Attribute (af_IEnumConnectionPoints_Next, "ms_abi");

   type af_IEnumConnectionPoints_Skip is access
     function (This         : access IEnumConnectionPoints;
               cConnections :        Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Skip);
   pragma Machine_Attribute (af_IEnumConnectionPoints_Skip, "ms_abi");

   type af_IEnumConnectionPoints_Reset is access
     function (This : access IEnumConnectionPoints) return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Reset);
   pragma Machine_Attribute (af_IEnumConnectionPoints_Reset, "ms_abi");

   type af_IEnumConnectionPoints_Clone is access
     function (This   : access IEnumConnectionPoints;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnectionPoints)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Clone);
   pragma Machine_Attribute (af_IEnumConnectionPoints_Clone, "ms_abi");

   type IEnumConnectionPointsVtbl;
   type Pointer_To_IEnumConnectionPointsVtbl is
     access all IEnumConnectionPointsVtbl;

   type IEnumConnectionPoints is
      record
         Vtbl : Pointer_To_IEnumConnectionPointsVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumConnectionPoints);

   type IEnumConnectionPointsVtbl is
      record
         QueryInterface : af_IEnumConnectionPoints_QueryInterface;
         AddRef         : af_IEnumConnectionPoints_AddRef;
         Release        : af_IEnumConnectionPoints_Release;
         Next           : af_IEnumConnectionPoints_Next;
         Skip           : af_IEnumConnectionPoints_Skip;
         Reset          : af_IEnumConnectionPoints_Reset;
         Clone          : af_IEnumConnectionPoints_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumConnectionPointsVtbl);

   --  IConnectionPoint Interface
   --  {B196B286-BAB4-101A-B69C-00AA00341D07}
   IID_IConnectionPoint : aliased GUID :=
     (2979443334, 47796, 4122,
      (C.unsigned_char'Val (182), C.unsigned_char'Val (156),
       C.unsigned_char'Val (0), C.unsigned_char'Val (170),
       C.unsigned_char'Val (0), C.unsigned_char'Val (52),
       C.unsigned_char'Val (29), C.unsigned_char'Val (7)));

   type af_IConnectionPoint_QueryInterface is access
     function (This   : access IConnectionPoint;
               riid   :        Pointer_To_GUID;
               ppvObj :        Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_QueryInterface);
   pragma Machine_Attribute (af_IConnectionPoint_QueryInterface, "ms_abi");

   type af_IConnectionPoint_AddRef is access
     function (This : access IConnectionPoint)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IConnectionPoint_AddRef);
   pragma Machine_Attribute (af_IConnectionPoint_AddRef, "ms_abi");

   type af_IConnectionPoint_Release is access
     function (This : access IConnectionPoint)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IConnectionPoint_Release);
   pragma Machine_Attribute (af_IConnectionPoint_Release, "ms_abi");

   type af_IConnectionPoint_GetConnectionInterface is access
     function (This : access IConnectionPoint;
               piid :        Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_GetConnectionInterface);
   pragma Machine_Attribute (af_IConnectionPoint_GetConnectionInterface, "ms_abi");

   type af_IConnectionPoint_GetConnectionPointContainer is access
     function (This  : access IConnectionPoint;
               ppCPC : Pointer_To_Pointer_To_IConnectionPointContainer)
     return HRESULT;
   pragma Convention (StdCall,
                      af_IConnectionPoint_GetConnectionPointContainer);

   type af_IConnectionPoint_Advise is access
     function (This      : access IConnectionPoint;
               PUnkSink  :        Pointer_To_IUnknown;
               pdwCookie :        Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_Advise);
   pragma Machine_Attribute (af_IConnectionPoint_Advise, "ms_abi");

   type af_IConnectionPoint_Unadvise is access
     function (This     : access IConnectionPoint;
               dwCookie : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_Unadvise);
   pragma Machine_Attribute (af_IConnectionPoint_Unadvise, "ms_abi");

   type af_IConnectionPoint_EnumConnections is access
     function (This   : access IConnectionPoint;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnections)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_EnumConnections);
   pragma Machine_Attribute (af_IConnectionPoint_EnumConnections, "ms_abi");

   type IConnectionPointVtbl;
   type Pointer_To_IConnectionPointVtbl is
     access all IConnectionPointVtbl;

   type IConnectionPoint is
      record
         Vtbl : Pointer_To_IConnectionPointVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IConnectionPoint);

   type IConnectionPointVtbl is
      record
         QueryInterface              : af_IConnectionPoint_QueryInterface;
         AddRef                      : af_IConnectionPoint_AddRef;
         Release                     : af_IConnectionPoint_Release;
         GetConnectionInterface      :
           af_IConnectionPoint_GetConnectionInterface;
         GetConnectionPointContainer :
           af_IConnectionPoint_GetConnectionPointContainer;
         Advise                      : af_IConnectionPoint_Advise;
         Unadvise                    : af_IConnectionPoint_Unadvise;
         EnumConnections             : af_IConnectionPoint_EnumConnections;
      end record;
   pragma Convention (C_Pass_By_Copy, IConnectionPointVtbl);

   type CONNECTDATA is
      record
         punk : Pointer_To_IUnknown;
         dwCookie : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, CONNECTDATA);
   type Pointer_To_CONNECTDATA is access all CONNECTDATA;

   --  IEnumConnections Interface
   --  {B196B287-BAB4-101A-B69C-00AA00341D07}
   IID_IEnumConnections : aliased GUID :=
     (2979443335, 47796, 4122,
      (C.unsigned_char'Val (182), C.unsigned_char'Val (156),
       C.unsigned_char'Val (0), C.unsigned_char'Val (170),
       C.unsigned_char'Val (0), C.unsigned_char'Val (52),
       C.unsigned_char'Val (29), C.unsigned_char'Val (7)));

   type af_IEnumConnections_QueryInterface is access
     function (This   : access IEnumConnections;
               riid   :        Pointer_To_GUID;
               ppvObj :        Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_QueryInterface);
   pragma Machine_Attribute (af_IEnumConnections_QueryInterface, "ms_abi");

   type af_IEnumConnections_AddRef is access
     function (This : access IEnumConnections)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumConnections_AddRef);
   pragma Machine_Attribute (af_IEnumConnections_AddRef, "ms_abi");

   type af_IEnumConnections_Release is access
     function (This : access IEnumConnections)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumConnections_Release);
   pragma Machine_Attribute (af_IEnumConnections_Release, "ms_abi");

   type af_IEnumConnections_Next is access
     function (This         : access IEnumConnections;
               cConnections :        Win32_Types.Unsigned_Long;
               rgcd         :        Pointer_To_CONNECTDATA;
               LpcFetched   :        Pointer_To_Void)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Next);
   pragma Machine_Attribute (af_IEnumConnections_Next, "ms_abi");

   type af_IEnumConnections_Skip is access
     function (This         : access IEnumConnections;
               cConnections :        Win32_Types.Unsigned_Long)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Skip);
   pragma Machine_Attribute (af_IEnumConnections_Skip, "ms_abi");

   type af_IEnumConnections_Reset is access
     function (This : access IEnumConnections)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Reset);
   pragma Machine_Attribute (af_IEnumConnections_Reset, "ms_abi");

   type af_IEnumConnections_Clone is access
     function (This   : access IEnumConnections;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnections)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Clone);
   pragma Machine_Attribute (af_IEnumConnections_Clone, "ms_abi");

   type IEnumConnectionsVtbl;
   type Pointer_To_IEnumConnectionsVtbl is
     access all IEnumConnectionsVtbl;

   type IEnumConnections is
      record
         Vtbl : Pointer_To_IEnumConnectionsVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumConnections);

   type IEnumConnectionsVtbl is
      record
         QueryInterface : af_IEnumConnections_QueryInterface;
         AddRef         : af_IEnumConnections_AddRef;
         Release        : af_IEnumConnections_Release;
         Next           : af_IEnumConnections_Next;
         Skip           : af_IEnumConnections_Skip;
         Reset          : af_IEnumConnections_Reset;
         Clone          : af_IEnumConnections_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumConnectionsVtbl);

   --  IEnumVARIANT Interface
   --  {00020404-0000-0000-C000-000000000046}

   IID_IEnumVARIANT : aliased GUID :=
     (132100, 0, 0,
       (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_IEnumVARIANT_QueryInterface is access
     function (This   : access IEnumVARIANT;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_QueryInterface);
   pragma Machine_Attribute (af_IEnumVARIANT_QueryInterface, "ms_abi");

   type af_IEnumVARIANT_AddRef is access
     function (This : access IEnumVARIANT)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumVARIANT_AddRef);
   pragma Machine_Attribute (af_IEnumVARIANT_AddRef, "ms_abi");

   type af_IEnumVARIANT_Release is access
     function (This : access IEnumVARIANT)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumVARIANT_Release);
   pragma Machine_Attribute (af_IEnumVARIANT_Release, "ms_abi");

   type af_IEnumVARIANT_Next is access
     function (This         : access IEnumVARIANT;
               celt         : Win32_Types.Long;
               rgvar        : access VARIANT;
               pceltFetched : access Win32_Types.Long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Next);
   pragma Machine_Attribute (af_IEnumVARIANT_Next, "ms_abi");

   type af_IEnumVARIANT_Skip is access
     function (This : access IEnumVARIANT;
               celt : Win32_Types.Long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Skip);
   pragma Machine_Attribute (af_IEnumVARIANT_Skip, "ms_abi");

   type af_IEnumVARIANT_Reset is access
     function (This : access IEnumVARIANT)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Reset);
   pragma Machine_Attribute (af_IEnumVARIANT_Reset, "ms_abi");

   type af_IEnumVARIANT_Clone is access
     function (This   : access IEnumVARIANT;
               ppenum : access Pointer_To_IEnumVARIANT)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Clone);
   pragma Machine_Attribute (af_IEnumVARIANT_Clone, "ms_abi");

   type IEnumVARIANTVtbl;
   type Pointer_To_IEnumVARIANTVtbl is access all IEnumVARIANTVtbl;

   type IEnumVARIANT is
      record
         Vtbl : Pointer_To_IEnumVARIANTVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumVARIANT);

   type IEnumVARIANTVtbl is
      record
         QueryInterface : af_IEnumVARIANT_QueryInterface;
         AddRef         : af_IEnumVARIANT_AddRef;
         Release        : af_IEnumVARIANT_Release;
         Next           : af_IEnumVARIANT_Next;
         Skip           : af_IEnumVARIANT_Skip;
         Reset          : af_IEnumVARIANT_Reset;
         Clone          : af_IEnumVARIANT_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumVARIANTVtbl);

   --  {0002E005-0000-0000-C000-000000000046}

   CLSID_StdComponentCategoriesMgr : aliased GUID :=
     (188421, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   --  {0002E012-0000-0000-C000-000000000046}

   IID_ICatRegister : aliased GUID :=
     (188434, 0, 0,
      (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
       C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   --  StdGlobalInterfaceTable CoClass
   --  {00000323-0000-0000-C000-000000000046}

   CLSID_StdGlobalInterfaceTable : aliased GUID :=
     (803, 0, 0,
       (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   --  IGlobalInterfaceTable Interface
   --  {00000146-0000-0000-C000-000000000046}

   IID_IGlobalInterfaceTable : aliased GUID :=
     (326, 0, 0,
       (C.unsigned_char'Val (192), C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (0),
        C.unsigned_char'Val (0),   C.unsigned_char'Val (70)));

   type af_IGlobalInterfaceTable_QueryInterface is access
     function (This   : access IGlobalInterfaceTable;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IGlobalInterfaceTable_QueryInterface);
   pragma Machine_Attribute (af_IGlobalInterfaceTable_QueryInterface, "ms_abi");

   type af_IGlobalInterfaceTable_AddRef is access
     function (This : access IGlobalInterfaceTable)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IGlobalInterfaceTable_AddRef);
   pragma Machine_Attribute (af_IGlobalInterfaceTable_AddRef, "ms_abi");

   type af_IGlobalInterfaceTable_Release is access
     function (This : access IGlobalInterfaceTable)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IGlobalInterfaceTable_Release);
   pragma Machine_Attribute (af_IGlobalInterfaceTable_Release, "ms_abi");

   type af_IGlobalInterfaceTable_RegisterInterfaceInGlobal is access
     function (This      : access IGlobalInterfaceTable;
               pUnk      : Pointer_To_IUnknown;
               riid      : Pointer_To_GUID;
               pdwCookie : Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall,
                        af_IGlobalInterfaceTable_RegisterInterfaceInGlobal);

   type af_IGlobalInterfaceTable_RevokeInterfaceFromGlobal is access
     function (This     : access IGlobalInterfaceTable;
               dwCookie : Win32_Types.Unsigned_Long)
     return HRESULT;
   pragma Convention (StdCall,
                        af_IGlobalInterfaceTable_RevokeInterfaceFromGlobal);

   type af_IGlobalInterfaceTable_GetInterfaceFromGlobal is access
     function (This     : access IGlobalInterfaceTable;
               dwCookie : Win32_Types.Unsigned_Long;
               riid     : Pointer_To_GUID;
               ppv      : Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall,
                        af_IGlobalInterfaceTable_GetInterfaceFromGlobal);

   type IGlobalInterfaceTableVtbl;
   type Pointer_To_IGlobalInterfaceTableVtbl is
     access all IGlobalInterfaceTableVtbl;

   type IGlobalInterfaceTable is
      record
         Vtbl : Pointer_To_IGlobalInterfaceTableVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IGlobalInterfaceTable);

   type IGlobalInterfaceTableVtbl is
      record
         QueryInterface            : af_IGlobalInterfaceTable_QueryInterface;
         AddRef                    : af_IGlobalInterfaceTable_AddRef;
         Release                   : af_IGlobalInterfaceTable_Release;
         RegisterInterfaceInGlobal :
           af_IGlobalInterfaceTable_RegisterInterfaceInGlobal;
         RevokeInterfaceFromGlobal :
           af_IGlobalInterfaceTable_RevokeInterfaceFromGlobal;
         GetInterfaceFromGlobal    :
           af_IGlobalInterfaceTable_GetInterfaceFromGlobal;
      end record;
   pragma Convention (C_Pass_By_Copy, IGlobalInterfaceTableVtbl);

   type af_ICatRegister_QueryInterface is access
     function (This   : access ICatRegister;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_AddRef is access
     function (This : access ICatRegister)
               return Win32_Types.Unsigned_Long
     with Convention => Stdcall;

   type af_ICatRegister_Release is access
     function (This : access ICatRegister)
               return Win32_Types.Unsigned_Long;

   type af_ICatRegister_RegisterCategories is access
     function (This           : access ICatRegister;
               cCategories    : ULONG;
               rgCategoryInfo : access CATEGORYINFO)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_UnRegisterCategories is access
     function (This        : access ICatRegister;
               cCategories : ULONG;
               rgcatid     : access GUID)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_RegisterClassImplCategories is access
     function (This        : access ICatRegister;
               rclsid      : access GUID;
               cCategories : ULONG;
               rgcatid     : access GUID)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_UnRegisterClassImplCategories is access
     function (This        : access ICatRegister;
               rclsid      : access GUID;
               cCategories : ULONG;
               rgcatid     : access GUID)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_RegisterClassReqCategories is access
     function (This        : access ICatRegister;
               rclsid      : access GUID;
               cCategories : ULONG;
               rgcatid     : access GUID)
               return HRESULT
     with Convention => Stdcall;

   type af_ICatRegister_UnRegisterClassReqCategories is access
     function (This        : access ICatRegister;
               rclsid      : access GUID;
               cCategories : ULONG;
               rgcatid     : access GUID)
               return HRESULT
     with Convention => Stdcall;

   type ICatRegisterVtbl;
   type Pointer_To_ICatRegisterVtbl is access all ICatRegisterVtbl;

   type ICatRegister is
      record
         Vtbl : Pointer_To_ICatRegisterVtbl;
      end record
     with Convention => C_Pass_By_Copy;

   type ICatRegisterVtbl is
      record
         QueryInterface                : af_ICatRegister_QueryInterface;
         AddRef                        : af_ICatRegister_AddRef;
         Release                       : af_ICatRegister_Release;
         RegisterCategories            : af_ICatRegister_RegisterCategories;
         UnRegisterCategories          : af_ICatRegister_UnRegisterCategories;
         RegisterClassImplCategories   :
         af_ICatRegister_RegisterClassImplCategories;
         UnRegisterClassImplCategories :
         af_ICatRegister_UnRegisterClassImplCategories;
         RegisterClassReqCategories    :
         af_ICatRegister_RegisterClassReqCategories;
         UnRegisterClassReqCategories  :
         af_ICatRegister_UnRegisterClassReqCategories;
      end record
     with Convention => C_Pass_By_Copy;

   --  IPropertyNotifySink Interface
   --  {9BFBBC02-EFF1-101A-84ED-00AA00341D07}
   IID_IPropertyNotifySink : aliased GUID :=
     (16#9BFBBC02#, 16#EFF1#, 16#101A#,
      (C.unsigned_char'Val (16#84#), C.unsigned_char'Val (16#ED#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#34#),
       C.unsigned_char'Val (16#1D#), C.unsigned_char'Val (16#07#)));

   type af_IPropertyNotifySink_QueryInterface is access
     function (This   : access IPropertyNotifySink;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
               return HRESULT
     with Convention => Stdcall;

   type af_IPropertyNotifySink_AddRef is access
     function (This : access IPropertyNotifySink)
               return Win32_Types.Unsigned_Long
     with Convention => Stdcall;

   type af_IPropertyNotifySink_Release is access
     function (This : access IPropertyNotifySink)
               return Win32_Types.Unsigned_Long;

   type af_IPropertyNotifySink_OnChanged is access
     function (This   : access IPropertyNotifySink;
               dispID : Win32_Types.Long)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IPropertyNotifySink_OnRequestEdit is access
     function (This   : access IPropertyNotifySink;
               dispID : Win32_Types.Long)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type IPropertyNotifySinkVtbl;
   type Pointer_To_IPropertyNotifySinkVtbl is access all IPropertyNotifySinkVtbl;

   type IPropertyNotifySink is
      record
         Vtbl : Pointer_To_IPropertyNotifySinkVtbl;
      end record
     with Convention => C_Pass_By_Copy;

   type IPropertyNotifySinkVtbl is
      record
         QueryInterface                : af_IPropertyNotifySink_QueryInterface;
         AddRef                        : af_IPropertyNotifySink_AddRef;
         Release                       : af_IPropertyNotifySink_Release;
         OnChanged                     : af_IPropertyNotifySink_OnChanged;
         OnRequestEdit                 : af_IPropertyNotifySink_OnRequestEdit;
      end record
     with Convention => C_Pass_By_Copy;

   --  {157083E1-2368-11cf-87B9-00AA006C8166}
   CATID_PropertyNotifyControl : aliased GUID :=
     (16#157083E1#, 16#2368#, 16#11CF#,
      (C.unsigned_char'Val (16#87#), C.unsigned_char'Val (16#B9#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#6C#),
       C.unsigned_char'Val (16#81#), C.unsigned_char'Val (16#66#)));

   --  {6d5140c1-7436-11ce-8034-00aa006009fa}
   IID_IServiceProvider : aliased GUID :=
     (16#6d5140c1#, 16#7436#, 16#11CE#,
      (C.unsigned_char'Val (16#80#), C.unsigned_char'Val (16#34#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#AA#),
       C.unsigned_char'Val (0),      C.unsigned_char'Val (16#60#),
       C.unsigned_char'Val (16#09#), C.unsigned_char'Val (16#FA#)));

   type af_IServiceProvider_QueryInterface is access
     function (This   : access IServiceProvider;
               riid   : Pointer_To_GUID;
               ppvObj : Pointer_To_Pointer_To_Void)
               return HRESULT
     with Convention => Stdcall;

   type af_IServiceProvider_AddRef is access
     function (This : access IServiceProvider)
               return Win32_Types.Unsigned_Long
     with Convention => Stdcall;

   type af_IServiceProvider_Release is access
     function (This : access IServiceProvider)
               return Win32_Types.Unsigned_Long;

   type af_IServiceProvider_QueryService is access
     function (This        : access IServiceProvider;
               guidService : Pointer_To_GUID;
               ppvObject   : Pointer_To_Pointer_To_Void)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type IServiceProviderVtbl;
   type Pointer_To_IServiceProviderVtbl is access all IServiceProviderVtbl;

   type IServiceProvider is
      record
         Vtbl : Pointer_To_IServiceProviderVtbl;
      end record
     with Convention => C_Pass_By_Copy;

   type IServiceProviderVtbl is
      record
         QueryInterface                : af_IServiceProvider_QueryInterface;
         AddRef                        : af_IServiceProvider_AddRef;
         Release                       : af_IServiceProvider_Release;
         QueryService                  : af_IServiceProvider_QueryService;
      end record
     with Convention => C_Pass_By_Copy;

end GNATCOM.Types;
