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

--  Thin bindings to COM Types

with Interfaces.C;
with System;

package GNATCOM.Types is

   package C renames Interfaces.C;

   type HRESULT is new Interfaces.C.unsigned_long;
   type Pointer_To_HRESULT is access all HRESULT;
   subtype SCODE is HRESULT;
   type Pointer_To_SCODE is access all SCODE;
   --  HRESULTs are used to return the success or failure of Interface
   --  methods and COM APIs. See GNATCOM.Errors

   type Void is null record;
   subtype Pointer_To_Void is System.Address;
   type Pointer_To_Pointer_To_Void is access all Pointer_To_Void;
   --  C style Void pointers

   type Pointer_To_char is access all Interfaces.C.char;
   type Pointer_To_Pointer_To_char is access all Interfaces.C.char;
   type Pointer_To_wchar_t is access all Interfaces.C.wchar_t;
   subtype LPSTR is Pointer_To_char;
   type Pointer_To_LPSTR is access all LPSTR;
   subtype LPWSTR is Pointer_To_wchar_t;
   pragma No_Strict_Aliasing (LPWSTR);
   type Pointer_To_LPWSTR is access all LPWSTR;
   --  C style strings

   function To_C (From : access Interfaces.C.wchar_t)
     return Interfaces.C.wchar_array;
   function To_Ada (From : access Interfaces.C.wchar_t) return String;
   function To_Ada (From : access Interfaces.C.wchar_t) return Wide_String;
   --  Convert pointers to C wide strings to usable types

   Size_Of_Currency : constant := 64;

   type CURRENCY is
      record
         Lo : Interfaces.C.unsigned_long;
         Hi : Interfaces.C.long;
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
   subtype bool is Interfaces.C.long;
   type Pointer_To_bool is access all bool;

   type DECIMAL is
      record
         wReserved : Interfaces.C.unsigned_short;
         scale     : BYTE;
         sign      : BYTE;
         Hi32      : Interfaces.C.unsigned_long;
         Lo32      : Interfaces.C.unsigned_long;
         Mid32     : Interfaces.C.unsigned_long;
      end record;
   pragma Convention (C_Pass_By_Copy, DECIMAL);
   type Pointer_To_DECIMAL is access all DECIMAL;

   Size_Of_BLOB : constant := 2 * Standard'Address_Size;

   type BLOB is
      record
         cbSize    : Interfaces.C.unsigned_long;
         pBlobData : Pointer_To_BYTE;
      end record;
   pragma Convention (C_Pass_By_Copy, BLOB);
   for BLOB'Size use Size_Of_BLOB;
   type Pointer_To_BLOB is access all BLOB;
   --  Automation type - (B)inary (L)arge (OB)ject

   type SAFEARRAYBOUND is
      record
         cElements : Interfaces.C.unsigned_long;
         lLbound   : Interfaces.C.long;
      end record;
   pragma Convention (C_Pass_By_Copy, SAFEARRAYBOUND);
   type Pointer_To_SAFEARRAYBOUND is access all SAFEARRAYBOUND;

   type SAFEARRAY is
      record
         cDims      : Interfaces.C.short;
         fFeatures  : Interfaces.C.short;
         cbElements : Interfaces.C.unsigned_long;
         cLocks     : Interfaces.C.unsigned_long;
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
         Data1 : Interfaces.C.unsigned_long;
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
   type IClassFactory;
   type IClassFactory2;
   type IConnectionPointContainer;
   type IConnectionPoint;
   type IEnumConnectionPoints;
   type IEnumConnections;
   type IEnumVARIANT;
   type IGlobalInterfaceTable;
   --  Forward references to COM Interfaces and types

   type Pointer_To_int is access all Interfaces.C.int;
   type Pointer_To_short is access all Interfaces.C.short;
   type Pointer_To_long is access all Interfaces.C.long;
   type Pointer_To_double is access all Interfaces.C.double;
   type Pointer_To_C_float is access all Interfaces.C.C_float;
   type Pointer_To_unsigned is access all Interfaces.C.unsigned;
   type Pointer_To_unsigned_long is access all Interfaces.C.unsigned_long;
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
   type Pointer_To_Pointer_To_IEnumVARIANT is access all IEnumVARIANT;
   type Pointer_To_IGlobalInterfaceTable is access all IGlobalInterfaceTable;
   pragma No_Strict_Aliasing (Pointer_To_IGlobalInterfaceTable);
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
               lVal      : Interfaces.C.long;
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
               ulVal     : Interfaces.C.unsigned_long;
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
     aliased Interfaces.C.long;
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
         dwHelpContext     : Interfaces.C.long;
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
   subtype CLSCTX is Interfaces.C.long;
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

   type af_IUnknown_AddRef is access
     function (This : access IUnknown) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IUnknown_AddRef);

   type af_IUnknown_Release is access
     function (This : access IUnknown) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IUnknown_Release);

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

   type af_IDispatch_AddRef is access
     function (This : access IDispatch) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IDispatch_AddRef);

   type af_IDispatch_Release is access
     function (This : access IDispatch) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IDispatch_Release);

   type af_IDispatch_GetTypeInfoCount is access
     function (This    : access IDispatch;
               pctinfo : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetTypeInfoCount);

   type af_IDispatch_GetTypeInfo is access
     function (This    : access IDispatch;
               itinfo  : in     Interfaces.C.int;
               lcid    : in     Interfaces.C.long;
               pptinfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetTypeInfo);

   type af_IDispatch_GetIDsOfNames is access
     function (This      : access IDispatch;
               riid      : in     Pointer_To_GUID;
               rgszNames : in     Pointer_To_LPWSTR;
               cNames    : in     Interfaces.C.int;
               lcid      : in     Interfaces.C.long;
               rgdispid  : in     Pointer_To_long)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_GetIDsOfNames);

   type af_IDispatch_Invoke is access
     function (This         : access IDispatch;
               dispidMember : in     Interfaces.C.long;
               riid         : in     Pointer_To_GUID;
               lcid         : in     Interfaces.C.long;
               wFlags       : in     Interfaces.C.short;
               pdispparams  : in     Pointer_To_DISPPARAMS;
               pvarResult   : in     Pointer_To_VARIANT;
               pexcepinfo   : in     Pointer_To_EXCEPINFO;
               puArgErr     : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_IDispatch_Invoke);

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
   subtype TYPEKIND is Interfaces.C.long;
   type Pointer_To_TYPEKIND is access all TYPEKIND;
   --  Identifies TypeInfo Kind

   SYS_WIN16 : constant := 0;
   SYS_WIN32 : constant := 1;
   SYS_MAC   : constant := 2;
   subtype SYSKIND is Interfaces.C.long;
   type Pointer_TO_SYSKIND is access all SYSKIND;
   --  Identifies system type

   DESCKIND_NONE           : constant := 0;
   DESCKIND_FUNCDESC       : constant := 1;
   DESCKIND_VARDESC        : constant := 2;
   DESCKIND_TYPECOMP       : constant := 3;
   DESCKIND_IMPLICITAPPOBJ : constant := 4;
   DESCKIND_MAX            : constant := 5;
   subtype DESCKIND is Interfaces.C.long;
   type Pointer_To_DESCKIND is access all DESCKIND;

   FUNC_VIRTUAL            : constant := 0;
   FUNC_PUREVIRTUAL        : constant := 1;
   FUNC_NONVIRTUAL         : constant := 2;
   FUNC_STATIC             : constant := 3;
   FUNC_DISPATCH           : constant := 4;
   subtype FUNCKIND is Interfaces.C.long;
   type Pointer_To_FUNCKIND is access all FUNCKIND;

   INVOKE_FUNC             : constant := 1;
   INVOKE_PROPERTYGET      : constant := 2;
   INVOKE_PROPERTYPUT      : constant := 4;
   INVOKE_PROPERTYPUTREF   : constant := 8;
   subtype INVOKEKIND is Interfaces.C.long;
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
   subtype CALLCONV is Interfaces.C.long;
   type Pointer_To_CALLCONV is access all CALLCONV;

   VAR_PERINSTANCE : constant := 0;
   VAR_STATIC      : constant := 1;
   VAR_CONST       : constant := 2;
   VAR_DISPATCH    : constant := 3;
   subtype VARKIND is Interfaces.C.long;
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
               hreftype : Interfaces.C.unsigned_long;
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
         dwReserved : Interfaces.C.unsigned_long;
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
         lcid             : Interfaces.C.unsigned_long;
         dwReserved       : Interfaces.C.unsigned_long;
         memidConstructor : Interfaces.C.long;
         memidDestructor  : Interfaces.C.long;
         lpstrSchema      : LPWSTR;
         cbSizeInstance   : Interfaces.C.unsigned_long;
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
         memid             : Interfaces.C.long;
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
               oInst : Interfaces.C.unsigned_long;
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
         memid           : Interfaces.C.long;
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
         lcid            : Interfaces.C.unsigned_long;
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
         cbLicInfo        : Interfaces.C.long;
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

   type af_ITypeLib_AddRef is access
     function (This : access ITypeLib) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeLib_AddRef);

   type af_ITypeLib_Release is access
     function (This : access ITypeLib) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeLib_Release);

   type af_ITypeLib_GetTypeInfoCount is access
     function (This    : access ITypeLib)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoCount);

   type af_ITypeLib_GetTypeInfo is access
     function (This    : access ITypeLib;
               index   : in     Interfaces.C.int;
               ppTInfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfo);

   type af_ITypeLib_GetTypeInfoType is access
     function (This   : access ITypeLib;
               index  : in     Interfaces.C.int;
               pTKind : in     Pointer_To_TYPEKIND)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoType);

   type af_ITypeLib_GetTypeInfoOfGuid is access
     function (This    : access ITypeLib;
               guid    : in     Pointer_To_GUID;
               ppTInfo : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeInfoOfGuid);

   type af_ITypeLib_GetLibAttr is access
     function (This       : access ITypeLib;
               ppTLibAttr : in     Pointer_To_Pointer_To_TLIBATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetLibAttr);

   type af_ITypeLib_GetTypeComp is access
     function (This    : access ITypeLib;
               ppTComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetTypeComp);

   type af_ITypeLib_GetDocumentation is access
     function (This           : access ITypeLib;
               index          : in     Interfaces.C.int;
               pBstrName      : in     Pointer_To_BSTR;
               pBstrDocString : in     Pointer_To_BSTR;
               pdwHelpContext : in     Pointer_To_unsigned_long;
               pBstrHelpFile  : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_GetDocumentation);

   type af_ITypeLib_IsName is access
     function (This         : access ITypeLib;
               szNameBuf    : in     LPWSTR;
               lHashVal     : in     Interfaces.C.unsigned_long;
               pfName       : in     Pointer_To_bool)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_IsName);

   type af_ITypeLib_FindName is access
     function (This         : access ITypeLib;
               szNameBuf    : in     LPWSTR;
               lHashVal     : in     Interfaces.C.unsigned_long;
               ppTInfo      : in     Pointer_To_Pointer_To_ITypeInfo;
               rgMemId      : in     Pointer_To_long;
               pcFound      : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_FindName);

   type af_ITypeLib_ReleaseTLibAttr is access
     function (This      : access ITypeLib;
               pTLibAttr : Pointer_To_TLIBATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeLib_ReleaseTLibAttr);

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

   type af_ITypeInfo_AddRef is access
     function (This : access ITypeInfo) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeInfo_AddRef);

   type af_ITypeInfo_Release is access
     function (This : access ITypeInfo) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeInfo_Release);

   type af_ITypeInfo_GetTypeAttr is access
     function (This       : access ITypeInfo;
               ppTypeAttr : in     Pointer_To_Pointer_To_TYPEATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetTypeAttr);

   type af_ITypeInfo_GetTypeComp is access
     function (This    : access ITypeInfo;
               ppTComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetTypeComp);

   type af_ITypeInfo_GetFuncDesc is access
     function (This       : access ITypeInfo;
               index      : in     Interfaces.C.int;
               ppFuncDesc : in     Pointer_To_Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetFuncDesc);

   type af_ITypeInfo_GetVarDesc is access
     function (This      : access ITypeInfo;
               index     : in     Interfaces.C.int;
               ppVarDesc : in     Pointer_To_Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetVarDesc);

   type af_ITypeInfo_GetNames is access
     function (This        : access ITypeInfo;
               memid       : in     Interfaces.C.long;
               rgBstrNames : in     Pointer_To_BSTR_PARAM_ARRAY;
               cMaxNames   : in     Interfaces.C.int;
               pcNames     : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetNames);

   type af_ITypeInfo_GetRefTypeOfImplType is access
     function (This     : access ITypeInfo;
               index    : in     Interfaces.C.int;
               pRefType : in     Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetRefTypeOfImplType);

   type af_ITypeInfo_GetImplTypeFlags is access
     function (This           : access ITypeInfo;
               index          : in     Interfaces.C.int;
               pImplTypeFlags : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetImplTypeFlags);

   type af_ITypeInfo_GetIDsOfNames is access
     function (This      : access ITypeInfo;
               rgszNames : in     Pointer_To_LPWSTR;
               cNames    : in     Interfaces.C.int;
               pMemId    : in     Pointer_To_long)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetIDsOfNames);

   type af_ITypeInfo_Invoke is access
     function (This        : access ITypeInfo;
               pvInstance  : in Pointer_To_Void;
               memid       : in     Interfaces.C.long;
               wFlags      : in     Interfaces.C.short;
               pdispparams : in     Pointer_To_DISPPARAMS;
               pvarResult  : in     Pointer_To_VARIANT;
               pexcepinfo  : in     Pointer_To_EXCEPINFO;
               puArgErr    : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_Invoke);

   type af_ITypeInfo_GetDocumentation is access
     function (This           : access ITypeInfo;
               memid          : in     Interfaces.C.long;
               pBstrName      : in     Pointer_To_BSTR;
               pBstrDocString : in     Pointer_To_BSTR;
               pdwHelpContext : in     Pointer_To_unsigned_long;
               pBstrHelpFile  : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetDocumentation);

   type af_ITypeInfo_GetDllEntry is access
     function (This         : access ITypeInfo;
               memid        : in     Interfaces.C.long;
               invkind      : in     INVOKEKIND;
               pBstrDllName : in     Pointer_To_BSTR;
               pBstrName    : in     Pointer_To_BSTR;
               pwOrdinal    : in     Pointer_To_short)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetDllEntry);

   type af_ITypeInfo_GetRefTypeInfo is access
     function (This     : access ITypeInfo;
               hreftype : in     Interfaces.C.unsigned_long;
               ppTInfo  : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetRefTypeInfo);

   type af_ITypeInfo_AddressOfMember is access
     function (This    : access ITypeInfo;
               memid   : in     Interfaces.C.long;
               invkind : in     INVOKEKIND;
               ppv     : in     Pointer_To_Pointer_To_Void)
    return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_AddressOfMember);

   type af_ITypeInfo_CreateInstance is access
     function (This   : access ITypeInfo;
               riid   : in     Pointer_To_GUID;
               ppvObj : in     Pointer_To_Pointer_To_IUnknown)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_CreateInstance);

   type af_ITypeInfo_GetMops is access
     function (This      : access ITypeInfo;
               memid     : in     Interfaces.C.long;
               pBstrMops : in     Pointer_To_BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetMops);

   type af_ITypeInfo_GetContainingTypeLib is access
     function (This   : access ITypeInfo;
               ppTLib : in     Pointer_To_Pointer_To_ITypeLib;
               pIndex : in     Pointer_To_int)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_GetContainingTypeLib);

   type af_ITypeInfo_ReleaseTypeAttr is access
     function (This      : access ITypeInfo;
               pTypeAttr : in     Pointer_To_TYPEATTR)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseTypeAttr);

   type af_ITypeInfo_ReleaseFuncDesc is access
     function (This      : access ITypeInfo;
               pFuncDesc : in     Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseFuncDesc);

   type af_ITypeInfo_ReleaseVarDesc is access
     function (This     : access ITypeInfo;
               pVarDesc : in     Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeInfo_ReleaseVarDesc);

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

   type af_ITypeComp_AddRef is access
     function (This : access ITypeComp) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeComp_AddRef);

   type af_ITypeComp_Release is access
     function (This : access ITypeComp) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITypeComp_Release);

   type af_ITypeComp_Bind is access
     function (This       : access ITypeComp;
               szName     : in     LPWSTR;
               lHashVal   : in     Interfaces.C.unsigned_long;
               wFlags     : in     Interfaces.C.short;
               ppTInfo    : in     Pointer_To_Pointer_To_ITypeInfo;
               pDescKind  : in     Pointer_To_DESCKIND;
               ppFuncDesc : in     Pointer_To_Pointer_To_FUNCDESC;
               ppVarDesc  : in     Pointer_To_Pointer_To_VARDESC;
               ppTypeComp : in     Pointer_To_Pointer_To_ITypeComp)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeComp_Bind);

   type af_ITypeComp_BindType is access
     function (This     : access ITypeComp;
               SzName   : in     LPWSTR;
               lHashVal : in     Interfaces.C.unsigned_long;
               ppTInfo  : in     Pointer_To_Pointer_To_ITypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ITypeComp_BindType);

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

   type af_ICreateTypeInfo_AddRef is access
     function (This : access ICreateTypeInfo)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddRef);

   type af_ICreateTypeInfo_Release is access
     function (This : access ICreateTypeInfo)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateTypeInfo_Release);

   type af_ICreateTypeInfo_SetGuid is access
     function (This : access ICreateTypeInfo;
               guid : Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetGuid);

   type af_ICreateTypeInfo_SetTypeFlags is access
     function (This       : access ICreateTypeInfo;
               uTypeFlags : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeFlags);

   type af_ICreateTypeInfo_SetDocString is access
     function (This    : access ICreateTypeInfo;
               pStrDoc : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetDocString);

   type af_ICreateTypeInfo_SetHelpContext is access
     function (This          : access ICreateTypeInfo;
               dwHelpContext : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetHelpContext);

   type af_ICreateTypeInfo_SetVersion is access
     function (This         : access ICreateTypeInfo;
               wMajorVerNum : Interfaces.C.unsigned_short;
               wMinorVerNum : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVersion);

   type af_ICreateTypeInfo_AddRefTypeInfo is access
     function (This      : access ICreateTypeInfo;
               pTInfo    : Pointer_To_ITypeInfo;
               phRefType : Pointer_To_unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddRefTypeInfo);

   type af_ICreateTypeInfo_AddFuncDesc is access
     function (This      : access ICreateTypeInfo;
               index     : Interfaces.C.unsigned;
               pFuncDesc : Pointer_To_FUNCDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddFuncDesc);

   type af_ICreateTypeInfo_AddImplType is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               hreftype : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddImplType);

   type af_ICreateTypeInfo_SetImplTypeFlags is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               implTypeFlags : Interfaces.C.int)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetImplTypeFlags);

   type af_ICreateTypeInfo_SetAlignment is access
     function (This        : access ICreateTypeInfo;
               cbAlignment : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetAlignment);

   type af_ICreateTypeInfo_SetSchema is access
     function (This       : access ICreateTypeInfo;
               pStrSchema : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetSchema);

   type af_ICreateTypeInfo_AddVarDesc is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               pVarDesc : Pointer_To_VARDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_AddVarDesc);

   type af_ICreateTypeInfo_SetFuncAndParamNames is access
     function (This      : access ICreateTypeInfo;
               index     : Interfaces.C.unsigned;
               rgszNames : Pointer_To_LPWSTR;
               cNames    : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncAndParamNames);

   type af_ICreateTypeInfo_SetVarName is access
     function (This   : access ICreateTypeInfo;
               index  : Interfaces.C.unsigned;
               szName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarName);

   type af_ICreateTypeInfo_SetTypeDescAlias is access
     function (This        : access ICreateTypeInfo;
               pTDescAlias : Pointer_To_TYPEDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeDescAlias);

   type af_ICreateTypeInfo_DefineFuncAsDllEntry is access
     function (This       : access ICreateTypeInfo;
               index      : Interfaces.C.unsigned;
               szDllName  : LPWSTR;
               szProcName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_DefineFuncAsDllEntry);

   type af_ICreateTypeInfo_SetFuncDocString is access
     function (This        : access ICreateTypeInfo;
               index       : Interfaces.C.unsigned;
               szDocString : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncDocString);

   type af_ICreateTypeInfo_SetVarDocString is access
     function (This        : access ICreateTypeInfo;
               index       : Interfaces.C.unsigned;
               szDocString : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarDocString);

   type af_ICreateTypeInfo_SetFuncHelpContext is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               dwHelpContext : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetFuncHelpContext);

   type af_ICreateTypeInfo_SetVarHelpContext is access
     function (This          : access ICreateTypeInfo;
               index         : Interfaces.C.unsigned;
               dwHelpContext : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetVarHelpContext);

   type af_ICreateTypeInfo_SetMops is access
     function (This     : access ICreateTypeInfo;
               index    : Interfaces.C.unsigned;
               bstrMops : BSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetMops);

   type af_ICreateTypeInfo_SetTypeIdldesc is access
     function (This     : access ICreateTypeInfo;
               pIdlDesc : Pointer_To_IDLDESC)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_SetTypeIdldesc);

   type af_ICreateTypeInfo_LayOut is access
     function (This : access ICreateTypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeInfo_LayOut);

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

   type af_ICreateTypeLib_AddRef is access
     function (This : access ICreateTypeLib)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateTypeLib_AddRef);

   type af_ICreateTypeLib_Release is access
     function (This : access ICreateTypeLib)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateTypeLib_Release);

   type af_ICreateTypeLib_CreateTypeInfo is access
     function (This     : access ICreateTypeLib;
               szName   : LPWSTR;
               tkind    : TYPEKIND;
               ppCTInfo : Pointer_To_Pointer_To_ICreateTypeInfo)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_CreateTypeInfo);

   type af_ICreateTypeLib_SetName is access
     function (This   : access ICreateTypeLib;
               szName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetName);

   type af_ICreateTypeLib_SetVersion is access
     function (This         : access ICreateTypeLib;
               wMajorVerNum : Interfaces.C.unsigned_short;
               wMinorVerNum : Interfaces.C.unsigned_short)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetVersion);

   type af_ICreateTypeLib_SetGuid is access
     function (This : access ICreateTypeLib;
               guid : Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetGuid);

   type af_ICreateTypeLib_SetDocString is access
     function (This  : access ICreateTypeLib;
               szDoc : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetDocString);

   type af_ICreateTypeLib_SetHelpFileName is access
     function (This           : access ICreateTypeLib;
               szHelpFileName : LPWSTR)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetHelpFileName);

   type af_ICreateTypeLib_SetHelpContext is access
     function (This          : access ICreateTypeLib;
               dwHelpContext : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetHelpContext);

   type af_ICreateTypeLib_SetLcid is access
     function (This : access ICreateTypeLib;
               lcid : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetLcid);

   type af_ICreateTypeLib_SetLibFlags is access
     function (This      : access ICreateTypeLib;
               uLibFlags : Interfaces.C.unsigned)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SetLibFlags);

   type af_ICreateTypeLib_SaveAllChanges is access
     function (This : access ICreateTypeLib)
     return HRESULT;
   pragma Convention (StdCall, af_ICreateTypeLib_SaveAllChanges);

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

   type af_IClassFactory2_AddRef is access
     function (This : access IClassFactory2) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IClassFactory2_AddRef);

   type af_IClassFactory2_Release is access
     function (This : access IClassFactory2) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IClassFactory2_Release);

   type af_IClassFactory2_CreateInstance is access
     function (This      : access IClassFactory2;
               pUnkOuter : in     Pointer_To_IUnknown;
               riid      : in     Pointer_To_GUID;
               ppvObject : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_CreateInstance);

   type af_IClassFactory2_LockServer is access
     function (This  : access IClassFactory2;
               fLock : in     bool)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_LockServer);

   type af_IClassFactory2_GetLicInfo is access
     function (This     : access IClassFactory2;
               pLicInfo : in     Pointer_To_LICINFO)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_GetLicInfo);

   type af_IClassFactory2_RequestLicKey is access
     function (This       : access IClassFactory2;
               dwReserved : in     DWORD;
               pLicInfo   : in     Pointer_To_BSTR)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_RequestLicKey);

   type af_IClassFactory2_CreateInstanceLic is access
     function (This         : access IClassFactory2;
               pUnkOuter    : in     Pointer_To_IUnknown;
               pUnkReserved : in     Pointer_To_IUnknown;
               riid         : in     Pointer_To_GUID;
               bstrKey      : in     BSTR;
               ppv          : in     Pointer_To_Pointer_To_Void)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory2_CreateInstanceLic);

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

   type af_IClassFactory_AddRef is access
     function (This : access IClassFactory) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IClassFactory_AddRef);

   type af_IClassFactory_Release is access
     function (This : access IClassFactory) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IClassFactory_Release);

   type af_IClassFactory_CreateInstance is access
     function (This      : access IClassFactory;
               pUnkOuter : in     Pointer_To_IUnknown;
               riid      : in     Pointer_To_GUID;
               ppvObject : in     Pointer_To_Pointer_To_Void)
     return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory_CreateInstance);

   type af_IClassFactory_LockServer is access
     function (This  : access IClassFactory;
               fLock : in     bool)
      return HRESULT;
   pragma Convention (Stdcall, af_IClassFactory_LockServer);

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

   type af_IConnectionPointContainer_AddRef is access
     function (This : access IConnectionPointContainer)
     return Interfaces.C.unsigned_long;

   pragma Convention (StdCall, af_IConnectionPointContainer_AddRef);

   type af_IConnectionPointContainer_Release is access
     function (This : access IConnectionPointContainer)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IConnectionPointContainer_Release);

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

   type af_IEnumConnectionPoints_AddRef is access
     function (This : access IEnumConnectionPoints)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumConnectionPoints_AddRef);

   type af_IEnumConnectionPoints_Release is access
     function (This : access IEnumConnectionPoints)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Release);

   type af_IEnumConnectionPoints_Next is access
     function (This         : access IEnumConnectionPoints;
               cConnections :        Interfaces.C.unsigned_long;
               rgpcn        :        Pointer_To_Pointer_To_IConnectionPoint;
               lpcFetched   :        Pointer_To_Void)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Next);

   type af_IEnumConnectionPoints_Skip is access
     function (This         : access IEnumConnectionPoints;
               cConnections :        Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Skip);

   type af_IEnumConnectionPoints_Reset is access
     function (This : access IEnumConnectionPoints) return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Reset);

   type af_IEnumConnectionPoints_Clone is access
     function (This   : access IEnumConnectionPoints;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnectionPoints)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnectionPoints_Clone);

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

   type af_IConnectionPoint_AddRef is access
     function (This : access IConnectionPoint)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IConnectionPoint_AddRef);

   type af_IConnectionPoint_Release is access
     function (This : access IConnectionPoint)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IConnectionPoint_Release);

   type af_IConnectionPoint_GetConnectionInterface is access
     function (This : access IConnectionPoint;
               piid :        Pointer_To_GUID)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_GetConnectionInterface);

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

   type af_IConnectionPoint_Unadvise is access
     function (This     : access IConnectionPoint;
               dwCookie : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_Unadvise);

   type af_IConnectionPoint_EnumConnections is access
     function (This   : access IConnectionPoint;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnections)
     return HRESULT;
   pragma Convention (StdCall, af_IConnectionPoint_EnumConnections);

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
         dwCookie : Interfaces.C.unsigned_long;
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

   type af_IEnumConnections_AddRef is access
     function (This : access IEnumConnections)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumConnections_AddRef);

   type af_IEnumConnections_Release is access
     function (This : access IEnumConnections)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumConnections_Release);

   type af_IEnumConnections_Next is access
     function (This         : access IEnumConnections;
               cConnections :        Interfaces.C.unsigned_long;
               rgcd         :        Pointer_To_CONNECTDATA;
               LpcFetched   :        Pointer_To_Void)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Next);

   type af_IEnumConnections_Skip is access
     function (This         : access IEnumConnections;
               cConnections :        Interfaces.C.unsigned_long)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Skip);

   type af_IEnumConnections_Reset is access
     function (This : access IEnumConnections)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Reset);

   type af_IEnumConnections_Clone is access
     function (This   : access IEnumConnections;
               ppEnum :        Pointer_To_Pointer_To_IEnumConnections)
   return HRESULT;
   pragma Convention (StdCall, af_IEnumConnections_Clone);

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

   type af_IEnumVARIANT_AddRef is access
     function (This : access IEnumVARIANT)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumVARIANT_AddRef);

   type af_IEnumVARIANT_Release is access
     function (This : access IEnumVARIANT)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IEnumVARIANT_Release);

   type af_IEnumVARIANT_Next is access
     function (This         : access IEnumVARIANT;
               celt         : Interfaces.C.long;
               rgvar        : access VARIANT;
               pceltFetched : access Interfaces.C.long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Next);

   type af_IEnumVARIANT_Skip is access
     function (This : access IEnumVARIANT;
               celt : Interfaces.C.long)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Skip);

   type af_IEnumVARIANT_Reset is access
     function (This : access IEnumVARIANT)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Reset);

   type af_IEnumVARIANT_Clone is access
     function (This   : access IEnumVARIANT;
               ppenum : access Pointer_To_IEnumVARIANT)
     return HRESULT;
   pragma Convention (StdCall, af_IEnumVARIANT_Clone);

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

   type af_IGlobalInterfaceTable_AddRef is access
     function (This : access IGlobalInterfaceTable)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IGlobalInterfaceTable_AddRef);

   type af_IGlobalInterfaceTable_Release is access
     function (This : access IGlobalInterfaceTable)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IGlobalInterfaceTable_Release);

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
               dwCookie : Interfaces.C.unsigned_long)
     return HRESULT;
   pragma Convention (StdCall,
                        af_IGlobalInterfaceTable_RevokeInterfaceFromGlobal);

   type af_IGlobalInterfaceTable_GetInterfaceFromGlobal is access
     function (This     : access IGlobalInterfaceTable;
               dwCookie : Interfaces.C.unsigned_long;
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

end GNATCOM.Types;
