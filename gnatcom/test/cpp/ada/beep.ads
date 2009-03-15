--  Library Name             : BeepLibrary
--  Library Documentation    : Beep Type Library
--  Library Version          : 1.0
--  Library LIBID            : {0FE0EE20-8AA2-11D2-81AA-444553540001}
--  Elements in Type Library :  2

with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;

with GNATCOM.Types;
with GNATCOM.GUID;

package Beep is

   --  {0FE0EE20-8AA2-11D2-81AA-444553540001}
   LIBID_BeepLibrary : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0FE0EE20-8AA2-11D2-81AA-444553540001}");

   type IBeep;
   type IBeepVtbl;
   type Pointer_To_IBeep is access all IBeep;
   type Pointer_To_IBeepVtbl is access all IBeepVtbl;
   type Pointer_To_unsigned is access all Interfaces.C.unsigned;
   type Pointer_To_char is access all Interfaces.C.char;
   type Pointer_To_Pointer_To_char is access all Pointer_To_char;

   --  Element Name             : IBeep
   --  Element Documentation    : Beep Interface
   --  Element Kind             : Interface
   --  {0FE0EE22-8AA2-11D2-81AA-444553540001}
   IID_IBeep
    : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0FE0EE22-8AA2-11D2-81AA-444553540001}");

   type af_IBeep_QueryInterface is access
     function (This     : access IBeep;
               riid     : in     GNATCOM.Types.Pointer_To_GUID;
               ppvObj   : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBeep_QueryInterface);

   type af_IBeep_AddRef is access
     function (This : access IBeep) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IBeep_AddRef);

   type af_IBeep_Release is access
     function (This : access IBeep) return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IBeep_Release);

   type af_IBeep_GetTypeInfoCount is access
     function (This    : access IBeep;
               pctinfo : in     Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBeep_GetTypeInfoCount);

   type af_IBeep_GetTypeInfo is access
     function (This    : access IBeep;
               itinfo  : in     Interfaces.C.unsigned;
               lcid    : in     Interfaces.C.unsigned_long;
               pptinfo : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBeep_GetTypeInfo);

   type af_IBeep_GetIDsOfNames is access
     function (This      : access IBeep;
               riid      : in     GNATCOM.Types.Pointer_To_GUID;
               rgszNames : in     Pointer_To_Pointer_To_char;
               cNames    : in     Interfaces.C.unsigned;
               lcid      : in     Interfaces.C.unsigned_long;
               rgdispid  : in     GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBeep_GetIDsOfNames);

   type af_IBeep_Invoke is access
     function (This         : access IBeep;
               dispidMember : in     Interfaces.C.long;
               riid         : in     GNATCOM.Types.Pointer_To_GUID;
               lcid         : in     Interfaces.C.unsigned_long;
               wFlags       : in     Interfaces.C.unsigned_short;
               pdispparams  : in     GNATCOM.Types.Pointer_To_DISPPARAMS;
               pvarResult   : in     GNATCOM.Types.Pointer_To_VARIANT;
               pexcepinfo   : in     GNATCOM.Types.Pointer_To_EXCEPINFO;
               puArgErr     : in     Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBeep_Invoke);

   type af_IBeep_Beep is access
     function (This : access IBeep) return GNATCOM.Types.HRESULT;
   --  Make a beep
   pragma Convention (StdCall, af_IBeep_Beep);

   type af_IBeep_Display is access
     function (This : access IBeep;
               text : in     GNATCOM.Types.BSTR)
     return GNATCOM.Types.HRESULT;
   --  Display text
   pragma Convention (StdCall, af_IBeep_Display);

   type af_IBeep_GetText is access
     function (This : access IBeep;
               text : in      GNATCOM.Types.Pointer_To_BSTR)
     return GNATCOM.Types.HRESULT;
   --  Get text
   pragma Convention (StdCall, af_IBeep_GetText);

   type af_IBeep_SetGetText is access
     function (This    : access IBeep;
               inText  : in     GNATCOM.Types.BSTR;
               outText : in     GNATCOM.Types.Pointer_To_BSTR)
     return GNATCOM.Types.HRESULT;
   --  Set and Get Text
   pragma Convention (StdCall, af_IBeep_SetGetText);

   type af_IBeep_InDoubleOut is access
     function (This     : access IBeep;
               InText   : in     GNATCOM.Types.BSTR;
               out1Text : in     GNATCOM.Types.Pointer_To_BSTR;
               out2Text : in     GNATCOM.Types.Pointer_To_BSTR)
     return GNATCOM.Types.HRESULT;
   --  In and Double Out
   pragma Convention (StdCall, af_IBeep_InDoubleOut);

   type af_IBeep_InDoubleOutVar is access
     function (This    : access IBeep;
               inVar   : GNATCOM.Types.VARIANT;
               out1Var : GNATCOM.Types.Pointer_To_VARIANT;
               out2Var : GNATCOM.Types.Pointer_To_VARIANT)
     return GNATCOM.Types.HRESULT;
   --  In and Double Out VARIANT
   pragma Convention (StdCall, af_IBeep_InDoubleOutVar);

   type IBeep is
      record
         Vtbl : Pointer_To_IBeepVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IBeep);

   type IBeepVtbl is
      record
         QueryInterface   : af_IBeep_QueryInterface;
         AddRef           : af_IBeep_AddRef;
         Release          : af_IBeep_Release;
         GetTypeInfoCount : af_IBeep_GetTypeInfoCount;
         GetTypeInfo      : af_IBeep_GetTypeInfo;
         GetIDsOfNames    : af_IBeep_GetIDsOfNames;
         Invoke           : af_IBeep_Invoke;
         Beep             : af_IBeep_Beep;
         Display          : af_IBeep_Display;
         GetText          : af_IBeep_GetText;
         SetGetText       : af_IBeep_SetGetText;
         InDoubleOut      : af_IBeep_InDoubleOut;
         InDoubleOutVar   : af_IBeep_InDoubleOutVar;
      end record;
   pragma Convention (C_Pass_By_Copy, IBeepVtbl);

   function To_Pointer_To_IBeep is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IBeep);

   ID_IBeep_Beep           : constant := 1;
   ID_IBeep_Display        : constant := 2;
   ID_IBeep_GetText        : constant := 3;
   ID_IBeep_SetGetText     : constant := 4;
   ID_IBeep_InDoubleOut    : constant := 5;
   ID_IBeep_InDoubleOutVar : constant := 6;

   CLSID_BeepClass : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0FE0EE21-8AA2-11D2-81AA-444553540001}");

end Beep;



