with Ada.Finalization;

package GNATExample.GNATCOMClass is

   --  Functions for IUnknown

   function IUnknown_QueryInterface
     (This      : access Interface;
      riid      : in     Win32.Objbase.REFIID;
      ppvObject : access Win32.PVOID)
     return HRESULT;
   pragma Convention (Stdcall, IUnknown_QueryInterface);

   function IUnknown_AddRef  (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, IUnknown_AddRef);

   function IUnknown_Release  (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, IUnknown_Release);

   --  Functions for IDispatch

   function IDispatch_GetTypeInfoCount
     (This    : access Interface;
      pctinfo : access Win32.UINT)
     return HRESULT;
   pragma Convention (Stdcall, IDispatch_GetTypeInfoCount);

   function IDispatch_GetTypeInfo
     (This    : access Interface;
      itinfo  : in     Win32.UINT;
      lcid    : in     Win32.Winnt.LCID;
      pptinfo : access Win32.OleAuto.LPTYPEINFO)
     return HRESULT;
   pragma Convention (Stdcall, IDispatch_GetTypeInfo);

   function IDispatch_GetIDsOfNames
     (This      : access Interface;
      riid      : access Win32.Objbase.IID;
      rgszNames : access Win32.Objbase.LPOLESTR;
      cNames    : in     Win32.UINT;
      lcid      : in     Win32.Winnt.LCID;
      rgdispid  : access Win32.OleAuto.DISPID)
     return HRESULT;
   pragma Convention (Stdcall, IDispatch_GetIDsOfNames);

   function IDispatch_Invoke
     (This         : access Interface;
      dispidMember : in     Win32.OleAuto.DISPID;
      riid         : access Win32.Objbase.IID;
      lcid         : in     Win32.Winnt.LCID;
      wFlags       : in     Win32.WORD;
      pdispparams  : access Win32.OleAuto.DISPPARAMS;
      pvarResult   : access Win32.OleAuto.VARIANT;
      pexcepinfo   : access Win32.OleAuto.EXCEPINFO;
      puArgErr     : access Win32.UINT)
     return HRESULT;
   pragma Convention (Stdcall, IDispatch_Invoke);

   --  Functions for IGNATMessage

   function IGNATMessage_Beep (This : access Interface)
     return HRESULT;
   pragma Convention (StdCall, IGNATMessage_Beep);

   function IGNATMessage_MessageBox
     (This    : access Interface;
      Message : in     Win32.OleAuto.BSTR)
     return HRESULT;
   pragma Convention (StdCall, IGNATMessage_MessageBox);

   --  Function types for IGNATStat

   function IGNATStat_Calls
     (This          : access Interface;
      NumberOfTimes : access Interfaces.C.Int)
     return HRESULT;
   pragma Convention (StdCall, IGNATStat_Calls);

   --  Interface Definitions

   type IGNATMessage_Vtbl_Record is
      record
         --  IUnknown
         QueryInterface   : Af_IUnknown_QueryInterface
           := IUnknown_QueryInterface'Access;
         AddRef           : Af_IUnknown_AddRef
           := IUnknown_AddRef'Access;
         Release          : Af_IUnknown_Release
           := IUnknown_Release'Access;
         --  IDispatch
         GetTypeInfoCount : Af_IDispatch_GetTypeInfoCount
           := IDispatch_GetTypeInfoCount'Access;
         GetTypeInfo      : Af_IDispatch_GetTypeInfo
           := IDispatch_GetTypeInfo'Access;
         GetIDsOfNames    : Af_IDispatch_GetIDsOfNames
           := IDispatch_GetIDsOfNames'Access;
         Invoke           : Af_IDispatch_Invoke
           := IDispatch_Invoke'Access;
         --  IGNATMessage
         Beep             : Af_IGNATMessage_Beep
           := IGNATMessage_Beep'Access;
         MessageBox       : AF_IGNATMessage_MessageBox
           := IGNATMessage_MessageBox'Access;
      end record;
   pragma Convention (C, IGNATMessage_Vtbl_Record);
   type IGNATMessage_Vtbl_Pointer is access all IGNATMessage_Vtbl_Record;

   type IGNATStat_Vtbl_Record is
      record
         --  IUnknown
         QueryInterface   : Af_IUnknown_QueryInterface
           := IUnknown_QueryInterface'Access;
         AddRef           : Af_IUnknown_AddRef
           := IUnknown_AddRef'Access;
         Release          : Af_IUnknown_Release
           := IUnknown_Release'Access;
         --  IGNATMessage
         Calls             : Af_IGNATStat_Calls
           := IGNATStat_Calls'Access;
      end record;
   pragma Convention (C, IGNATStat_Vtbl_Record);
   type IGNATStat_Vtbl_Pointer is access all IGNATMessage_Vtbl_Record;

   --  Create Vtbls in memory

   IGNATMessage_Vtbl : aliased IGNATMessage_Vtbl_Record;
   IGNATStat_Vtbl    : aliased IGNATStat_Vtbl_Record;

   --  Controlled type for handling information that should be
   --  processed when the object is created or destroyed.

   type Controlled_Information is
     new Ada.Finalization.Controlled with
      record
         Type_Information : aliased Win32.OleAuto.LPTYPEINFO := null;
      end record;

   procedure Initialize (This : in out Controlled_Information);
   procedure Finalize (This : in out Controlled_Information);
   procedure Adjust (This : in out Controlled_Information);

   --  Internal data for for the COM object. Notice that there is
   --  no Convention pragma on this type. This type is an Ada type.

   type GNATCOMClass_Object is
      record
         Ref_Count : aliased Win32.LONG := 1;
         Data      : Controlled_Information;
         Count     : Integer := 0;
      end record;
   type GNATCOMClass_Pointer is access all GNATCOMClass_Object;

   --  Creates a new COM object to be returned by Class Factory

   function New_Object return Interface_Pointer;

end GNATExample.GNATCOMClass;

