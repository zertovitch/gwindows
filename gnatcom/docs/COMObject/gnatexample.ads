with Interfaces.C;
with System;

with Win32.Objbase;
with Win32.OleAuto;
with Win32.Winnt;

package GNATExample is

   package C renames Interfaces.C;

   --  Global reference counters for COM to determine when to unload

   Component_Count   : aliased Interfaces.C.long := 0;
   Server_Lock_Count : aliased Interfaces.C.long := 0;

   --  This is set to false by the local server (exe) version of the
   --  COM object.  During release of COM objects, if this variable is
   --  set to false, and every COM object created by the server has
   --  been released, then the server will shut itself down. In the
   --  case of InProc Servers, the operating system queries the DLL
   --  periodicaly by calling DllCanUnloadNow which returns true when
   --  all COM objects have been released. On a true return, the
   --  operating system closes down the server.

   InProcServer : Boolean := True;

   --  Can_Close is called when any object is released in order to allow
   --  the local server version of the COM object to post a WM_QUIT message
   --  to the server when needed.

   procedure Can_Close;


   --  LIBID of library GNATCOMLibrary
   --  {45F9F480-787C-11D3-821C-52544C1913DE}

   LIBID_GNATCOMLibrary : aliased Win32.Objbase.IID :=
     (16#45F9F480#, 16#787C#, 16#11D3#,
       (C.char'Val (16#82#), C.char'Val (16#1C#),
        C.char'Val (16#52#), C.char'Val (16#54#),
        C.char'Val (16#4C#), C.char'Val (16#19#),
        C.char'Val (16#13#), C.char'Val (16#DE#)));

   --  CLSID of coclass GNATCOMClass
   --  {45F9F481-787C-11D3-821C-52544C1913DE}

   CLSID_GNATCOMClass : aliased Win32.Objbase.CLSID :=
     (16#45F9F481#, 16#787C#, 16#11D3#,
       (C.char'Val (16#82#), C.char'Val (16#1C#),
        C.char'Val (16#52#), C.char'Val (16#54#),
        C.char'Val (16#4C#), C.char'Val (16#19#),
        C.char'Val (16#13#), C.char'Val (16#DE#)));

   --  IID of interface IGNATMessage
   --  {45F9F482-787C-11D3-821C-52544C1913DE}
   IID_IGNATMessage : aliased Win32.Objbase.IID :=
     (16#45F9F482#, 16#787C#, 16#11D3#,
       (C.char'Val (16#82#), C.char'Val (16#1C#),
        C.char'Val (16#52#), C.char'Val (16#54#),
        C.char'Val (16#4C#), C.char'Val (16#19#),
        C.char'Val (16#13#), C.char'Val (16#DE#)));

   --  IID of interface IGNATStat
   --  {45F9F483-787C-11D3-821C-52544C1913DE}
   IID_IGNATStat : aliased Win32.Objbase.IID :=
     (16#45F9F483#, 16#787C#, 16#11D3#,
       (C.char'Val (16#82#), C.char'Val (16#1C#),
        C.char'Val (16#52#), C.char'Val (16#54#),
        C.char'Val (16#4C#), C.char'Val (16#19#),
        C.char'Val (16#13#), C.char'Val (16#DE#)));

   --  IID for IUnkown
   IID_IUnknown : aliased Win32.Objbase.IID :=
     (16#00000000#, 16#0000#, 16#0000#,
      (C.char'Val (16#C0#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#46#)));

   --  IID for IUnkown
   IID_IDispatch  : aliased Win32.Objbase.IID :=
     (16#00020400#, 16#0000#, 16#0000#,
      (C.char'Val (16#C0#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#46#)));

   --  IID for IClassFactory
   IID_IClassFactory : aliased Win32.Objbase.IID :=
     (16#00000001#, 16#0000#, 16#0000#,
      (C.char'Val (16#C0#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#00#),
       C.char'Val (16#00#), C.char'Val (16#46#)));

   --  Since COM constants are defined as Unsigned_Longs and the
   --  HRESULT type is in fact a Long, it is redefined here for
   --  simplicity.
   subtype HRESULT is Interfaces.C.Unsigned_Long;

   --  Forward reference for a generic interface type and pointer that
   --  will handle the "this" pointer passed to COM interfaces.
   type Interface;

   --  Access function types for IUnknown

   type Af_IUnknown_QueryInterface is access
     function (This        : access Interface;
               riid        : in     Win32.Objbase.REFIID;
               ppvObject : access Win32.PVOID)
     return HRESULT;
   pragma Convention (Stdcall, Af_IUnknown_QueryInterface);

   type Af_IUnknown_AddRef is access
     function (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, Af_IUnknown_AddRef);

   type Af_IUnknown_Release is access
     function (This : access Interface)
     return Win32.ULONG;
   pragma Convention (Stdcall, Af_IUnknown_Release);

   --  Access function types for IDispatch

   type Af_IDispatch_GetTypeInfoCount is access
     function (This    : access Interface;
               pctinfo : access Win32.UINT)
     return HRESULT;
   pragma Convention (Stdcall, Af_IDispatch_GetTypeInfoCount);

   type Af_IDispatch_GetTypeInfo is access
     function (This    : access Interface;
               itinfo  : in     Win32.UINT;
               lcid    : in     Win32.Winnt.LCID;
               pptinfo : access Win32.OleAuto.LPTYPEINFO)
     return HRESULT;
   pragma Convention (Stdcall, Af_IDispatch_GetTypeInfo);

   type Af_IDispatch_GetIDsOfNames is access
     function (This      : access Interface;
               riid      : access Win32.Objbase.IID;
               rgszNames : access Win32.Objbase.LPOLESTR;
               cNames    : in     Win32.UINT;
               lcid      : in     Win32.Winnt.LCID;
               rgdispid  : access Win32.OleAuto.DISPID)
     return HRESULT;
   pragma Convention (Stdcall, Af_IDispatch_GetIDsOfNames);

   type Af_IDispatch_Invoke is access
     function (This         : access Interface;
               dispidMember : in     Win32.OleAuto.DISPID;
               riid         : access Win32.Objbase.IID;
               lcid         : in     Win32.Winnt.LCID;
               wFlags       : in     Win32.WORD;
               pdispparams  : access Win32.OleAuto.DISPPARAMS;
               pvarResult   : access Win32.OleAuto.VARIANT;
               pexcepinfo   : access Win32.OleAuto.EXCEPINFO;
               puArgErr     : access Win32.UINT)
     return HRESULT;
   pragma Convention (Stdcall, Af_IDispatch_Invoke);

   --  Access function types for IGNATMessage

   type Af_IGNATMessage_Beep is access
     function (This : access Interface)
     return HRESULT;
   pragma Convention (StdCall, Af_IGNATMessage_Beep);


   type Af_IGNATMessage_MessageBox is access
     function (This    : access Interface;
               Message : in     Win32.OleAuto.BSTR)
     return HRESULT;
   pragma Convention (StdCall, Af_IGNATMessage_MessageBox);

   --  Access function types for IGNATStat

   type Af_IGNATStat_Calls is access
     function (This          : access Interface;
               NumberOfTimes : access Interfaces.C.Int)
     return HRESULT;
   pragma Convention (StdCall, Af_IGNATStat_Calls);

   --  The Interface type is contstructed so that the first element in
   --  the record is a pointer to the table of functions and additional
   --  members are private data elements of the interface unavailable
   --  to clients of the COM object. The CoClass element holds the
   --  address of the object instance data. The Ref_Count is set to one
   --  so that upon creating it already has one reference. When the
   --  reference count reaches zero the IUnknown Release method will
   --  free the interface object from memory along with the object
   --  pointed to by the CoClass address.

   type Interface is
      record
         Vtbl      : System.Address;
         Ref_Count : aliased Win32.LONG := 1;
         CoClass   : System.Address;
      end record;
   pragma Convention (C, Interface);
   type Interface_Pointer is access all Interface;

end GNATExample;
