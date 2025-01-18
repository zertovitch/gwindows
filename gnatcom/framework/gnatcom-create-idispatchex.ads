with GNATCOM.Create.COM_Interface;
with GNATCOM.Types;

package GNATCOM.Create.IDispatchEx is

   function IDispatchEx_GetTypeInfoCount
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetTypeInfo
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetIDsOfNames
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_Invoke
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dispidMember : Interfaces.C.long;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      lcid         : Interfaces.C.unsigned_long;
      wFlags       : Interfaces.C.unsigned_short;
      pdispparams  : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetDispID
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long;
      pid      : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_InvokeEx
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id          : Interfaces.C.long;
      lcid        : Interfaces.C.unsigned_long;
      wFlags      : Interfaces.C.unsigned_short;
      pdp         : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarRes     : GNATCOM.Types.Pointer_To_VARIANT;
      pei         : GNATCOM.Types.Pointer_To_EXCEPINFO;
      pspCaller   : GNATCOM.Types.Pointer_To_IServiceProvider)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_DeleteMemberByName
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_DeleteMemberByDispID
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id   : Interfaces.C.long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetMemberProperties
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id          : Interfaces.C.long;
      grfdexFetch : Interfaces.C.unsigned_long;
      pgrfdex     : GNATCOM.Types.Pointer_To_unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetMemberName
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id        : Interfaces.C.long;
      pbstrName : GNATCOM.Types.Pointer_To_BSTR)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetNextDispID
     (This   : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfdex : Interfaces.C.unsigned_long;
      id     : Interfaces.C.long;
      pid    : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IDispatchEx_GetNameSpaceParent
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppunk : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetTypeInfoCount is access function
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetTypeInfo is access function
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetIDsOfNames is access function
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_Invoke is access function
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dispidMember : Interfaces.C.long;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      lcid         : Interfaces.C.unsigned_long;
      wFlags       : Interfaces.C.unsigned_short;
      pdispparams  : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetDispID is access function
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long;
      pid      : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_InvokeEx is access function
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id          : Interfaces.C.long;
      lcid        : Interfaces.C.unsigned_long;
      wFlags      : Interfaces.C.unsigned_short;
      pdp         : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarRes     : GNATCOM.Types.Pointer_To_VARIANT;
      pei         : GNATCOM.Types.Pointer_To_EXCEPINFO;
      pspCaller   : GNATCOM.Types.Pointer_To_IServiceProvider)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_DeleteMemberByName is access function
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_DeleteMemberByDispID is access function
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id   : Interfaces.C.long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetMemberProperties is access function
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id          : Interfaces.C.long;
      grfdexFetch : Interfaces.C.unsigned_long;
      pgrfdex     : GNATCOM.Types.Pointer_To_unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetMemberName is access function
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id        : Interfaces.C.long;
      pbstrName : GNATCOM.Types.Pointer_To_BSTR)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetNextDispID is access function
     (This   : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfdex : Interfaces.C.unsigned_long;
      id     : Interfaces.C.long;
      pid    : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IDispatchEx_GetNameSpaceParent is access function
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppunk : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type IDispatchEx_Vtbl_Record is
      record
         IUnknown                 : COM_Interface.IUnknown_Vtbl_Record;
         GetTypeInfoCount         : af_IDispatchEx_GetTypeInfoCount :=
           IDispatchEx_GetTypeInfoCount'Access;
         GetTypeInfo              : af_IDispatchEx_GetTypeInfo :=
           IDispatchEx_GetTypeInfo'Access;
         GetIDsOfNames            : af_IDispatchEx_GetIDsOfNames :=
           IDispatchEx_GetIDsOfNames'Access;
         Invoke                   : af_IDispatchEx_Invoke :=
           IDispatchEx_Invoke'Access;
         GetDispID                : af_IDispatchEx_GetDispID :=
           IDispatchEx_GetDispID'Access;
         InvokeEx                 : af_IDispatchEx_InvokeEx :=
           IDispatchEx_InvokeEx'Access;
         DeleteMemberByName       : af_IDispatchEx_DeleteMemberByName :=
           IDispatchEx_DeleteMemberByName'Access;
         GetMemberProperties      : af_IDispatchEx_GetMemberProperties :=
           IDispatchEx_GetMemberProperties'Access;
         GetMemberName            : af_IDispatchEx_GetMemberName :=
           IDispatchEx_GetMemberName'Access;
         GetNextDispID            : af_IDispatchEx_GetNextDispID :=
           IDispatchEx_GetNextDispID'Access;
         GetNameSpaceParent       : af_IDispatchEx_GetNameSpaceParent :=
           IDispatchEx_GetNameSpaceParent'Access;
      end record
     with Convention => C_Pass_By_Copy;

   IDispatchEx_Vtbl : aliased IDispatchEx_Vtbl_Record;

end GNATCOM.Create.IDispatchEx;
