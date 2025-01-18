with Ada.Unchecked_Conversion;
with Interfaces.C;
with GNATCOM.Errors;

package body GNATCOM.Create.IDispatchEx is

   function IDispatchEx_GetTypeInfoCount
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pctinfo);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_GetTypeInfoCount;

   function IDispatchEx_GetTypeInfo
     (This    : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, itinfo, lcid, pptinfo);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_GetTypeInfo;

   function IDispatchEx_GetIDsOfNames
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, riid, rgszNames, cNames, lcid, rgdispid);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_GetIDsOfNames;

   function IDispatchEx_Invoke
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dispidMember : Interfaces.C.long;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      lcid         : Interfaces.C.unsigned_long;
      wFlags       : Interfaces.C.unsigned_short;
      pdispparams  : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, dispidMember, riid, lcid, wFlags,
                           pdispparams, pvarResult,
                           pexcepinfo, puArgErr);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_Invoke;

   function IDispatchEx_GetDispID
     (This     : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long;
      pid      : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, bstrName, grfdex, pid);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_GetDispID;

   function IDispatchEx_InvokeEx
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id        : Interfaces.C.long;
      lcid      : Interfaces.C.unsigned_long;
      wFlags    : Interfaces.C.unsigned_short;
      pdp       : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarRes   : GNATCOM.Types.Pointer_To_VARIANT;
      pei       : GNATCOM.Types.Pointer_To_EXCEPINFO;
      pspCaller : GNATCOM.Types.Pointer_To_IServiceProvider)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (pspCaller);
   begin
      return
        IDispatchEx_Invoke
          (This, id, GNATCOM.Types.GUID_NULL'Access, lcid, wFlags, pdp, pvarRes, pei, null);
   end IDispatchEx_InvokeEx;

   function IDispatchEx_DeleteMemberByName
     (This     : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      bstrName : GNATCOM.Types.BSTR;
      grfdex   : Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, bstrName, grfdex);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_DeleteMemberByName;

   function IDispatchEx_DeleteMemberByDispID
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id   : Interfaces.C.long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, id);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_DeleteMemberByDispID;

   function IDispatchEx_GetMemberProperties
     (This        : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id          : Interfaces.C.long;
      grfdexFetch : Interfaces.C.unsigned_long;
      pgrfdex     : GNATCOM.Types.Pointer_To_unsigned_long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, id, grfdexFetch, pgrfdex);
   begin
      return GNATCOM.E_NOTIMPL;
   end IDispatchEx_GetMemberProperties;

   function IDispatchEx_GetMemberName
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      id        : Interfaces.C.long;
      pbstrName : GNATCOM.Types.Pointer_To_BSTR)
      return GNATCOM.Types.HRESULT
   is
      use type Interfaces.C.unsigned;
      use type GNATCOM.Types.Pointer_To_ITypeInfo;
      use type GNATCOM.Types.Pointer_To_BSTR;

      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_ITypeInfo, GNATCOM.Types.Pointer_To_Pointer_To_Void);

      Count     : aliased Interfaces.C.unsigned := 0;
      Type_Info : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
      Names     : aliased GNATCOM.Types.BSTR_PARAM_ARRAY;
      Result    : GNATCOM.Types.HRESULT         :=
        (if pbstrName = null then GNATCOM.S_OK else GNATCOM.E_POINTER);
   begin
      if GNATCOM.Errors.SUCCEEDED (Result) then
         Result := IDispatchEx_GetTypeInfoCount (This, Count'Unchecked_Access);
      end if;

      if GNATCOM.Errors.SUCCEEDED (Result) then
         if Count /= 0 then
            Result :=
              IDispatchEx_GetTypeInfo
                (This, 0, 0, To_Pointer_To_Pointer_To_Void (Type_Info'Unchecked_Access));
         else
            Result := GNATCOM.DISP_E_UNKNOWNNAME;
         end if;
      end if;

      if GNATCOM.Errors.SUCCEEDED (Result) then
         Result        :=
           Type_Info.Vtbl.GetNames.all
             (Type_Info, id, Names'Unchecked_Access, Names'Length, Count'Unchecked_Access);
         pbstrName.all := Names (Names'First);
      end if;

      if GNATCOM.Errors.SUCCEEDED (Result) and then Count = 0 then
         Result := GNATCOM.DISP_E_UNKNOWNNAME;
      end if;

      if Type_Info /= null then
         declare
            Ref_Count : constant Interfaces.C.unsigned_long :=
              Type_Info.Vtbl.Release.all (Type_Info);
            pragma Unreferenced (Ref_Count);
         begin
            Type_Info := null;
         end;
      end if;

      return Result;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IDispatchEx_GetMemberName;

   function IDispatchEx_GetNextDispID
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfdex : Interfaces.C.unsigned_long;
      id     : Interfaces.C.long;
      pid    : GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, grfdex, id, pid);
   begin
      return E_NOTIMPL;
   end IDispatchEx_GetNextDispID;

   function IDispatchEx_GetNameSpaceParent
     (This  : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppunk : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, ppunk);
   begin
      return E_NOTIMPL;
   end IDispatchEx_GetNameSpaceParent;

end GNATCOM.Create.IDispatchEx;
