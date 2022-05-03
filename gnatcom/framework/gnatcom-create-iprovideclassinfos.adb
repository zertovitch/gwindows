with GNATCOM.Create.IDispatch;

package body GNATCOM.Create.IProvideClassInfos is

   IDispatch : GNATCOM.Create.IDispatch.Pointer_To_IDispatch_Type;

   function IProvideClassInfo_GetClassInfo
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppTI : GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
      use type GNATCOM.Create.IDispatch.Pointer_To_IDispatch_Type;
      use type GNATCOM.Types.Pointer_To_Pointer_To_Void;
      LANG_NEUTRAL : constant := 0;
   begin
      if ppTI /= null then
         if IDispatch = null then
            IDispatch := new GNATCOM.Create.IDispatch.IDispatch_Type
              (CLSID, LIB_IID, wVerMajor, wVerMinor);
         end if;
         return GNATCOM.Create.IDispatch.GetTypeInfo
           (IDispatch, LANG_NEUTRAL, ppTI);
      else
         return GNATCOM.E_POINTER;
      end if;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IProvideClassInfo_GetClassInfo;

   function IProvideClassInfo2_GetGUID
     (This       : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dwGuidKind : GNATCOM.Types.DWORD;
      pGUID      : GNATCOM.Types.Pointer_To_GUID)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
      use type GNATCOM.Types.DWORD;
      use type GNATCOM.Types.Pointer_To_GUID;
      GUIDKIND_DEFAULT_SOURCE_DISP_IID : constant := 0;
   begin
      if dwGuidKind /= GUIDKIND_DEFAULT_SOURCE_DISP_IID then
         return GNATCOM.E_INVALIDARG;
      elsif pGUID = null then
         return GNATCOM.E_POINTER;
      else
         pGUID.all := Event_IID.all;
         return GNATCOM.S_OK;
      end if;
   end IProvideClassInfo2_GetGUID;

end GNATCOM.Create.IProvideClassInfos;
