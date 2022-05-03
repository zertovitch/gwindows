with Interfaces.C;
with GNATCOM.Types;
with GNATCOM.Create.COM_Interface;

generic
   CLSID     : GNATCOM.Types.Pointer_To_GUID;
   LIB_IID   : GNATCOM.Types.Pointer_To_GUID;
   wVerMajor : Interfaces.C.unsigned_short;
   wVerMinor : Interfaces.C.unsigned_short;
   Event_IID : GNATCOM.Types.Pointer_To_GUID;
package GNATCOM.Create.IProvideClassInfos is

   type af_IProvideClassInfo_GetClassInfo is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppTI : GNATCOM.Types.Pointer_To_Pointer_To_Void)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   function IProvideClassInfo_GetClassInfo
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppTI : GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type IProvideClassInfo_Vtbl_Record is
      record
         IUnknown     : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetClassInfo : af_IProvideClassInfo_GetClassInfo :=
           IProvideClassInfo_GetClassInfo'Access;
      end record;

   IProvideClassInfo_Vtbl  : aliased IProvideClassInfo_Vtbl_Record;

   type af_IProvideClassInfo2_GetGUID is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dwGuidKind : GNATCOM.Types.DWORD;
               pGUID      : GNATCOM.Types.Pointer_To_GUID)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   function IProvideClassInfo2_GetGUID
     (This       : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dwGuidKind : GNATCOM.Types.DWORD;
      pGUID      : GNATCOM.Types.Pointer_To_GUID)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type IProvideClassInfo2_Vtbl_Record is
      record
         IUnknown : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetGUID  : af_IProvideClassInfo2_GetGUID :=
           IProvideClassInfo2_GetGUID'Access;
      end record;

   IProvideClassInfo2_Vtbl : aliased IProvideClassInfo2_Vtbl_Record;

end GNATCOM.Create.IProvideClassInfos;
