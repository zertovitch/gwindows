with GNATCOM.Create.IDispatch;

package Beep.BeepClass is

   function IBeep_GetTypeInfoCount
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IBeep_GetTypeInfoCount);

   function IBeep_GetTypeInfo
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IBeep_GetTypeInfo);

   function IBeep_GetIDsOfNames
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IBeep_GetIDsOfNames);

   function IBeep_Invoke
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
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IBeep_Invoke);

   function IBeep_Beep
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IBeep_Beep);
   --  Make a beep

   type IBeep_Vtbl_Record is
      record
         IUnknown         : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetTypeInfoCount : af_IBeep_GetTypeInfoCount :=
           IBeep_GetTypeInfoCount'Access;
         GetTypeInfo      : af_IBeep_GetTypeInfo :=
           IBeep_GetTypeInfo'Access;
         GetIDsOfNames    : af_IBeep_GetIDsOfNames :=
           IBeep_GetIDsOfNames'Access;
         Invoke           : af_IBeep_Invoke :=
           IBeep_Invoke'Access;
         Beep             : af_IBeep_Beep :=
           IBeep_Beep'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IBeep_Vtbl_Record);

   type Pointer_To_IBeep_Vtbl_Record is access all IBeep_Vtbl_Record;

   IBeep_Vtbl : aliased IBeep_Vtbl_Record;

   GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
     (1 => (IID_IBeep, IBeep_Vtbl'Address),
      2 => (GNATCOM.Types.IID_IDispatch, IBeep_Vtbl'Address));

   type BeepClass_Type is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         Data : aliased GNATCOM.Create.IDispatch.IDispatch_Type
           (IID_IBeep'Access,
            LIBID_BeepLibrary'Access, 1, 0);
      end record;

   type Pointer_To_BeepClass_Type is
     access all BeepClass_Type;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

end Beep.BeepClass;

