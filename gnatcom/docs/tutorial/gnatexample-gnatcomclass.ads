with GNATCOM.Create.IDispatch;

package GNATExample.GNATCOMClass is

   function IGNATMessage_GetTypeInfoCount
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATMessage_GetTypeInfoCount);

   function IGNATMessage_GetTypeInfo
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATMessage_GetTypeInfo);

   function IGNATMessage_GetIDsOfNames
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATMessage_GetIDsOfNames);

   function IGNATMessage_Invoke
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
   pragma Convention (StdCall, IGNATMessage_Invoke);

   function IGNATMessage_Beep
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATMessage_Beep);
   --  Audio Alert

   function IGNATMessage_MessageBox
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Message : GNATCOM.Types.BSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATMessage_MessageBox);
   --  Display Message Box

   type IGNATMessage_Vtbl_Record is
      record
         IUnknown         : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetTypeInfoCount : af_IGNATMessage_GetTypeInfoCount :=
           IGNATMessage_GetTypeInfoCount'Access;
         GetTypeInfo      : af_IGNATMessage_GetTypeInfo :=
           IGNATMessage_GetTypeInfo'Access;
         GetIDsOfNames    : af_IGNATMessage_GetIDsOfNames :=
           IGNATMessage_GetIDsOfNames'Access;
         Invoke           : af_IGNATMessage_Invoke :=
           IGNATMessage_Invoke'Access;
         Beep             : af_IGNATMessage_Beep :=
           IGNATMessage_Beep'Access;
         MessageBox       : af_IGNATMessage_MessageBox :=
           IGNATMessage_MessageBox'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATMessage_Vtbl_Record);

   type Pointer_To_IGNATMessage_Vtbl_Record is
     access all IGNATMessage_Vtbl_Record;

   IGNATMessage_Vtbl : aliased IGNATMessage_Vtbl_Record;

   function IGNATStat_Calls
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      NumberOfTimes : GNATCOM.Types.Pointer_To_int)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IGNATStat_Calls);
   --  Return number of times methods of the IGNATMessage inertface were called

   type IGNATStat_Vtbl_Record is
      record
         IUnknown : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         Calls    : af_IGNATStat_Calls :=
           IGNATStat_Calls'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATStat_Vtbl_Record);

   type Pointer_To_IGNATStat_Vtbl_Record is
     access all IGNATStat_Vtbl_Record;

   IGNATStat_Vtbl : aliased IGNATStat_Vtbl_Record;

      GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
        (1 => (IID_IGNATMessage, IGNATMessage_Vtbl'Address),
         2 => (GNATCOM.Types.IID_IDispatch, IGNATMessage_Vtbl'Address),
         3 => (IID_IGNATStat, IGNATStat_Vtbl'Address));


   type GNATCOMClass_Type is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         Data  : aliased GNATCOM.Create.IDispatch.IDispatch_Type
           (IID_IGNATMessage'Access,
            LIBID_GNATCOMLibrary'Access, 1, 0);
         Count : Integer := 0;
      end record;

   type Pointer_To_GNATCOMClass_Type is
     access all GNATCOMClass_Type;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

end GNATExample.GNATCOMClass;

