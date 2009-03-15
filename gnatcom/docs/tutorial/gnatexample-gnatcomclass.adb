with Win32.WinUser;

with GNATCOM.Utility;
with GNATCOM.BSTR;

package body GNATExample.GNATCOMClass is

   function IGNATMessage_GetTypeInfoCount
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.GetTypeInfoCount (pctinfo);
   end IGNATMessage_GetTypeInfoCount;

   function IGNATMessage_GetTypeInfo
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.GetTypeInfo (Object.Data'Access,
                                                   itinfo,
                                                   pptinfo);
   end IGNATMessage_GetTypeInfo;

   function IGNATMessage_GetIDsOfNames
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.GetIDsOfNames (Object.Data'Access,
                                                     rgszNames,
                                                     cNames,
                                                     rgdispid);
   end IGNATMessage_GetIDsOfNames;

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
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.Invoke (This,
                                              Object.Data'Access,
                                              dispidMember,
                                              wFlags,
                                              pdispparams,
                                              pvarResult,
                                              pexcepinfo,
                                              puArgErr);
   end IGNATMessage_Invoke;

   function IGNATMessage_Beep
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      Object   : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
      RetValue : Win32.BOOL;
   begin
      RetValue := Win32.WinUser.MessageBeep (Win32.WinUser.MB_ICONEXCLAMATION);
      Object.Count := Object.Count + 1;
      return GNATCOM.S_OK;
   end IGNATMessage_Beep;

   function IGNATMessage_MessageBox
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Message : GNATCOM.Types.BSTR)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      GNATCOM.Utility.Message_Box ("GNATCOM",
                                   GNATCOM.BSTR.To_Ada (Message, False));
      Object.Count := Object.Count + 1;
      return GNATCOM.S_OK;
   exception
      when others =>
         return GNATCOM.E_FAIL;
   end IGNATMessage_MessageBox;

   function IGNATStat_Calls
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      NumberOfTimes : GNATCOM.Types.Pointer_To_int)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATCOMClass_Type :=
        Pointer_To_GNATCOMClass_Type (This.CoClass);
   begin
      NumberOfTimes.all := Interfaces.C.Int (Object.Count);
      return GNATCOM.S_OK;
   end IGNATStat_Calls;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Create.COM_Interface.Create_Object
        (new GNATCOMClass_Type);
   end Create;
end GNATExample.GNATCOMClass;

