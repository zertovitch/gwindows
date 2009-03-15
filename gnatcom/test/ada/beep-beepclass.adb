with GNAT.IO; use GNAT.IO;

package body Beep.BeepClass is

   function IBeep_GetTypeInfoCount
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pctinfo : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT
   is
   begin
      return GNATCOM.Create.IDispatch.GetTypeInfoCount (pctinfo);
   end IBeep_GetTypeInfoCount;

   function IBeep_GetTypeInfo
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      itinfo  : Interfaces.C.unsigned;
      lcid    : Interfaces.C.unsigned_long;
      pptinfo : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_BeepClass_Type :=
        Pointer_To_BeepClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.GetTypeInfo (Object.Data'Access,
                                                   itinfo,
                                                   pptinfo);
   end IBeep_GetTypeInfo;

   function IBeep_GetIDsOfNames
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      lcid      : Interfaces.C.unsigned_long;
      rgdispid  : GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_BeepClass_Type :=
        Pointer_To_BeepClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.GetIDsOfNames (Object.Data'Access,
                                                     rgszNames,
                                                     cNames,
                                                     rgdispid);
   end IBeep_GetIDsOfNames;

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
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_BeepClass_Type :=
        Pointer_To_BeepClass_Type (This.CoClass);
   begin
      return GNATCOM.Create.IDispatch.Invoke (This,
                                              Object.Data'Access,
                                              dispidMember,
                                              wFlags,
                                              pdispparams,
                                              pvarResult,
                                              pexcepinfo,
                                              puArgErr);
   end IBeep_Invoke;

   function IBeep_Beep
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
   begin
      Put_Line ("++ Pass");

      return 123;
   end IBeep_Beep;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Create.COM_Interface.Create_Object
        (new BeepClass_Type);
   end Create;

end Beep.BeepClass;

