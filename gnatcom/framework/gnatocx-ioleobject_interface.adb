with GNATCOM.Errors;

package body GNATOCX.IOleObject_Interface is

   procedure Initialize (This : in out IOleObject_Type) is
   begin
      Set_IID (This, IID_IOleObject);
   end Initialize;

   function Pointer (This : IOleObject_Type)
     return Pointer_To_IOleObject
   is
   begin
      return To_Pointer_To_IOleObject (Address (This));
   end Pointer;

   procedure Attach (This    : in out IOleObject_Type;
                     Pointer : in     Pointer_To_IOleObject)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure SetClientSite
     (This        : IOleObject_Type;
      pClientSite : Pointer_To_IOleClientSite)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetClientSite
         (Pointer (This),
          pClientSite));

   end SetClientSite;

   procedure GetClientSite
     (This         : IOleObject_Type;
      ppClientSite : Pointer_To_Pointer_To_IOleClientSite)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetClientSite
         (Pointer (This),
          ppClientSite));

   end GetClientSite;

   procedure SetHostNames
     (This           : IOleObject_Type;
      szContainerApp : GNATCOM.Types.BSTR;
      szContainerObj : GNATCOM.Types.BSTR;
      Free           : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetHostNames
         (Pointer (This),
          szContainerApp,
          szContainerObj));

      if Free then
         GNATCOM.Iinterface.Free (szContainerApp);
         GNATCOM.Iinterface.Free (szContainerObj);
      end if;
   end SetHostNames;

   procedure Close
     (This         : IOleObject_Type;
      dwSaveOption : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Close
         (Pointer (This),
          dwSaveOption));

   end Close;

   procedure SetMoniker
     (This           : IOleObject_Type;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      pmk            : Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetMoniker
         (Pointer (This),
          dwWhichMoniker,
          pmk));

   end SetMoniker;

   procedure GetMoniker
     (This           : IOleObject_Type;
      dwAssign       : Interfaces.C.unsigned_long;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      ppmk           : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetMoniker
         (Pointer (This),
          dwAssign,
          dwWhichMoniker,
          ppmk));

   end GetMoniker;

   procedure InitFromData
     (This        : IOleObject_Type;
      pDataObject : Pointer_To_IDataObject;
      fCreation   : Interfaces.C.long;
      dwReserved  : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InitFromData
         (Pointer (This),
          pDataObject,
          fCreation,
          dwReserved));

   end InitFromData;

   procedure GetClipboardData
     (This         : IOleObject_Type;
      dwReserved   : Interfaces.C.unsigned_long;
      ppDataObject : Pointer_To_Pointer_To_IDataObject)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetClipboardData
         (Pointer (This),
          dwReserved,
          ppDataObject));

   end GetClipboardData;

   procedure DoVerb
     (This        : IOleObject_Type;
      iVerb       : Interfaces.C.long;
      lpmsg       : Pointer_To_MSG;
      pActiveSite : Pointer_To_IOleClientSite;
      lindex      : Interfaces.C.long;
      hwndParent  : wireHWND;
      lprcPosRect : Pointer_To_RECT)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DoVerb
         (Pointer (This),
          iVerb,
          lpmsg,
          pActiveSite,
          lindex,
          hwndParent,
          lprcPosRect));

   end DoVerb;

   procedure EnumVerbs
     (This          : IOleObject_Type;
      ppEnumOleVerb : Pointer_To_Pointer_To_IEnumOLEVERB)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumVerbs
         (Pointer (This),
          ppEnumOleVerb));

   end EnumVerbs;

   procedure Update
     (This : IOleObject_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Update
         (Pointer (This)));

   end Update;

   procedure IsUpToDate
     (This : IOleObject_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsUpToDate
         (Pointer (This)));

   end IsUpToDate;

   procedure GetUserClassID
     (This   : IOleObject_Type;
      pClsid : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetUserClassID
         (Pointer (This),
          pClsid));

   end GetUserClassID;

   procedure GetUserType
     (This         : IOleObject_Type;
      dwFormOfType : Interfaces.C.unsigned_long;
      pszUserType  : GNATCOM.Types.Pointer_To_LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetUserType
         (Pointer (This),
          dwFormOfType,
          pszUserType));

   end GetUserType;

   procedure SetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Interfaces.C.unsigned_long;
      psizel       : Pointer_To_SIZEL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetExtent
         (Pointer (This),
          dwDrawAspect,
          psizel));

   end SetExtent;

   procedure GetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Interfaces.C.unsigned_long;
      psizel       : Pointer_To_SIZEL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetExtent
         (Pointer (This),
          dwDrawAspect,
          psizel));

   end GetExtent;

   procedure Advise
     (This          : IOleObject_Type;
      pAdvSink      : Pointer_To_IAdviseSink;
      pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Advise
         (Pointer (This),
          pAdvSink,
          pdwConnection));

   end Advise;

   procedure Unadvise
     (This         : IOleObject_Type;
      dwConnection : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Unadvise
         (Pointer (This),
          dwConnection));

   end Unadvise;

   procedure EnumAdvise
     (This         : IOleObject_Type;
      ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumAdvise
         (Pointer (This),
          ppenumAdvise));

   end EnumAdvise;

   procedure GetMiscStatus
     (This      : IOleObject_Type;
      dwAspect  : Interfaces.C.unsigned_long;
      pdwStatus : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetMiscStatus
         (Pointer (This),
          dwAspect,
          pdwStatus));

   end GetMiscStatus;

   procedure SetColorScheme
     (This    : IOleObject_Type;
      pLogpal : Pointer_To_LOGPALETTE)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetColorScheme
         (Pointer (This),
          pLogpal));

   end SetColorScheme;

end GNATOCX.IOleObject_Interface;
