with GNATCOM.Iinterface;

package GNATOCX.IOleObject_Interface is

   type IOleObject_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IOleObject_Type);

   function Pointer (This : IOleObject_Type)
     return Pointer_To_IOleObject;

   procedure Attach (This    : in out IOleObject_Type;
                     Pointer : in     Pointer_To_IOleObject);

   procedure SetClientSite
     (This        : IOleObject_Type;
      pClientSite : Pointer_To_IOleClientSite);

   procedure GetClientSite
     (This         : IOleObject_Type;
      ppClientSite : Pointer_To_Pointer_To_IOleClientSite);

   procedure SetHostNames
     (This           : IOleObject_Type;
      szContainerApp : GNATCOM.Types.BSTR;
      szContainerObj : GNATCOM.Types.BSTR;
      Free           : Boolean := True);

   procedure Close
     (This         : IOleObject_Type;
      dwSaveOption : Interfaces.C.unsigned_long);

   procedure SetMoniker
     (This           : IOleObject_Type;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      pmk            : Pointer_To_IMoniker);

   procedure GetMoniker
     (This           : IOleObject_Type;
      dwAssign       : Interfaces.C.unsigned_long;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      ppmk           : Pointer_To_Pointer_To_IMoniker);

   procedure InitFromData
     (This        : IOleObject_Type;
      pDataObject : Pointer_To_IDataObject;
      fCreation   : Interfaces.C.long;
      dwReserved  : Interfaces.C.unsigned_long);

   procedure GetClipboardData
     (This         : IOleObject_Type;
      dwReserved   : Interfaces.C.unsigned_long;
      ppDataObject : Pointer_To_Pointer_To_IDataObject);

   procedure DoVerb
     (This        : IOleObject_Type;
      iVerb       : Interfaces.C.long;
      lpmsg       : Pointer_To_MSG;
      pActiveSite : Pointer_To_IOleClientSite;
      lindex      : Interfaces.C.long;
      hwndParent  : wireHWND;
      lprcPosRect : Pointer_To_RECT);

   procedure EnumVerbs
     (This          : IOleObject_Type;
      ppEnumOleVerb : Pointer_To_Pointer_To_IEnumOLEVERB);

   procedure Update
     (This : IOleObject_Type);

   procedure IsUpToDate
     (This : IOleObject_Type);

   procedure GetUserClassID
     (This   : IOleObject_Type;
      pClsid : GNATCOM.Types.Pointer_To_GUID);

   procedure GetUserType
     (This         : IOleObject_Type;
      dwFormOfType : Interfaces.C.unsigned_long;
      pszUserType  : GNATCOM.Types.Pointer_To_LPWSTR);

   procedure SetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Interfaces.C.unsigned_long;
      psizel       : Pointer_To_SIZEL);

   procedure GetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Interfaces.C.unsigned_long;
      psizel       : Pointer_To_SIZEL);

   procedure Advise
     (This          : IOleObject_Type;
      pAdvSink      : Pointer_To_IAdviseSink;
      pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Unadvise
     (This         : IOleObject_Type;
      dwConnection : Interfaces.C.unsigned_long);

   procedure EnumAdvise
     (This         : IOleObject_Type;
      ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA);

   procedure GetMiscStatus
     (This      : IOleObject_Type;
      dwAspect  : Interfaces.C.unsigned_long;
      pdwStatus : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure SetColorScheme
     (This    : IOleObject_Type;
      pLogpal : Pointer_To_LOGPALETTE);

end GNATOCX.IOleObject_Interface;
