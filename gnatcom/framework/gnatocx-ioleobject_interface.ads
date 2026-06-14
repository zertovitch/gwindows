with GNATCOM.Iinterface;
with Win32_Types;

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
      dwSaveOption : Win32_Types.Unsigned_Long);

   procedure SetMoniker
     (This           : IOleObject_Type;
      dwWhichMoniker : Win32_Types.Unsigned_Long;
      pmk            : Pointer_To_IMoniker);

   procedure GetMoniker
     (This           : IOleObject_Type;
      dwAssign       : Win32_Types.Unsigned_Long;
      dwWhichMoniker : Win32_Types.Unsigned_Long;
      ppmk           : Pointer_To_Pointer_To_IMoniker);

   procedure InitFromData
     (This        : IOleObject_Type;
      pDataObject : Pointer_To_IDataObject;
      fCreation   : Win32_Types.Long;
      dwReserved  : Win32_Types.Unsigned_Long);

   procedure GetClipboardData
     (This         : IOleObject_Type;
      dwReserved   : Win32_Types.Unsigned_Long;
      ppDataObject : Pointer_To_Pointer_To_IDataObject);

   procedure DoVerb
     (This        : IOleObject_Type;
      iVerb       : Win32_Types.Long;
      lpmsg       : Pointer_To_MSG;
      pActiveSite : Pointer_To_IOleClientSite;
      lindex      : Win32_Types.Long;
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
      dwFormOfType : Win32_Types.Unsigned_Long;
      pszUserType  : GNATCOM.Types.Pointer_To_LPWSTR);

   procedure SetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Win32_Types.Unsigned_Long;
      psizel       : Pointer_To_SIZEL);

   procedure GetExtent
     (This         : IOleObject_Type;
      dwDrawAspect : Win32_Types.Unsigned_Long;
      psizel       : Pointer_To_SIZEL);

   procedure Advise
     (This          : IOleObject_Type;
      pAdvSink      : Pointer_To_IAdviseSink;
      pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Unadvise
     (This         : IOleObject_Type;
      dwConnection : Win32_Types.Unsigned_Long);

   procedure EnumAdvise
     (This         : IOleObject_Type;
      ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA);

   procedure GetMiscStatus
     (This      : IOleObject_Type;
      dwAspect  : Win32_Types.Unsigned_Long;
      pdwStatus : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure SetColorScheme
     (This    : IOleObject_Type;
      pLogpal : Pointer_To_LOGPALETTE);

end GNATOCX.IOleObject_Interface;
