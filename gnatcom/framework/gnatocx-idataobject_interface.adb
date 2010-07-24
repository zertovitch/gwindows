with GNATCOM.Errors;

package body GNATOCX.IDataObject_Interface is

   procedure Initialize (This : in out IDataObject_Type) is
   begin
      Set_IID (This, IID_IDataObject);
   end Initialize;

   function Pointer (This : IDataObject_Type)
     return Pointer_To_IDataObject
   is
   begin
      return To_Pointer_To_IDataObject (Address (This));
   end Pointer;

   procedure Attach (This    : in out IDataObject_Type;
                     Pointer : in     Pointer_To_IDataObject)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteGetData
     (This          : IDataObject_Type;
      pformatetcIn  : Pointer_To_FORMATETC;
      pRemoteMedium : Pointer_To_wireSTGMEDIUM)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteGetData
         (Pointer (This),
          pformatetcIn,
          pRemoteMedium));

   end RemoteGetData;

   procedure RemoteGetDataHere
     (This          : IDataObject_Type;
      pformatetc    : Pointer_To_FORMATETC;
      pRemoteMedium : Pointer_To_wireSTGMEDIUM)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteGetDataHere
         (Pointer (This),
          pformatetc,
          pRemoteMedium));

   end RemoteGetDataHere;

   procedure QueryGetData
     (This       : IDataObject_Type;
      pformatetc : Pointer_To_FORMATETC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.QueryGetData
         (Pointer (This),
          pformatetc));

   end QueryGetData;

   procedure GetCanonicalFormatEtc
     (This          : IDataObject_Type;
      pformatectIn  : Pointer_To_FORMATETC;
      pformatetcOut : Pointer_To_FORMATETC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetCanonicalFormatEtc
         (Pointer (This),
          pformatectIn,
          pformatetcOut));

   end GetCanonicalFormatEtc;

   procedure RemoteSetData
     (This       : IDataObject_Type;
      pformatetc : Pointer_To_FORMATETC;
      pmedium    : Pointer_To_wireFLAG_STGMEDIUM;
      fRelease   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteSetData
         (Pointer (This),
          pformatetc,
          pmedium,
          fRelease));

   end RemoteSetData;

   procedure EnumFormatEtc
     (This            : IDataObject_Type;
      dwDirection     : Interfaces.C.unsigned_long;
      ppenumFormatEtc : Pointer_To_Pointer_To_IEnumFORMATETC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumFormatEtc
         (Pointer (This),
          dwDirection,
          ppenumFormatEtc));

   end EnumFormatEtc;

   procedure DAdvise
     (This          : IDataObject_Type;
      pformatetc    : Pointer_To_FORMATETC;
      advf          : Interfaces.C.unsigned_long;
      pAdvSink      : Pointer_To_IAdviseSink;
      pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DAdvise
         (Pointer (This),
          pformatetc,
          advf,
          pAdvSink,
          pdwConnection));

   end DAdvise;

   procedure DUnadvise
     (This         : IDataObject_Type;
      dwConnection : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DUnadvise
         (Pointer (This),
          dwConnection));

   end DUnadvise;

   procedure EnumDAdvise
     (This         : IDataObject_Type;
      ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumDAdvise
         (Pointer (This),
          ppenumAdvise));

   end EnumDAdvise;

end GNATOCX.IDataObject_Interface;
