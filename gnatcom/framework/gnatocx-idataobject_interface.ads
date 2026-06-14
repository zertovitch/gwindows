with GNATCOM.Iinterface;
with Win32_Types;

package GNATOCX.IDataObject_Interface is

   type IDataObject_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IDataObject_Type);

   function Pointer (This : IDataObject_Type)
     return Pointer_To_IDataObject;

   procedure Attach (This    : in out IDataObject_Type;
                     Pointer : in     Pointer_To_IDataObject);

   procedure RemoteGetData
     (This          : IDataObject_Type;
      pformatetcIn  : Pointer_To_FORMATETC;
      pRemoteMedium : Pointer_To_wireSTGMEDIUM);

   procedure RemoteGetDataHere
     (This          : IDataObject_Type;
      pformatetc    : Pointer_To_FORMATETC;
      pRemoteMedium : Pointer_To_wireSTGMEDIUM);

   procedure QueryGetData
     (This       : IDataObject_Type;
      pformatetc : Pointer_To_FORMATETC);

   procedure GetCanonicalFormatEtc
     (This          : IDataObject_Type;
      pformatectIn  : Pointer_To_FORMATETC;
      pformatetcOut : Pointer_To_FORMATETC);

   procedure RemoteSetData
     (This       : IDataObject_Type;
      pformatetc : Pointer_To_FORMATETC;
      pmedium    : Pointer_To_wireFLAG_STGMEDIUM;
      fRelease   : Win32_Types.Long);

   procedure EnumFormatEtc
     (This            : IDataObject_Type;
      dwDirection     : Win32_Types.Unsigned_Long;
      ppenumFormatEtc : Pointer_To_Pointer_To_IEnumFORMATETC);

   procedure DAdvise
     (This          : IDataObject_Type;
      pformatetc    : Pointer_To_FORMATETC;
      advf          : Win32_Types.Unsigned_Long;
      pAdvSink      : Pointer_To_IAdviseSink;
      pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure DUnadvise
     (This         : IDataObject_Type;
      dwConnection : Win32_Types.Unsigned_Long);

   procedure EnumDAdvise
     (This         : IDataObject_Type;
      ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA);

end GNATOCX.IDataObject_Interface;
