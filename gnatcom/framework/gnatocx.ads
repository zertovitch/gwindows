------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                              G N A T O C X                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  Support for direct Ada use of GUI ActiveX controls

with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

with GNATCOM.Types;
with GNATCOM.GUID;
with Win32_Types;

package GNATOCX is

   type IOleObject;
   type IOleClientSite;
   type IMoniker;
   type IStream;
   type uLARGE_INTEGER;
   type uULARGE_INTEGER;
   type STATSTG;
   type uFILETIME;
   type IBindCtx;
   type BIND_OPTS2;
   type uCOSERVERINFO;
   type uCOAUTHINFO;
   type uCOAUTHIDENTITY;
   type IRunningObjectTable;
   type IEnumMoniker;
   type IEnumString;
   type IOleContainer;
   type IEnumUnknown;
   type IDataObject;
   type FORMATETC;
   type wireCLIPFORMAT;
   type uuserCLIPFORMAT;
   type u_MIDL_IWinTypes_0001;
   type DVTARGETDEVICE;
   type wireSTGMEDIUM;
   type uuserSTGMEDIUM;
   type uSTGMEDIUM_UNION;
   type u_MIDL_IAdviseSink_0003;
   type uuserHMETAFILEPICT;
   type u_MIDL_IWinTypes_0005;
   type uremoteMETAFILEPICT;
   type uuserHMETAFILE;
   type u_MIDL_IWinTypes_0004;
   type uBYTE_BLOB;
   type uuserHENHMETAFILE;
   type u_MIDL_IWinTypes_0006;
   type uGDI_OBJECT;
   type u_MIDL_IAdviseSink_0002;
   type uuserHBITMAP;
   type u_MIDL_IWinTypes_0007;
   type uuserBITMAP;
   type uuserHPALETTE;
   type u_MIDL_IWinTypes_0008;
   type LOGPALETTE;
   type PALETTEENTRY;
   type uuserHGLOBAL;
   type u_MIDL_IWinTypes_0003;
   type uFLAGGED_BYTE_BLOB;
   type wireFLAG_STGMEDIUM;
   type uuserFLAG_STGMEDIUM;
   type IEnumFORMATETC;
   type IAdviseSink;
   type wireASYNC_STGMEDIUM;
   type IEnumSTATDATA;
   type STATDATA;
   type MSG;
   subtype wireHWND is System.Address;
   type uRemotableHandle;
   type u_MIDL_IWinTypes_0009;
   type POINT;
   type RECT;
   type IEnumOLEVERB;
   type OLEVERB;
   type SIZEL;
   type IPersistStream;
   type IPersist;
   type ISequentialStream;
   type IParseDisplayName;
   type IOleWindow;
   type IOleInPlaceObject;

   type Pointer_To_wireHWND is access all wireHWND;
   type Pointer_To_IOleObject is access all IOleObject;
   type Pointer_To_IOleClientSite is access all IOleClientSite;
   type Pointer_To_IMoniker is access all IMoniker;
   type Pointer_To_IStream is access all IStream;
   type Pointer_To_unsigned_char is access all Interfaces.C.unsigned_char;
   type Pointer_To_uULARGE_INTEGER is access all uULARGE_INTEGER;
   type Pointer_To_STATSTG is access all STATSTG;
   type Pointer_To_Pointer_To_IStream is access all Pointer_To_IStream;
   type Pointer_To_IBindCtx is access all IBindCtx;
   type Pointer_To_uCOAUTHIDENTITY is access all uCOAUTHIDENTITY;
   type Pointer_To_uCOAUTHINFO is access all uCOAUTHINFO;
   type Pointer_To_uCOSERVERINFO is access all uCOSERVERINFO;
   type Pointer_To_BIND_OPTS2 is access all BIND_OPTS2;
   type Pointer_To_IRunningObjectTable is access all IRunningObjectTable;
   type Pointer_To_uFILETIME is access all uFILETIME;
   type Pointer_To_IEnumMoniker is access all IEnumMoniker;
   type Pointer_To_Pointer_To_IMoniker is access all Pointer_To_IMoniker;
   type Pointer_To_Pointer_To_IEnumMoniker is
     access all Pointer_To_IEnumMoniker;
   type Pointer_To_Pointer_To_IRunningObjectTable is
     access all Pointer_To_IRunningObjectTable;
   type Pointer_To_IEnumString is access all IEnumString;
   type Pointer_To_Pointer_To_IEnumString is access all Pointer_To_IEnumString;
   type Pointer_To_IOleContainer is access all IOleContainer;
   type Pointer_To_IEnumUnknown is access all IEnumUnknown;
   type Pointer_To_Pointer_To_IEnumUnknown is
     access all Pointer_To_IEnumUnknown;
   type Pointer_To_Pointer_To_IOleContainer is
     access all Pointer_To_IOleContainer;
   type Pointer_To_Pointer_To_IOleClientSite is
     access all Pointer_To_IOleClientSite;
   type Pointer_To_IDataObject is access all IDataObject;
   type Pointer_To_uuserCLIPFORMAT is access all uuserCLIPFORMAT;
   type Pointer_To_DVTARGETDEVICE is access all DVTARGETDEVICE;
   type Pointer_To_FORMATETC is access all FORMATETC;
   type Pointer_To_uBYTE_BLOB is access all uBYTE_BLOB;
   type Pointer_To_uuserHMETAFILE is access all uuserHMETAFILE;
   type Pointer_To_uremoteMETAFILEPICT is access all uremoteMETAFILEPICT;
   type Pointer_To_uuserHMETAFILEPICT is access all uuserHMETAFILEPICT;
   type Pointer_To_uuserHENHMETAFILE is access all uuserHENHMETAFILE;
   type Pointer_To_uuserBITMAP is access all uuserBITMAP;
   type Pointer_To_uuserHBITMAP is access all uuserHBITMAP;
   type Pointer_To_PALETTEENTRY is access all PALETTEENTRY;
   type Pointer_To_LOGPALETTE is access all LOGPALETTE;
   type Pointer_To_uuserHPALETTE is access all uuserHPALETTE;
   type Pointer_To_uFLAGGED_BYTE_BLOB is access all uFLAGGED_BYTE_BLOB;
   type Pointer_To_uuserHGLOBAL is access all uuserHGLOBAL;
   type Pointer_To_uGDI_OBJECT is access all uGDI_OBJECT;
   type Pointer_To_uuserSTGMEDIUM is access all uuserSTGMEDIUM;
   type Pointer_To_wireSTGMEDIUM is access all wireSTGMEDIUM;
   type Pointer_To_uuserFLAG_STGMEDIUM is access all uuserFLAG_STGMEDIUM;
   type Pointer_To_wireFLAG_STGMEDIUM is access all wireFLAG_STGMEDIUM;
   type Pointer_To_IEnumFORMATETC is access all IEnumFORMATETC;
   type Pointer_To_Pointer_To_IEnumFORMATETC is
     access all Pointer_To_IEnumFORMATETC;
   type Pointer_To_IAdviseSink is access all IAdviseSink;
   type Pointer_To_wireASYNC_STGMEDIUM is access all wireASYNC_STGMEDIUM;
   type Pointer_To_IEnumSTATDATA is access all IEnumSTATDATA;
   type Pointer_To_STATDATA is access all STATDATA;
   type Pointer_To_Pointer_To_IEnumSTATDATA is
     access all Pointer_To_IEnumSTATDATA;
   type Pointer_To_Pointer_To_IDataObject is access all Pointer_To_IDataObject;
   type Pointer_To_uRemotableHandle is access all uRemotableHandle;
   type Pointer_To_MSG is access all MSG;
   type Pointer_To_RECT is access all RECT;
   type Pointer_To_IEnumOLEVERB is access all IEnumOLEVERB;
   type Pointer_To_OLEVERB is access all OLEVERB;
   type Pointer_To_Pointer_To_IEnumOLEVERB is
     access all Pointer_To_IEnumOLEVERB;
   type Pointer_To_SIZEL is access all SIZEL;
   type Pointer_To_IPersistStream is access all IPersistStream;
   type Pointer_To_IPersist is access all IPersist;
   type Pointer_To_ISequentialStream is access all ISequentialStream;
   type Pointer_To_IParseDisplayName is access all IParseDisplayName;
   type Pointer_To_IOleWindow is access all IOleWindow;
   type Pointer_To_IOleInPlaceObject is access all IOleInPlaceObject;

   type wireCLIPFORMAT is
     new Pointer_To_uuserCLIPFORMAT;

   type wireSTGMEDIUM is
     new Pointer_To_uuserSTGMEDIUM;

   type wireFLAG_STGMEDIUM is
     new Pointer_To_uuserFLAG_STGMEDIUM;

   type wireASYNC_STGMEDIUM is
     new Pointer_To_uuserSTGMEDIUM;

   Size_Of_uLARGE_INTEGER : constant := 64;

   --  Element Name          : _LARGE_INTEGER
   --  Element Type          : Record

   type uLARGE_INTEGER is
      record
         QuadPart : GNATCOM.Types.LONGLONG;
      end record;
   pragma Convention (C_Pass_By_Copy, uLARGE_INTEGER);
   for uLARGE_INTEGER use
      record
         QuadPart at 0 range 0 .. 0 + GNATCOM.Types.Size_Of_LONGLONG - 1;
      end record;
   for uLARGE_INTEGER'Size use Size_Of_uLARGE_INTEGER;

   Size_Of_uULARGE_INTEGER : constant := 64;

   --  Element Name          : _ULARGE_INTEGER
   --  Element Type          : Record

   type uULARGE_INTEGER is
      record
         QuadPart : GNATCOM.Types.DWORDLONG;
      end record;
   pragma Convention (C_Pass_By_Copy, uULARGE_INTEGER);
   for uULARGE_INTEGER use
      record
         QuadPart at 0 range 0 .. 0 + GNATCOM.Types.Size_Of_DWORDLONG - 1;
      end record;
   for uULARGE_INTEGER'Size use Size_Of_uULARGE_INTEGER;

   Size_Of_uFILETIME : constant := 64;

   --  Element Name          : _FILETIME
   --  Element Type          : Record

   type uFILETIME is
      record
         dwLowDateTime  : Interfaces.C.unsigned;
         dwHighDateTime : Interfaces.C.unsigned;
      end record;
   pragma Convention (C_Pass_By_Copy, uFILETIME);
   for uFILETIME use
      record
         dwLowDateTime at 0 range 0 .. 31;
         dwHighDateTime at 4 range 0 .. 31;
      end record;
   for uFILETIME'Size use Size_Of_uFILETIME;
   for uFILETIME'Alignment use 4;

   --  Element Name          : STATSTG
   --  Element Type          : Record

   type STATSTG is
      record
         pwcsName          : GNATCOM.Types.LPWSTR;
         utype             : Win32_Types.Unsigned_Long;
         cbSize            : uULARGE_INTEGER;
         mtime             : uFILETIME;
         ctime             : uFILETIME;
         atime             : uFILETIME;
         grfMode           : Win32_Types.Unsigned_Long;
         grfLocksSupported : Win32_Types.Unsigned_Long;
         clsid             : GNATCOM.Types.GUID;
         grfStateBits      : Win32_Types.Unsigned_Long;
         reserved          : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, STATSTG);

   IID_IStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000C-0000-0000-C000-000000000046}");

   type af_IStream_QueryInterface is access
     function (This   : access IStream;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_QueryInterface);
   pragma Machine_Attribute (af_IStream_QueryInterface, "ms_abi");

   type af_IStream_AddRef is access
     function (This : access IStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStream_AddRef);
   pragma Machine_Attribute (af_IStream_AddRef, "ms_abi");

   type af_IStream_Release is access
     function (This : access IStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStream_Release);
   pragma Machine_Attribute (af_IStream_Release, "ms_abi");

   type af_IStream_RemoteRead is access
     function (This    : access IStream;
               pv      : Pointer_To_unsigned_char;
               cb      : Win32_Types.Unsigned_Long;
               pcbRead : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteRead);
   pragma Machine_Attribute (af_IStream_RemoteRead, "ms_abi");

   type af_IStream_RemoteWrite is access
     function (This       : access IStream;
               pv         : Pointer_To_unsigned_char;
               cb         : Win32_Types.Unsigned_Long;
               pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteWrite);
   pragma Machine_Attribute (af_IStream_RemoteWrite, "ms_abi");

   type af_IStream_RemoteSeek is access
     function (This            : access IStream;
               dlibMove        : uLARGE_INTEGER;
               dwOrigin        : Win32_Types.Unsigned_Long;
               plibNewPosition : Pointer_To_uULARGE_INTEGER)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteSeek);
   pragma Machine_Attribute (af_IStream_RemoteSeek, "ms_abi");

   type af_IStream_SetSize is access
     function (This       : access IStream;
               libNewSize : uULARGE_INTEGER)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_SetSize);
   pragma Machine_Attribute (af_IStream_SetSize, "ms_abi");

   type af_IStream_RemoteCopyTo is access
     function (This       : access IStream;
               pstm       : Pointer_To_IStream;
               cb         : uULARGE_INTEGER;
               pcbRead    : Pointer_To_uULARGE_INTEGER;
               pcbWritten : Pointer_To_uULARGE_INTEGER)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteCopyTo);
   pragma Machine_Attribute (af_IStream_RemoteCopyTo, "ms_abi");

   type af_IStream_Commit is access
     function (This           : access IStream;
               grfCommitFlags : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Commit);
   pragma Machine_Attribute (af_IStream_Commit, "ms_abi");

   type af_IStream_Revert is access
     function (This : access IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Revert);
   pragma Machine_Attribute (af_IStream_Revert, "ms_abi");

   type af_IStream_LockRegion is access
     function (This       : access IStream;
               libOffset  : uULARGE_INTEGER;
               cb         : uULARGE_INTEGER;
               dwLockType : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_LockRegion);
   pragma Machine_Attribute (af_IStream_LockRegion, "ms_abi");

   type af_IStream_UnlockRegion is access
     function (This       : access IStream;
               libOffset  : uULARGE_INTEGER;
               cb         : uULARGE_INTEGER;
               dwLockType : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_UnlockRegion);
   pragma Machine_Attribute (af_IStream_UnlockRegion, "ms_abi");

   type af_IStream_Stat is access
     function (This        : access IStream;
               pstatstg    : Pointer_To_STATSTG;
               grfStatFlag : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Stat);
   pragma Machine_Attribute (af_IStream_Stat, "ms_abi");

   type af_IStream_Clone is access
     function (This  : access IStream;
               ppstm : Pointer_To_Pointer_To_IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Clone);
   pragma Machine_Attribute (af_IStream_Clone, "ms_abi");

   type IStreamVtbl;
   type Pointer_To_IStreamVtbl is access all IStreamVtbl;

   type IStream is
      record
         Vtbl : Pointer_To_IStreamVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IStream);

   type IStreamVtbl is
      record
         QueryInterface : af_IStream_QueryInterface;
         AddRef         : af_IStream_AddRef;
         Release        : af_IStream_Release;
         RemoteRead     : af_IStream_RemoteRead;
         RemoteWrite    : af_IStream_RemoteWrite;
         RemoteSeek     : af_IStream_RemoteSeek;
         SetSize        : af_IStream_SetSize;
         RemoteCopyTo   : af_IStream_RemoteCopyTo;
         Commit         : af_IStream_Commit;
         Revert         : af_IStream_Revert;
         LockRegion     : af_IStream_LockRegion;
         UnlockRegion   : af_IStream_UnlockRegion;
         Stat           : af_IStream_Stat;
         Clone          : af_IStream_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IStreamVtbl);

   function To_Pointer_To_IStream is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IStream);

   --  Element Name          : _COAUTHIDENTITY
   --  Element Type          : Record

   type uCOAUTHIDENTITY is
      record
         User           : GNATCOM.Types.Pointer_To_unsigned_short;
         UserLength     : Win32_Types.Unsigned_Long;
         Domain         : GNATCOM.Types.Pointer_To_unsigned_short;
         DomainLength   : Win32_Types.Unsigned_Long;
         Password       : GNATCOM.Types.Pointer_To_unsigned_short;
         PasswordLength : Win32_Types.Unsigned_Long;
         Flags          : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, uCOAUTHIDENTITY);
   --  Element Name          : _COAUTHINFO
   --  Element Type          : Record

   type uCOAUTHINFO is
      record
         dwAuthnSvc           : Win32_Types.Unsigned_Long;
         dwAuthzSvc           : Win32_Types.Unsigned_Long;
         pwszServerPrincName  : GNATCOM.Types.LPWSTR;
         dwAuthnLevel         : Win32_Types.Unsigned_Long;
         dwImpersonationLevel : Win32_Types.Unsigned_Long;
         pAuthIdentityData    : Pointer_To_uCOAUTHIDENTITY;
         dwCapabilities       : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, uCOAUTHINFO);
   --  Element Name          : _COSERVERINFO
   --  Element Type          : Record

   type uCOSERVERINFO is
      record
         dwReserved1 : Win32_Types.Unsigned_Long;
         pwszName    : GNATCOM.Types.LPWSTR;
         pAuthInfo   : Pointer_To_uCOAUTHINFO;
         dwReserved2 : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, uCOSERVERINFO);
   --  Element Name          : BIND_OPTS2
   --  Element Type          : Record

   type BIND_OPTS2 is
      record
         cbStruct            : Win32_Types.Unsigned_Long;
         grfFlags            : Win32_Types.Unsigned_Long;
         grfMode             : Win32_Types.Unsigned_Long;
         dwTickCountDeadline : Win32_Types.Unsigned_Long;
         dwTrackFlags        : Win32_Types.Unsigned_Long;
         dwClassContext      : Win32_Types.Unsigned_Long;
         locale              : Win32_Types.Unsigned_Long;
         pServerInfo         : Pointer_To_uCOSERVERINFO;
      end record;
   pragma Convention (C_Pass_By_Copy, BIND_OPTS2);
   --  Element Name          : IEnumMoniker
   --  Element Type          : Interface

   IID_IEnumMoniker : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000102-0000-0000-C000-000000000046}");

   type af_IEnumMoniker_QueryInterface is access
     function (This   : access IEnumMoniker;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_QueryInterface);
   pragma Machine_Attribute (af_IEnumMoniker_QueryInterface, "ms_abi");

   type af_IEnumMoniker_AddRef is access
     function (This : access IEnumMoniker)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumMoniker_AddRef);
   pragma Machine_Attribute (af_IEnumMoniker_AddRef, "ms_abi");

   type af_IEnumMoniker_Release is access
     function (This : access IEnumMoniker)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumMoniker_Release);
   pragma Machine_Attribute (af_IEnumMoniker_Release, "ms_abi");

   type af_IEnumMoniker_RemoteNext is access
     function (This         : access IEnumMoniker;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_Pointer_To_IMoniker;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_RemoteNext);
   pragma Machine_Attribute (af_IEnumMoniker_RemoteNext, "ms_abi");

   type af_IEnumMoniker_Skip is access
     function (This : access IEnumMoniker;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Skip);
   pragma Machine_Attribute (af_IEnumMoniker_Skip, "ms_abi");

   type af_IEnumMoniker_Reset is access
     function (This : access IEnumMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Reset);
   pragma Machine_Attribute (af_IEnumMoniker_Reset, "ms_abi");

   type af_IEnumMoniker_Clone is access
     function (This   : access IEnumMoniker;
               ppenum : Pointer_To_Pointer_To_IEnumMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Clone);
   pragma Machine_Attribute (af_IEnumMoniker_Clone, "ms_abi");

   type IEnumMonikerVtbl;
   type Pointer_To_IEnumMonikerVtbl is access all IEnumMonikerVtbl;

   type IEnumMoniker is
      record
         Vtbl : Pointer_To_IEnumMonikerVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumMoniker);

   type IEnumMonikerVtbl is
      record
         QueryInterface : af_IEnumMoniker_QueryInterface;
         AddRef         : af_IEnumMoniker_AddRef;
         Release        : af_IEnumMoniker_Release;
         RemoteNext     : af_IEnumMoniker_RemoteNext;
         Skip           : af_IEnumMoniker_Skip;
         Reset          : af_IEnumMoniker_Reset;
         Clone          : af_IEnumMoniker_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumMonikerVtbl);

   function To_Pointer_To_IEnumMoniker is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumMoniker);

   --  Element Name          : IRunningObjectTable
   --  Element Type          : Interface

   IID_IRunningObjectTable : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000010-0000-0000-C000-000000000046}");

   type af_IRunningObjectTable_QueryInterface is access
     function (This   : access IRunningObjectTable;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_QueryInterface);
   pragma Machine_Attribute (af_IRunningObjectTable_QueryInterface, "ms_abi");

   type af_IRunningObjectTable_AddRef is access
     function (This : access IRunningObjectTable)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IRunningObjectTable_AddRef);
   pragma Machine_Attribute (af_IRunningObjectTable_AddRef, "ms_abi");

   type af_IRunningObjectTable_Release is access
     function (This : access IRunningObjectTable)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IRunningObjectTable_Release);
   pragma Machine_Attribute (af_IRunningObjectTable_Release, "ms_abi");

   type af_IRunningObjectTable_Register is access
     function (This          : access IRunningObjectTable;
               grfFlags      : Win32_Types.Unsigned_Long;
               punkObject    : GNATCOM.Types.Pointer_To_IUnknown;
               pmkObjectName : Pointer_To_IMoniker;
               pdwRegister   : GNATCOM.Types.Pointer_To_DWORD)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_Register);
   pragma Machine_Attribute (af_IRunningObjectTable_Register, "ms_abi");

   type af_IRunningObjectTable_Revoke is access
     function (This       : access IRunningObjectTable;
               dwRegister : GNATCOM.Types.DWORD)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_Revoke);
   pragma Machine_Attribute (af_IRunningObjectTable_Revoke, "ms_abi");

   type af_IRunningObjectTable_IsRunning is access
     function (This          : access IRunningObjectTable;
               pmkObjectName : Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_IsRunning);
   pragma Machine_Attribute (af_IRunningObjectTable_IsRunning, "ms_abi");

   type af_IRunningObjectTable_GetObject is access
     function (This          : access IRunningObjectTable;
               pmkObjectName : Pointer_To_IMoniker;
               ppunkObject   : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_GetObject);
   pragma Machine_Attribute (af_IRunningObjectTable_GetObject, "ms_abi");

   type af_IRunningObjectTable_NoteChangeTime is access
     function (This       : access IRunningObjectTable;
               dwRegister : Win32_Types.Unsigned_Long;
               pfiletime  : Pointer_To_uFILETIME)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_NoteChangeTime);
   pragma Machine_Attribute (af_IRunningObjectTable_NoteChangeTime, "ms_abi");

   type af_IRunningObjectTable_GetTimeOfLastChange is access
     function (This          : access IRunningObjectTable;
               pmkObjectName : Pointer_To_IMoniker;
               pfiletime     : Pointer_To_uFILETIME)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_GetTimeOfLastChange);
   pragma Machine_Attribute (af_IRunningObjectTable_GetTimeOfLastChange, "ms_abi");

   type af_IRunningObjectTable_EnumRunning is access
     function (This          : access IRunningObjectTable;
               ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_EnumRunning);
   pragma Machine_Attribute (af_IRunningObjectTable_EnumRunning, "ms_abi");

   type IRunningObjectTableVtbl;
   type Pointer_To_IRunningObjectTableVtbl is
     access all IRunningObjectTableVtbl;

   type IRunningObjectTable is
      record
         Vtbl : Pointer_To_IRunningObjectTableVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IRunningObjectTable);

   type IRunningObjectTableVtbl is
      record
         QueryInterface      : af_IRunningObjectTable_QueryInterface;
         AddRef              : af_IRunningObjectTable_AddRef;
         Release             : af_IRunningObjectTable_Release;
         Register            : af_IRunningObjectTable_Register;
         Revoke              : af_IRunningObjectTable_Revoke;
         IsRunning           : af_IRunningObjectTable_IsRunning;
         GetObject           : af_IRunningObjectTable_GetObject;
         NoteChangeTime      : af_IRunningObjectTable_NoteChangeTime;
         GetTimeOfLastChange : af_IRunningObjectTable_GetTimeOfLastChange;
         EnumRunning         : af_IRunningObjectTable_EnumRunning;
      end record;
   pragma Convention (C_Pass_By_Copy, IRunningObjectTableVtbl);

   function To_Pointer_To_IRunningObjectTable is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IRunningObjectTable);

   --  Element Name          : IEnumString
   --  Element Type          : Interface

   IID_IEnumString : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000101-0000-0000-C000-000000000046}");

   type af_IEnumString_QueryInterface is access
     function (This   : access IEnumString;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_QueryInterface);
   pragma Machine_Attribute (af_IEnumString_QueryInterface, "ms_abi");

   type af_IEnumString_AddRef is access
     function (This : access IEnumString)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumString_AddRef);
   pragma Machine_Attribute (af_IEnumString_AddRef, "ms_abi");

   type af_IEnumString_Release is access
     function (This : access IEnumString)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumString_Release);
   pragma Machine_Attribute (af_IEnumString_Release, "ms_abi");

   type af_IEnumString_RemoteNext is access
     function (This         : access IEnumString;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : GNATCOM.Types.Pointer_To_LPWSTR;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_RemoteNext);
   pragma Machine_Attribute (af_IEnumString_RemoteNext, "ms_abi");

   type af_IEnumString_Skip is access
     function (This : access IEnumString;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Skip);
   pragma Machine_Attribute (af_IEnumString_Skip, "ms_abi");

   type af_IEnumString_Reset is access
     function (This : access IEnumString)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Reset);
   pragma Machine_Attribute (af_IEnumString_Reset, "ms_abi");

   type af_IEnumString_Clone is access
     function (This   : access IEnumString;
               ppenum : Pointer_To_Pointer_To_IEnumString)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Clone);
   pragma Machine_Attribute (af_IEnumString_Clone, "ms_abi");

   type IEnumStringVtbl;
   type Pointer_To_IEnumStringVtbl is access all IEnumStringVtbl;

   type IEnumString is
      record
         Vtbl : Pointer_To_IEnumStringVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumString);

   type IEnumStringVtbl is
      record
         QueryInterface : af_IEnumString_QueryInterface;
         AddRef         : af_IEnumString_AddRef;
         Release        : af_IEnumString_Release;
         RemoteNext     : af_IEnumString_RemoteNext;
         Skip           : af_IEnumString_Skip;
         Reset          : af_IEnumString_Reset;
         Clone          : af_IEnumString_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumStringVtbl);

   function To_Pointer_To_IEnumString is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumString);

   --  Element Name          : IBindCtx
   --  Element Type          : Interface

   IID_IBindCtx : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000E-0000-0000-C000-000000000046}");

   type af_IBindCtx_QueryInterface is access
     function (This   : access IBindCtx;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_QueryInterface);
   pragma Machine_Attribute (af_IBindCtx_QueryInterface, "ms_abi");

   type af_IBindCtx_AddRef is access
     function (This : access IBindCtx)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IBindCtx_AddRef);
   pragma Machine_Attribute (af_IBindCtx_AddRef, "ms_abi");

   type af_IBindCtx_Release is access
     function (This : access IBindCtx)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IBindCtx_Release);
   pragma Machine_Attribute (af_IBindCtx_Release, "ms_abi");

   type af_IBindCtx_RegisterObjectBound is access
     function (This : access IBindCtx;
               punk : GNATCOM.Types.Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RegisterObjectBound);
   pragma Machine_Attribute (af_IBindCtx_RegisterObjectBound, "ms_abi");

   type af_IBindCtx_RevokeObjectBound is access
     function (This : access IBindCtx;
               punk : GNATCOM.Types.Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RevokeObjectBound);
   pragma Machine_Attribute (af_IBindCtx_RevokeObjectBound, "ms_abi");

   type af_IBindCtx_ReleaseBoundObjects is access
     function (This : access IBindCtx)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_ReleaseBoundObjects);
   pragma Machine_Attribute (af_IBindCtx_ReleaseBoundObjects, "ms_abi");

   type af_IBindCtx_RemoteSetBindOptions is access
     function (This      : access IBindCtx;
               pbindopts : Pointer_To_BIND_OPTS2)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RemoteSetBindOptions);
   pragma Machine_Attribute (af_IBindCtx_RemoteSetBindOptions, "ms_abi");

   type af_IBindCtx_RemoteGetBindOptions is access
     function (This      : access IBindCtx;
               pbindopts : Pointer_To_BIND_OPTS2)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RemoteGetBindOptions);
   pragma Machine_Attribute (af_IBindCtx_RemoteGetBindOptions, "ms_abi");

   type af_IBindCtx_GetRunningObjectTable is access
     function (This  : access IBindCtx;
               pprot : Pointer_To_Pointer_To_IRunningObjectTable)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_GetRunningObjectTable);
   pragma Machine_Attribute (af_IBindCtx_GetRunningObjectTable, "ms_abi");

   type af_IBindCtx_RegisterObjectParam is access
     function (This   : access IBindCtx;
               pszKey : GNATCOM.Types.LPWSTR;
               punk   : GNATCOM.Types.Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RegisterObjectParam);
   pragma Machine_Attribute (af_IBindCtx_RegisterObjectParam, "ms_abi");

   type af_IBindCtx_GetObjectParam is access
     function (This   : access IBindCtx;
               pszKey : GNATCOM.Types.LPWSTR;
               ppunk  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_GetObjectParam);
   pragma Machine_Attribute (af_IBindCtx_GetObjectParam, "ms_abi");

   type af_IBindCtx_EnumObjectParam is access
     function (This   : access IBindCtx;
               ppenum : Pointer_To_Pointer_To_IEnumString)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_EnumObjectParam);
   pragma Machine_Attribute (af_IBindCtx_EnumObjectParam, "ms_abi");

   type af_IBindCtx_RevokeObjectParam is access
     function (This   : access IBindCtx;
               pszKey : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RevokeObjectParam);
   pragma Machine_Attribute (af_IBindCtx_RevokeObjectParam, "ms_abi");

   type IBindCtxVtbl;
   type Pointer_To_IBindCtxVtbl is access all IBindCtxVtbl;

   type IBindCtx is
      record
         Vtbl : Pointer_To_IBindCtxVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IBindCtx);

   type IBindCtxVtbl is
      record
         QueryInterface        : af_IBindCtx_QueryInterface;
         AddRef                : af_IBindCtx_AddRef;
         Release               : af_IBindCtx_Release;
         RegisterObjectBound   : af_IBindCtx_RegisterObjectBound;
         RevokeObjectBound     : af_IBindCtx_RevokeObjectBound;
         ReleaseBoundObjects   : af_IBindCtx_ReleaseBoundObjects;
         RemoteSetBindOptions  : af_IBindCtx_RemoteSetBindOptions;
         RemoteGetBindOptions  : af_IBindCtx_RemoteGetBindOptions;
         GetRunningObjectTable : af_IBindCtx_GetRunningObjectTable;
         RegisterObjectParam   : af_IBindCtx_RegisterObjectParam;
         GetObjectParam        : af_IBindCtx_GetObjectParam;
         EnumObjectParam       : af_IBindCtx_EnumObjectParam;
         RevokeObjectParam     : af_IBindCtx_RevokeObjectParam;
      end record;
   pragma Convention (C_Pass_By_Copy, IBindCtxVtbl);

   function To_Pointer_To_IBindCtx is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IBindCtx);

   --  Element Name          : IMoniker
   --  Element Type          : Interface

   IID_IMoniker : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000F-0000-0000-C000-000000000046}");

   type af_IMoniker_QueryInterface is access
     function (This   : access IMoniker;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_QueryInterface);
   pragma Machine_Attribute (af_IMoniker_QueryInterface, "ms_abi");

   type af_IMoniker_AddRef is access
     function (This : access IMoniker)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IMoniker_AddRef);
   pragma Machine_Attribute (af_IMoniker_AddRef, "ms_abi");

   type af_IMoniker_Release is access
     function (This : access IMoniker)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IMoniker_Release);
   pragma Machine_Attribute (af_IMoniker_Release, "ms_abi");

   type af_IMoniker_GetClassID is access
     function (This     : access IMoniker;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetClassID);
   pragma Machine_Attribute (af_IMoniker_GetClassID, "ms_abi");

   type af_IMoniker_IsDirty is access
     function (This : access IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsDirty);
   pragma Machine_Attribute (af_IMoniker_IsDirty, "ms_abi");

   type af_IMoniker_Load is access
     function (This : access IMoniker;
               pstm : Pointer_To_IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Load);
   pragma Machine_Attribute (af_IMoniker_Load, "ms_abi");

   type af_IMoniker_Save is access
     function (This        : access IMoniker;
               pstm        : Pointer_To_IStream;
               fClearDirty : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Save);
   pragma Machine_Attribute (af_IMoniker_Save, "ms_abi");

   type af_IMoniker_GetSizeMax is access
     function (This    : access IMoniker;
               pcbSize : Pointer_To_uULARGE_INTEGER)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetSizeMax);
   pragma Machine_Attribute (af_IMoniker_GetSizeMax, "ms_abi");

   type af_IMoniker_RemoteBindToObject is access
     function (This       : access IMoniker;
               pbc        : Pointer_To_IBindCtx;
               pmkToLeft  : Pointer_To_IMoniker;
               riidResult : GNATCOM.Types.Pointer_To_GUID;
               ppvResult  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RemoteBindToObject);
   pragma Machine_Attribute (af_IMoniker_RemoteBindToObject, "ms_abi");

   type af_IMoniker_RemoteBindToStorage is access
     function (This      : access IMoniker;
               pbc       : Pointer_To_IBindCtx;
               pmkToLeft : Pointer_To_IMoniker;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObj    : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RemoteBindToStorage);
   pragma Machine_Attribute (af_IMoniker_RemoteBindToStorage, "ms_abi");

   type af_IMoniker_Reduce is access
     function (This           : access IMoniker;
               pbc            : Pointer_To_IBindCtx;
               dwReduceHowFar : Win32_Types.Unsigned_Long;
               ppmkToLeft     : Pointer_To_Pointer_To_IMoniker;
               ppmkReduced    : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Reduce);
   pragma Machine_Attribute (af_IMoniker_Reduce, "ms_abi");

   type af_IMoniker_ComposeWith is access
     function (This              : access IMoniker;
               pmkRight          : Pointer_To_IMoniker;
               fOnlyIfNotGeneric : Win32_Types.Long;
               ppmkComposite     : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_ComposeWith);
   pragma Machine_Attribute (af_IMoniker_ComposeWith, "ms_abi");

   type af_IMoniker_Enum is access
     function (This          : access IMoniker;
               fForward      : Win32_Types.Long;
               ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Enum);
   pragma Machine_Attribute (af_IMoniker_Enum, "ms_abi");

   type af_IMoniker_IsEqual is access
     function (This            : access IMoniker;
               pmkOtherMoniker : Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsEqual);
   pragma Machine_Attribute (af_IMoniker_IsEqual, "ms_abi");

   type af_IMoniker_Hash is access
     function (This    : access IMoniker;
               pdwHash : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Hash);
   pragma Machine_Attribute (af_IMoniker_Hash, "ms_abi");

   type af_IMoniker_IsRunning is access
     function (This            : access IMoniker;
               pbc             : Pointer_To_IBindCtx;
               pmkToLeft       : Pointer_To_IMoniker;
               pmkNewlyRunning : Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsRunning);
   pragma Machine_Attribute (af_IMoniker_IsRunning, "ms_abi");

   type af_IMoniker_GetTimeOfLastChange is access
     function (This      : access IMoniker;
               pbc       : Pointer_To_IBindCtx;
               pmkToLeft : Pointer_To_IMoniker;
               pfiletime : Pointer_To_uFILETIME)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetTimeOfLastChange);
   pragma Machine_Attribute (af_IMoniker_GetTimeOfLastChange, "ms_abi");

   type af_IMoniker_Inverse is access
     function (This : access IMoniker;
               ppmk : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Inverse);
   pragma Machine_Attribute (af_IMoniker_Inverse, "ms_abi");

   type af_IMoniker_CommonPrefixWith is access
     function (This       : access IMoniker;
               pmkOther   : Pointer_To_IMoniker;
               ppmkPrefix : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_CommonPrefixWith);
   pragma Machine_Attribute (af_IMoniker_CommonPrefixWith, "ms_abi");

   type af_IMoniker_RelativePathTo is access
     function (This        : access IMoniker;
               pmkOther    : Pointer_To_IMoniker;
               ppmkRelPath : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RelativePathTo);
   pragma Machine_Attribute (af_IMoniker_RelativePathTo, "ms_abi");

   type af_IMoniker_GetDisplayName is access
     function (This            : access IMoniker;
               pbc             : Pointer_To_IBindCtx;
               pmkToLeft       : Pointer_To_IMoniker;
               ppszDisplayName : GNATCOM.Types.Pointer_To_LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetDisplayName);
   pragma Machine_Attribute (af_IMoniker_GetDisplayName, "ms_abi");

   type af_IMoniker_ParseDisplayName is access
     function (This           : access IMoniker;
               pbc            : Pointer_To_IBindCtx;
               pmkToLeft      : Pointer_To_IMoniker;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_ParseDisplayName);
   pragma Machine_Attribute (af_IMoniker_ParseDisplayName, "ms_abi");

   type af_IMoniker_IsSystemMoniker is access
     function (This     : access IMoniker;
               pdwMksys : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsSystemMoniker);
   pragma Machine_Attribute (af_IMoniker_IsSystemMoniker, "ms_abi");

   type IMonikerVtbl;
   type Pointer_To_IMonikerVtbl is access all IMonikerVtbl;

   type IMoniker is
      record
         Vtbl : Pointer_To_IMonikerVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IMoniker);

   type IMonikerVtbl is
      record
         QueryInterface      : af_IMoniker_QueryInterface;
         AddRef              : af_IMoniker_AddRef;
         Release             : af_IMoniker_Release;
         GetClassID          : af_IMoniker_GetClassID;
         IsDirty             : af_IMoniker_IsDirty;
         Load                : af_IMoniker_Load;
         Save                : af_IMoniker_Save;
         GetSizeMax          : af_IMoniker_GetSizeMax;
         RemoteBindToObject  : af_IMoniker_RemoteBindToObject;
         RemoteBindToStorage : af_IMoniker_RemoteBindToStorage;
         Reduce              : af_IMoniker_Reduce;
         ComposeWith         : af_IMoniker_ComposeWith;
         Enum                : af_IMoniker_Enum;
         IsEqual             : af_IMoniker_IsEqual;
         Hash                : af_IMoniker_Hash;
         IsRunning           : af_IMoniker_IsRunning;
         GetTimeOfLastChange : af_IMoniker_GetTimeOfLastChange;
         Inverse             : af_IMoniker_Inverse;
         CommonPrefixWith    : af_IMoniker_CommonPrefixWith;
         RelativePathTo      : af_IMoniker_RelativePathTo;
         GetDisplayName      : af_IMoniker_GetDisplayName;
         ParseDisplayName    : af_IMoniker_ParseDisplayName;
         IsSystemMoniker     : af_IMoniker_IsSystemMoniker;
      end record;
   pragma Convention (C_Pass_By_Copy, IMonikerVtbl);

   function To_Pointer_To_IMoniker is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IMoniker);

   --  Element Name          : IEnumUnknown
   --  Element Type          : Interface

   IID_IEnumUnknown : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000100-0000-0000-C000-000000000046}");

   type af_IEnumUnknown_QueryInterface is access
     function (This   : access IEnumUnknown;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_QueryInterface);
   pragma Machine_Attribute (af_IEnumUnknown_QueryInterface, "ms_abi");

   type af_IEnumUnknown_AddRef is access
     function (This : access IEnumUnknown)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumUnknown_AddRef);
   pragma Machine_Attribute (af_IEnumUnknown_AddRef, "ms_abi");

   type af_IEnumUnknown_Release is access
     function (This : access IEnumUnknown)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumUnknown_Release);
   pragma Machine_Attribute (af_IEnumUnknown_Release, "ms_abi");

   type af_IEnumUnknown_RemoteNext is access
     function (This         : access IEnumUnknown;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_RemoteNext);
   pragma Machine_Attribute (af_IEnumUnknown_RemoteNext, "ms_abi");

   type af_IEnumUnknown_Skip is access
     function (This : access IEnumUnknown;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Skip);
   pragma Machine_Attribute (af_IEnumUnknown_Skip, "ms_abi");

   type af_IEnumUnknown_Reset is access
     function (This : access IEnumUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Reset);
   pragma Machine_Attribute (af_IEnumUnknown_Reset, "ms_abi");

   type af_IEnumUnknown_Clone is access
     function (This   : access IEnumUnknown;
               ppenum : Pointer_To_Pointer_To_IEnumUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Clone);
   pragma Machine_Attribute (af_IEnumUnknown_Clone, "ms_abi");

   type IEnumUnknownVtbl;
   type Pointer_To_IEnumUnknownVtbl is access all IEnumUnknownVtbl;

   type IEnumUnknown is
      record
         Vtbl : Pointer_To_IEnumUnknownVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumUnknown);

   type IEnumUnknownVtbl is
      record
         QueryInterface : af_IEnumUnknown_QueryInterface;
         AddRef         : af_IEnumUnknown_AddRef;
         Release        : af_IEnumUnknown_Release;
         RemoteNext     : af_IEnumUnknown_RemoteNext;
         Skip           : af_IEnumUnknown_Skip;
         Reset          : af_IEnumUnknown_Reset;
         Clone          : af_IEnumUnknown_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumUnknownVtbl);

   function To_Pointer_To_IEnumUnknown is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumUnknown);

   --  Element Name          : IOleContainer
   --  Element Type          : Interface

   IID_IOleContainer : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000011B-0000-0000-C000-000000000046}");

   type af_IOleContainer_QueryInterface is access
     function (This   : access IOleContainer;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_QueryInterface);
   pragma Machine_Attribute (af_IOleContainer_QueryInterface, "ms_abi");

   type af_IOleContainer_AddRef is access
     function (This : access IOleContainer)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleContainer_AddRef);
   pragma Machine_Attribute (af_IOleContainer_AddRef, "ms_abi");

   type af_IOleContainer_Release is access
     function (This : access IOleContainer)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleContainer_Release);
   pragma Machine_Attribute (af_IOleContainer_Release, "ms_abi");

   type af_IOleContainer_ParseDisplayName is access
     function (This           : access IOleContainer;
               pbc            : Pointer_To_IBindCtx;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_ParseDisplayName);
   pragma Machine_Attribute (af_IOleContainer_ParseDisplayName, "ms_abi");

   type af_IOleContainer_EnumObjects is access
     function (This     : access IOleContainer;
               grfFlags : Win32_Types.Unsigned_Long;
               ppenum   : Pointer_To_Pointer_To_IEnumUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_EnumObjects);
   pragma Machine_Attribute (af_IOleContainer_EnumObjects, "ms_abi");

   type af_IOleContainer_LockContainer is access
     function (This  : access IOleContainer;
               fLock : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_LockContainer);
   pragma Machine_Attribute (af_IOleContainer_LockContainer, "ms_abi");

   type IOleContainerVtbl;
   type Pointer_To_IOleContainerVtbl is access all IOleContainerVtbl;

   type IOleContainer is
      record
         Vtbl : Pointer_To_IOleContainerVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleContainer);

   type IOleContainerVtbl is
      record
         QueryInterface   : af_IOleContainer_QueryInterface;
         AddRef           : af_IOleContainer_AddRef;
         Release          : af_IOleContainer_Release;
         ParseDisplayName : af_IOleContainer_ParseDisplayName;
         EnumObjects      : af_IOleContainer_EnumObjects;
         LockContainer    : af_IOleContainer_LockContainer;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleContainerVtbl);

   function To_Pointer_To_IOleContainer is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleContainer);

   --  Element Name          : IOleClientSite
   --  Element Type          : Interface

   IID_IOleClientSite : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000118-0000-0000-C000-000000000046}");

   type af_IOleClientSite_QueryInterface is access
     function (This   : access IOleClientSite;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_QueryInterface);
   pragma Machine_Attribute (af_IOleClientSite_QueryInterface, "ms_abi");

   type af_IOleClientSite_AddRef is access
     function (This : access IOleClientSite)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleClientSite_AddRef);
   pragma Machine_Attribute (af_IOleClientSite_AddRef, "ms_abi");

   type af_IOleClientSite_Release is access
     function (This : access IOleClientSite)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleClientSite_Release);
   pragma Machine_Attribute (af_IOleClientSite_Release, "ms_abi");

   type af_IOleClientSite_SaveObject is access
     function (This : access IOleClientSite)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_SaveObject);
   pragma Machine_Attribute (af_IOleClientSite_SaveObject, "ms_abi");

   type af_IOleClientSite_GetMoniker is access
     function (This           : access IOleClientSite;
               dwAssign       : Win32_Types.Unsigned_Long;
               dwWhichMoniker : Win32_Types.Unsigned_Long;
               ppmk           : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_GetMoniker);
   pragma Machine_Attribute (af_IOleClientSite_GetMoniker, "ms_abi");

   type af_IOleClientSite_GetContainer is access
     function (This        : access IOleClientSite;
               ppContainer : Pointer_To_Pointer_To_IOleContainer)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_GetContainer);
   pragma Machine_Attribute (af_IOleClientSite_GetContainer, "ms_abi");

   type af_IOleClientSite_ShowObject is access
     function (This : access IOleClientSite)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_ShowObject);
   pragma Machine_Attribute (af_IOleClientSite_ShowObject, "ms_abi");

   type af_IOleClientSite_OnShowWindow is access
     function (This  : access IOleClientSite;
               fShow : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_OnShowWindow);
   pragma Machine_Attribute (af_IOleClientSite_OnShowWindow, "ms_abi");

   type af_IOleClientSite_RequestNewObjectLayout is access
     function (This : access IOleClientSite)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_RequestNewObjectLayout);
   pragma Machine_Attribute (af_IOleClientSite_RequestNewObjectLayout, "ms_abi");

   type IOleClientSiteVtbl;
   type Pointer_To_IOleClientSiteVtbl is access all IOleClientSiteVtbl;

   type IOleClientSite is
      record
         Vtbl : Pointer_To_IOleClientSiteVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleClientSite);

   type IOleClientSiteVtbl is
      record
         QueryInterface         : af_IOleClientSite_QueryInterface;
         AddRef                 : af_IOleClientSite_AddRef;
         Release                : af_IOleClientSite_Release;
         SaveObject             : af_IOleClientSite_SaveObject;
         GetMoniker             : af_IOleClientSite_GetMoniker;
         GetContainer           : af_IOleClientSite_GetContainer;
         ShowObject             : af_IOleClientSite_ShowObject;
         OnShowWindow           : af_IOleClientSite_OnShowWindow;
         RequestNewObjectLayout : af_IOleClientSite_RequestNewObjectLayout;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleClientSiteVtbl);

   function To_Pointer_To_IOleClientSite is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleClientSite);

   --  Element Name          : __MIDL_IWinTypes_0001
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0001_Range is Positive range 1 .. 2;
   type u_MIDL_IWinTypes_0001 (Which : u_MIDL_IWinTypes_0001_Range := 1) is
      record
         case Which is
            when 1 =>
               dwValue  : Win32_Types.Unsigned_Long;
            when 2 =>
               pwszName : GNATCOM.Types.LPWSTR;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0001);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0001);

   --  Element Name          : _userCLIPFORMAT
   --  Element Type          : Record

   type uuserCLIPFORMAT is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0001;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserCLIPFORMAT);

   --  Element Name          : DVTARGETDEVICE
   --  Element Type          : Record

   type DVTARGETDEVICE is
      record
         tdSize             : Win32_Types.Unsigned_Long;
         tdDriverNameOffset : Interfaces.C.unsigned_short;
         tdDeviceNameOffset : Interfaces.C.unsigned_short;
         tdPortNameOffset   : Interfaces.C.unsigned_short;
         tdExtDevmodeOffset : Interfaces.C.unsigned_short;
         tdData             : Pointer_To_unsigned_char;
      end record;
   pragma Convention (C_Pass_By_Copy, DVTARGETDEVICE);

   --  Element Name          : FORMATETC
   --  Element Type          : Record

   type FORMATETC is
      record
         cfFormat : wireCLIPFORMAT;
         ptd      : Pointer_To_DVTARGETDEVICE;
         dwAspect : Win32_Types.Unsigned_Long;
         lindex   : Win32_Types.Long;
         tymed    : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, FORMATETC);

   --  Element Name          : _BYTE_BLOB
   --  Element Type          : Record

   type uBYTE_BLOB is
      record
         clSize : Win32_Types.Unsigned_Long;
         abData : Pointer_To_unsigned_char;
      end record;
   pragma Convention (C_Pass_By_Copy, uBYTE_BLOB);

   Size_Of_u_MIDL_IWinTypes_0004 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0004
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0004_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0004 (Which : u_MIDL_IWinTypes_0004_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_uBYTE_BLOB;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0004);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0004);
   for u_MIDL_IWinTypes_0004'Size use Size_Of_u_MIDL_IWinTypes_0004;

   Size_Of_uuserHMETAFILE : constant := 128;

   --  Element Name          : _userHMETAFILE
   --  Element Type          : Record

   type uuserHMETAFILE is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0004;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHMETAFILE);
   for uuserHMETAFILE use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0004 - 1;
      end record;
   for uuserHMETAFILE'Size use Size_Of_uuserHMETAFILE;

   --  Element Name          : _remoteMETAFILEPICT
   --  Element Type          : Record

   type uremoteMETAFILEPICT is
      record
         mm   : Win32_Types.Long;
         xExt : Win32_Types.Long;
         yExt : Win32_Types.Long;
         hMF  : Pointer_To_uuserHMETAFILE;
      end record;
   pragma Convention (C_Pass_By_Copy, uremoteMETAFILEPICT);

   Size_Of_u_MIDL_IWinTypes_0005 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0005
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0005_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0005 (Which : u_MIDL_IWinTypes_0005_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_uremoteMETAFILEPICT;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0005);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0005);
   for u_MIDL_IWinTypes_0005'Size use Size_Of_u_MIDL_IWinTypes_0005;

   Size_Of_uuserHMETAFILEPICT : constant := 128;

   --  Element Name          : _userHMETAFILEPICT
   --  Element Type          : Record

   type uuserHMETAFILEPICT is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0005;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHMETAFILEPICT);
   for uuserHMETAFILEPICT use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0005 - 1;
      end record;
   for uuserHMETAFILEPICT'Size use Size_Of_uuserHMETAFILEPICT;

   Size_Of_u_MIDL_IWinTypes_0006 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0006
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0006_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0006 (Which : u_MIDL_IWinTypes_0006_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_uBYTE_BLOB;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0006);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0006);
   for u_MIDL_IWinTypes_0006'Size use Size_Of_u_MIDL_IWinTypes_0006;

   Size_Of_uuserHENHMETAFILE : constant := 128;

   --  Element Name          : _userHENHMETAFILE
   --  Element Type          : Record

   type uuserHENHMETAFILE is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0006;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHENHMETAFILE);
   for uuserHENHMETAFILE use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0006 - 1;
      end record;
   for uuserHENHMETAFILE'Size use Size_Of_uuserHENHMETAFILE;

   --  Element Name          : _userBITMAP
   --  Element Type          : Record

   type uuserBITMAP is
      record
         bmType       : Win32_Types.Long;
         bmWidth      : Win32_Types.Long;
         bmHeight     : Win32_Types.Long;
         bmWidthBytes : Win32_Types.Long;
         bmPlanes     : Interfaces.C.unsigned_short;
         bmBitsPixel  : Interfaces.C.unsigned_short;
         cbSize       : Win32_Types.Unsigned_Long;
         pBuffer      : Pointer_To_unsigned_char;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserBITMAP);

   Size_Of_u_MIDL_IWinTypes_0007 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0007
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0007_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0007 (Which : u_MIDL_IWinTypes_0007_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_uuserBITMAP;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0007);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0007);
   for u_MIDL_IWinTypes_0007'Size use Size_Of_u_MIDL_IWinTypes_0007;

   Size_Of_uuserHBITMAP : constant := 128;

   --  Element Name          : _userHBITMAP
   --  Element Type          : Record

   type uuserHBITMAP is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0007;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHBITMAP);
   for uuserHBITMAP use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0007 - 1;
      end record;
   for uuserHBITMAP'Size use Size_Of_uuserHBITMAP;

   Size_Of_PALETTEENTRY : constant := 32;

   --  Element Name          : PALETTEENTRY
   --  Element Type          : Record

   type PALETTEENTRY is
      record
         peRed   : Interfaces.C.unsigned_char;
         peGreen : Interfaces.C.unsigned_char;
         peBlue  : Interfaces.C.unsigned_char;
         peFlags : Interfaces.C.unsigned_char;
      end record;
   pragma Convention (C_Pass_By_Copy, PALETTEENTRY);
   for PALETTEENTRY use
      record
         peRed   at 0 range 0 .. 0 + Interfaces.C.unsigned_char'Size - 1;
         peGreen at 0 range 8 .. 8 + Interfaces.C.unsigned_char'Size - 1;
         peBlue  at 0 range 16 .. 16 + Interfaces.C.unsigned_char'Size - 1;
         peFlags at 0 range 24 .. 24 + Interfaces.C.unsigned_char'Size - 1;
      end record;
   for PALETTEENTRY'Size use Size_Of_PALETTEENTRY;
   for PALETTEENTRY'Alignment use 1;

   --  Element Name          : LOGPALETTE
   --  Element Type          : Record

   type LOGPALETTE is
      record
         palVersion    : Interfaces.C.unsigned_short;
         palNumEntries : Interfaces.C.unsigned_short;
         palPalEntry   : Pointer_To_PALETTEENTRY;
      end record;
   pragma Convention (C_Pass_By_Copy, LOGPALETTE);

   Size_Of_u_MIDL_IWinTypes_0008 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0008
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0008_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0008 (Which : u_MIDL_IWinTypes_0008_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_LOGPALETTE;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0008);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0008);
   for u_MIDL_IWinTypes_0008'Size use Size_Of_u_MIDL_IWinTypes_0008;

   Size_Of_uuserHPALETTE : constant := 128;

   --  Element Name          : _userHPALETTE
   --  Element Type          : Record

   type uuserHPALETTE is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0008;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHPALETTE);
   for uuserHPALETTE use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0008 - 1;
      end record;
   for uuserHPALETTE'Size use Size_Of_uuserHPALETTE;

   --  Element Name          : _FLAGGED_BYTE_BLOB
   --  Element Type          : Record

   type uFLAGGED_BYTE_BLOB is
      record
         fFlags : Win32_Types.Unsigned_Long;
         clSize : Win32_Types.Unsigned_Long;
         abData : Pointer_To_unsigned_char;
      end record;
   pragma Convention (C_Pass_By_Copy, uFLAGGED_BYTE_BLOB);

   Size_Of_u_MIDL_IWinTypes_0003 : constant := 64;

   --  Element Name          : __MIDL_IWinTypes_0003
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0003_Range is Positive range 1 .. 3;
   type u_MIDL_IWinTypes_0003 (Which : u_MIDL_IWinTypes_0003_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc   : Win32_Types.Long;
            when 2 =>
               hRemote   : Pointer_To_uFLAGGED_BYTE_BLOB;
            when 3 =>
               hInproc64 : GNATCOM.Types.LONGLONG;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0003);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0003);
   for u_MIDL_IWinTypes_0003'Size use Size_Of_u_MIDL_IWinTypes_0003;

   Size_Of_uuserHGLOBAL : constant := 128;

   --  Element Name          : _userHGLOBAL
   --  Element Type          : Record

   type uuserHGLOBAL is
      record
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0003;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserHGLOBAL);
   for uuserHGLOBAL use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 64 .. 64 + Size_Of_u_MIDL_IWinTypes_0003 - 1;
      end record;
   for uuserHGLOBAL'Size use Size_Of_uuserHGLOBAL;

   --  Element Name          : __MIDL_IAdviseSink_0002
   --  Element Type          : Union

   subtype u_MIDL_IAdviseSink_0002_Range is Positive range 1 .. 3;
   type u_MIDL_IAdviseSink_0002 (Which : u_MIDL_IAdviseSink_0002_Range := 1) is
      record
         case Which is
            when 1 =>
               hBitmap  : Pointer_To_uuserHBITMAP;
            when 2 =>
               hPalette : Pointer_To_uuserHPALETTE;
            when 3 =>
               hGeneric : Pointer_To_uuserHGLOBAL;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IAdviseSink_0002);
   pragma Unchecked_Union (u_MIDL_IAdviseSink_0002);

   --  Element Name          : _GDI_OBJECT
   --  Element Type          : Record

   type uGDI_OBJECT is
      record
         ObjectType : Win32_Types.Unsigned_Long;
         u          : u_MIDL_IAdviseSink_0002;
      end record;
   pragma Convention (C_Pass_By_Copy, uGDI_OBJECT);

   --  Element Name          : __MIDL_IAdviseSink_0003
   --  Element Type          : Union

   subtype u_MIDL_IAdviseSink_0003_Range is Positive range 1 .. 7;
   type u_MIDL_IAdviseSink_0003 (Which : u_MIDL_IAdviseSink_0003_Range := 1) is
      record
         case Which is
            when 1 =>
               hMetaFilePict : Pointer_To_uuserHMETAFILEPICT;
            when 2 =>
               hHEnhMetaFile : Pointer_To_uuserHENHMETAFILE;
            when 3 =>
               hGdiHandle    : Pointer_To_uGDI_OBJECT;
            when 4 =>
               hGlobal       : Pointer_To_uuserHGLOBAL;
            when 5 =>
               lpszFileName  : GNATCOM.Types.LPWSTR;
            when 6 =>
               pstm          : Pointer_To_uBYTE_BLOB;
            when 7 =>
               pstg          : Pointer_To_uBYTE_BLOB;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IAdviseSink_0003);
   pragma Unchecked_Union (u_MIDL_IAdviseSink_0003);

   --  Element Name          : _STGMEDIUM_UNION
   --  Element Type          : Record

   type uSTGMEDIUM_UNION is
      record
         tymed : Win32_Types.Unsigned_Long;
         u     : u_MIDL_IAdviseSink_0003;
      end record;
   pragma Convention (C_Pass_By_Copy, uSTGMEDIUM_UNION);

   --  Element Name          : _userSTGMEDIUM
   --  Element Type          : Record

   type uuserSTGMEDIUM is
      record
         u_MIDL_0003    : uSTGMEDIUM_UNION;
         pUnkForRelease : GNATCOM.Types.Pointer_To_IUnknown;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserSTGMEDIUM);

   --  Element Name          : _userFLAG_STGMEDIUM
   --  Element Type          : Record

   type uuserFLAG_STGMEDIUM is
      record
         ContextFlags   : Win32_Types.Long;
         fPassOwnership : Win32_Types.Long;
         Stgmed         : uuserSTGMEDIUM;
      end record;
   pragma Convention (C_Pass_By_Copy, uuserFLAG_STGMEDIUM);

   --  Element Name          : IEnumFORMATETC
   --  Element Type          : Interface

   IID_IEnumFORMATETC : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000103-0000-0000-C000-000000000046}");

   type af_IEnumFORMATETC_QueryInterface is access
     function (This   : access IEnumFORMATETC;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumFORMATETC_QueryInterface);
   pragma Machine_Attribute (af_IEnumFORMATETC_QueryInterface, "ms_abi");

   type af_IEnumFORMATETC_AddRef is access
     function (This : access IEnumFORMATETC)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumFORMATETC_AddRef);
   pragma Machine_Attribute (af_IEnumFORMATETC_AddRef, "ms_abi");

   type af_IEnumFORMATETC_Release is access
     function (This : access IEnumFORMATETC)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumFORMATETC_Release);
   pragma Machine_Attribute (af_IEnumFORMATETC_Release, "ms_abi");

   type af_IEnumFORMATETC_RemoteNext is access
     function (This         : access IEnumFORMATETC;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_FORMATETC;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumFORMATETC_RemoteNext);
   pragma Machine_Attribute (af_IEnumFORMATETC_RemoteNext, "ms_abi");

   type af_IEnumFORMATETC_Skip is access
     function (This : access IEnumFORMATETC;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumFORMATETC_Skip);
   pragma Machine_Attribute (af_IEnumFORMATETC_Skip, "ms_abi");

   type af_IEnumFORMATETC_Reset is access
     function (This : access IEnumFORMATETC)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumFORMATETC_Reset);
   pragma Machine_Attribute (af_IEnumFORMATETC_Reset, "ms_abi");

   type af_IEnumFORMATETC_Clone is access
     function (This   : access IEnumFORMATETC;
               ppenum : Pointer_To_Pointer_To_IEnumFORMATETC)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumFORMATETC_Clone);
   pragma Machine_Attribute (af_IEnumFORMATETC_Clone, "ms_abi");

   type IEnumFORMATETCVtbl;
   type Pointer_To_IEnumFORMATETCVtbl is access all IEnumFORMATETCVtbl;

   type IEnumFORMATETC is
      record
         Vtbl : Pointer_To_IEnumFORMATETCVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumFORMATETC);

   type IEnumFORMATETCVtbl is
      record
         QueryInterface : af_IEnumFORMATETC_QueryInterface;
         AddRef         : af_IEnumFORMATETC_AddRef;
         Release        : af_IEnumFORMATETC_Release;
         RemoteNext     : af_IEnumFORMATETC_RemoteNext;
         Skip           : af_IEnumFORMATETC_Skip;
         Reset          : af_IEnumFORMATETC_Reset;
         Clone          : af_IEnumFORMATETC_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumFORMATETCVtbl);

   function To_Pointer_To_IEnumFORMATETC is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumFORMATETC);

   --  Element Name          : IAdviseSink
   --  Element Type          : Interface

   IID_IAdviseSink : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000010F-0000-0000-C000-000000000046}");

   type af_IAdviseSink_QueryInterface is access
     function (This   : access IAdviseSink;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_QueryInterface);
   pragma Machine_Attribute (af_IAdviseSink_QueryInterface, "ms_abi");

   type af_IAdviseSink_AddRef is access
     function (This : access IAdviseSink)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IAdviseSink_AddRef);
   pragma Machine_Attribute (af_IAdviseSink_AddRef, "ms_abi");

   type af_IAdviseSink_Release is access
     function (This : access IAdviseSink)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IAdviseSink_Release);
   pragma Machine_Attribute (af_IAdviseSink_Release, "ms_abi");

   type af_IAdviseSink_RemoteOnDataChange is access
     function (This       : access IAdviseSink;
               pformatetc : Pointer_To_FORMATETC;
               pStgmed    : Pointer_To_wireASYNC_STGMEDIUM)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_RemoteOnDataChange);
   pragma Machine_Attribute (af_IAdviseSink_RemoteOnDataChange, "ms_abi");

   type af_IAdviseSink_RemoteOnViewChange is access
     function (This     : access IAdviseSink;
               dwAspect : Win32_Types.Unsigned_Long;
               lindex   : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_RemoteOnViewChange);
   pragma Machine_Attribute (af_IAdviseSink_RemoteOnViewChange, "ms_abi");

   type af_IAdviseSink_RemoteOnRename is access
     function (This : access IAdviseSink;
               pmk  : Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_RemoteOnRename);
   pragma Machine_Attribute (af_IAdviseSink_RemoteOnRename, "ms_abi");

   type af_IAdviseSink_RemoteOnSave is access
     function (This : access IAdviseSink)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_RemoteOnSave);
   pragma Machine_Attribute (af_IAdviseSink_RemoteOnSave, "ms_abi");

   type af_IAdviseSink_RemoteOnClose is access
     function (This : access IAdviseSink)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IAdviseSink_RemoteOnClose);
   pragma Machine_Attribute (af_IAdviseSink_RemoteOnClose, "ms_abi");

   type IAdviseSinkVtbl;
   type Pointer_To_IAdviseSinkVtbl is access all IAdviseSinkVtbl;

   type IAdviseSink is
      record
         Vtbl : Pointer_To_IAdviseSinkVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IAdviseSink);

   type IAdviseSinkVtbl is
      record
         QueryInterface     : af_IAdviseSink_QueryInterface;
         AddRef             : af_IAdviseSink_AddRef;
         Release            : af_IAdviseSink_Release;
         RemoteOnDataChange : af_IAdviseSink_RemoteOnDataChange;
         RemoteOnViewChange : af_IAdviseSink_RemoteOnViewChange;
         RemoteOnRename     : af_IAdviseSink_RemoteOnRename;
         RemoteOnSave       : af_IAdviseSink_RemoteOnSave;
         RemoteOnClose      : af_IAdviseSink_RemoteOnClose;
      end record;
   pragma Convention (C_Pass_By_Copy, IAdviseSinkVtbl);

   function To_Pointer_To_IAdviseSink is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IAdviseSink);

   --  Element Name          : STATDATA
   --  Element Type          : Record

   type STATDATA is
      record
         formatetc    : GNATOCX.FORMATETC;
         advf         : Win32_Types.Unsigned_Long;
         pAdvSink     : Pointer_To_IAdviseSink;
         dwConnection : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, STATDATA);

   --  Element Name          : IEnumSTATDATA
   --  Element Type          : Interface

   IID_IEnumSTATDATA : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000105-0000-0000-C000-000000000046}");

   type af_IEnumSTATDATA_QueryInterface is access
     function (This   : access IEnumSTATDATA;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATDATA_QueryInterface);
   pragma Machine_Attribute (af_IEnumSTATDATA_QueryInterface, "ms_abi");

   type af_IEnumSTATDATA_AddRef is access
     function (This : access IEnumSTATDATA)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumSTATDATA_AddRef);
   pragma Machine_Attribute (af_IEnumSTATDATA_AddRef, "ms_abi");

   type af_IEnumSTATDATA_Release is access
     function (This : access IEnumSTATDATA)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumSTATDATA_Release);
   pragma Machine_Attribute (af_IEnumSTATDATA_Release, "ms_abi");

   type af_IEnumSTATDATA_RemoteNext is access
     function (This         : access IEnumSTATDATA;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_STATDATA;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATDATA_RemoteNext);
   pragma Machine_Attribute (af_IEnumSTATDATA_RemoteNext, "ms_abi");

   type af_IEnumSTATDATA_Skip is access
     function (This : access IEnumSTATDATA;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATDATA_Skip);
   pragma Machine_Attribute (af_IEnumSTATDATA_Skip, "ms_abi");

   type af_IEnumSTATDATA_Reset is access
     function (This : access IEnumSTATDATA)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATDATA_Reset);
   pragma Machine_Attribute (af_IEnumSTATDATA_Reset, "ms_abi");

   type af_IEnumSTATDATA_Clone is access
     function (This   : access IEnumSTATDATA;
               ppenum : Pointer_To_Pointer_To_IEnumSTATDATA)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATDATA_Clone);
   pragma Machine_Attribute (af_IEnumSTATDATA_Clone, "ms_abi");

   type IEnumSTATDATAVtbl;
   type Pointer_To_IEnumSTATDATAVtbl is access all IEnumSTATDATAVtbl;

   type IEnumSTATDATA is
      record
         Vtbl : Pointer_To_IEnumSTATDATAVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumSTATDATA);

   type IEnumSTATDATAVtbl is
      record
         QueryInterface : af_IEnumSTATDATA_QueryInterface;
         AddRef         : af_IEnumSTATDATA_AddRef;
         Release        : af_IEnumSTATDATA_Release;
         RemoteNext     : af_IEnumSTATDATA_RemoteNext;
         Skip           : af_IEnumSTATDATA_Skip;
         Reset          : af_IEnumSTATDATA_Reset;
         Clone          : af_IEnumSTATDATA_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumSTATDATAVtbl);

   function To_Pointer_To_IEnumSTATDATA is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumSTATDATA);

   --  Element Name          : IDataObject
   --  Element Type          : Interface

   IID_IDataObject : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000010E-0000-0000-C000-000000000046}");

   type af_IDataObject_QueryInterface is access
     function (This   : access IDataObject;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_QueryInterface);
   pragma Machine_Attribute (af_IDataObject_QueryInterface, "ms_abi");

   type af_IDataObject_AddRef is access
     function (This : access IDataObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IDataObject_AddRef);
   pragma Machine_Attribute (af_IDataObject_AddRef, "ms_abi");

   type af_IDataObject_Release is access
     function (This : access IDataObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IDataObject_Release);
   pragma Machine_Attribute (af_IDataObject_Release, "ms_abi");

   type af_IDataObject_RemoteGetData is access
     function (This          : access IDataObject;
               pformatetcIn  : Pointer_To_FORMATETC;
               pRemoteMedium : Pointer_To_wireSTGMEDIUM)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_RemoteGetData);
   pragma Machine_Attribute (af_IDataObject_RemoteGetData, "ms_abi");

   type af_IDataObject_RemoteGetDataHere is access
     function (This          : access IDataObject;
               pformatetc    : Pointer_To_FORMATETC;
               pRemoteMedium : Pointer_To_wireSTGMEDIUM)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_RemoteGetDataHere);
   pragma Machine_Attribute (af_IDataObject_RemoteGetDataHere, "ms_abi");

   type af_IDataObject_QueryGetData is access
     function (This       : access IDataObject;
               pformatetc : Pointer_To_FORMATETC)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_QueryGetData);
   pragma Machine_Attribute (af_IDataObject_QueryGetData, "ms_abi");

   type af_IDataObject_GetCanonicalFormatEtc is access
     function (This          : access IDataObject;
               pformatectIn  : Pointer_To_FORMATETC;
               pformatetcOut : Pointer_To_FORMATETC)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_GetCanonicalFormatEtc);
   pragma Machine_Attribute (af_IDataObject_GetCanonicalFormatEtc, "ms_abi");

   type af_IDataObject_RemoteSetData is access
     function (This       : access IDataObject;
               pformatetc : Pointer_To_FORMATETC;
               pmedium    : Pointer_To_wireFLAG_STGMEDIUM;
               fRelease   : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_RemoteSetData);
   pragma Machine_Attribute (af_IDataObject_RemoteSetData, "ms_abi");

   type af_IDataObject_EnumFormatEtc is access
     function (This            : access IDataObject;
               dwDirection     : Win32_Types.Unsigned_Long;
               ppenumFormatEtc : Pointer_To_Pointer_To_IEnumFORMATETC)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_EnumFormatEtc);
   pragma Machine_Attribute (af_IDataObject_EnumFormatEtc, "ms_abi");

   type af_IDataObject_DAdvise is access
     function (This          : access IDataObject;
               pformatetc    : Pointer_To_FORMATETC;
               advf          : Win32_Types.Unsigned_Long;
               pAdvSink      : Pointer_To_IAdviseSink;
               pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_DAdvise);
   pragma Machine_Attribute (af_IDataObject_DAdvise, "ms_abi");

   type af_IDataObject_DUnadvise is access
     function (This         : access IDataObject;
               dwConnection : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_DUnadvise);
   pragma Machine_Attribute (af_IDataObject_DUnadvise, "ms_abi");

   type af_IDataObject_EnumDAdvise is access
     function (This         : access IDataObject;
               ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IDataObject_EnumDAdvise);
   pragma Machine_Attribute (af_IDataObject_EnumDAdvise, "ms_abi");

   type IDataObjectVtbl;
   type Pointer_To_IDataObjectVtbl is access all IDataObjectVtbl;

   type IDataObject is
      record
         Vtbl : Pointer_To_IDataObjectVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IDataObject);

   type IDataObjectVtbl is
      record
         QueryInterface        : af_IDataObject_QueryInterface;
         AddRef                : af_IDataObject_AddRef;
         Release               : af_IDataObject_Release;
         RemoteGetData         : af_IDataObject_RemoteGetData;
         RemoteGetDataHere     : af_IDataObject_RemoteGetDataHere;
         QueryGetData          : af_IDataObject_QueryGetData;
         GetCanonicalFormatEtc : af_IDataObject_GetCanonicalFormatEtc;
         RemoteSetData         : af_IDataObject_RemoteSetData;
         EnumFormatEtc         : af_IDataObject_EnumFormatEtc;
         DAdvise               : af_IDataObject_DAdvise;
         DUnadvise             : af_IDataObject_DUnadvise;
         EnumDAdvise           : af_IDataObject_EnumDAdvise;
      end record;
   pragma Convention (C_Pass_By_Copy, IDataObjectVtbl);

   function To_Pointer_To_IDataObject is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IDataObject);

   Size_Of_u_MIDL_IWinTypes_0009 : constant := 32;

   --  Element Name          : __MIDL_IWinTypes_0009
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0009_Range is Positive range 1 .. 2;
   type u_MIDL_IWinTypes_0009 (Which : u_MIDL_IWinTypes_0009_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc : Interfaces.C.int;
            when 2 =>
               hRemote : Interfaces.C.int;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0009);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0009);
   for u_MIDL_IWinTypes_0009'Size use Size_Of_u_MIDL_IWinTypes_0009;

   Size_Of_uRemotableHandle : constant := 64;

   --  Element Name          : _RemotableHandle
   --  Element Type          : Record

   type uRemotableHandle is
      record
         fContext : Interfaces.C.int;
         u        : u_MIDL_IWinTypes_0009;
      end record;
   pragma Convention (C_Pass_By_Copy, uRemotableHandle);
   for uRemotableHandle use
      record
         fContext at 0 range 0 .. 31;
         u        at 4 range 0 .. Size_Of_u_MIDL_IWinTypes_0009 - 1;
      end record;
   for uRemotableHandle'Size use Size_Of_uRemotableHandle;
   for uRemotableHandle'Alignment use 4;

   Size_Of_POINT : constant := 64;

   --  Element Name          : POINT
   --  Element Type          : Record

   type POINT is
      record
         x : Interfaces.C.int;
         y : Interfaces.C.int;
      end record;
   pragma Convention (C_Pass_By_Copy, POINT);
   for POINT use
      record
         x at 0 range 0 .. 31;
         y at 4 range 0 .. 31;
      end record;
   for POINT'Size use Size_Of_POINT;
   for POINT'Alignment use 4;

   --  Element Name          : MSG
   --  Element Type          : Record

   type MSG is
      record
         hwnd    : wireHWND;
         message : Interfaces.C.unsigned;
         wParam  : Win32_Types.Unsigned_Long;
         lParam  : Win32_Types.Long;
         time    : Win32_Types.Unsigned_Long;
         pt      : POINT;
      end record;
   pragma Convention (C_Pass_By_Copy, MSG);

   Size_Of_RECT : constant := 128;

   --  Element Name          : RECT
   --  Element Type          : Record

   type RECT is
      record
         left   : Interfaces.C.int;
         top    : Interfaces.C.int;
         right  : Interfaces.C.int;
         bottom : Interfaces.C.int;
      end record;
   pragma Convention (C_Pass_By_Copy, RECT);
   for RECT use
      record
         left   at  0 range 0 .. 31;
         top    at  4 range 0 .. 31;
         right  at  8 range 0 .. 31;
         bottom at 12 range 0 .. 31;
      end record;
   for RECT'Size use Size_Of_RECT;
   for RECT'Alignment use 4;

   --  Element Name          : OLEVERB
   --  Element Type          : Record

   type OLEVERB is
      record
         lVerb        : Win32_Types.Long;
         lpszVerbName : GNATCOM.Types.LPWSTR;
         fuFlags      : Win32_Types.Unsigned_Long;
         grfAttribs   : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, OLEVERB);

   --  Element Name          : IEnumOLEVERB
   --  Element Type          : Interface

   IID_IEnumOLEVERB : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000104-0000-0000-C000-000000000046}");

   type af_IEnumOLEVERB_QueryInterface is access
     function (This   : access IEnumOLEVERB;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumOLEVERB_QueryInterface);
   pragma Machine_Attribute (af_IEnumOLEVERB_QueryInterface, "ms_abi");

   type af_IEnumOLEVERB_AddRef is access
     function (This : access IEnumOLEVERB)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumOLEVERB_AddRef);
   pragma Machine_Attribute (af_IEnumOLEVERB_AddRef, "ms_abi");

   type af_IEnumOLEVERB_Release is access
     function (This : access IEnumOLEVERB)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumOLEVERB_Release);
   pragma Machine_Attribute (af_IEnumOLEVERB_Release, "ms_abi");

   type af_IEnumOLEVERB_RemoteNext is access
     function (This         : access IEnumOLEVERB;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_OLEVERB;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumOLEVERB_RemoteNext);
   pragma Machine_Attribute (af_IEnumOLEVERB_RemoteNext, "ms_abi");

   type af_IEnumOLEVERB_Skip is access
     function (This : access IEnumOLEVERB;
               celt : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumOLEVERB_Skip);
   pragma Machine_Attribute (af_IEnumOLEVERB_Skip, "ms_abi");

   type af_IEnumOLEVERB_Reset is access
     function (This : access IEnumOLEVERB)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumOLEVERB_Reset);
   pragma Machine_Attribute (af_IEnumOLEVERB_Reset, "ms_abi");

   type af_IEnumOLEVERB_Clone is access
     function (This   : access IEnumOLEVERB;
               ppenum : Pointer_To_Pointer_To_IEnumOLEVERB)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumOLEVERB_Clone);
   pragma Machine_Attribute (af_IEnumOLEVERB_Clone, "ms_abi");

   type IEnumOLEVERBVtbl;
   type Pointer_To_IEnumOLEVERBVtbl is access all IEnumOLEVERBVtbl;

   type IEnumOLEVERB is
      record
         Vtbl : Pointer_To_IEnumOLEVERBVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumOLEVERB);

   type IEnumOLEVERBVtbl is
      record
         QueryInterface : af_IEnumOLEVERB_QueryInterface;
         AddRef         : af_IEnumOLEVERB_AddRef;
         Release        : af_IEnumOLEVERB_Release;
         RemoteNext     : af_IEnumOLEVERB_RemoteNext;
         Skip           : af_IEnumOLEVERB_Skip;
         Reset          : af_IEnumOLEVERB_Reset;
         Clone          : af_IEnumOLEVERB_Clone;
      end record;
   pragma Convention (C_Pass_By_Copy, IEnumOLEVERBVtbl);

   function To_Pointer_To_IEnumOLEVERB is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IEnumOLEVERB);

   Size_Of_SIZEL : constant := 64;

   --  Element Name          : SIZEL
   --  Element Type          : Record

   type SIZEL is
      record
         cx : Interfaces.C.int;
         cy : Interfaces.C.int;
      end record;
   pragma Convention (C_Pass_By_Copy, SIZEL);
   for SIZEL use
      record
         cx at 0 range 0 .. 31;
         cy at 0 range 32 .. 63;
      end record;
   for SIZEL'Size use Size_Of_SIZEL;
   for SIZEL'Alignment use 4;

   --  Element Name          : IOleObject
   --  Element Type          : Interface

   IID_IOleObject : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000112-0000-0000-C000-000000000046}");

   type af_IOleObject_QueryInterface is access
     function (This   : access IOleObject;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_QueryInterface);
   pragma Machine_Attribute (af_IOleObject_QueryInterface, "ms_abi");

   type af_IOleObject_AddRef is access
     function (This : access IOleObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleObject_AddRef);
   pragma Machine_Attribute (af_IOleObject_AddRef, "ms_abi");

   type af_IOleObject_Release is access
     function (This : access IOleObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleObject_Release);
   pragma Machine_Attribute (af_IOleObject_Release, "ms_abi");

   type af_IOleObject_SetClientSite is access
     function (This        : access IOleObject;
               pClientSite : Pointer_To_IOleClientSite)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_SetClientSite);
   pragma Machine_Attribute (af_IOleObject_SetClientSite, "ms_abi");

   type af_IOleObject_GetClientSite is access
     function (This         : access IOleObject;
               ppClientSite : Pointer_To_Pointer_To_IOleClientSite)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetClientSite);
   pragma Machine_Attribute (af_IOleObject_GetClientSite, "ms_abi");

   type af_IOleObject_SetHostNames is access
     function (This           : access IOleObject;
               szContainerApp : GNATCOM.Types.LPWSTR;
               szContainerObj : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_SetHostNames);
   pragma Machine_Attribute (af_IOleObject_SetHostNames, "ms_abi");

   type af_IOleObject_Close is access
     function (This         : access IOleObject;
               dwSaveOption : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_Close);
   pragma Machine_Attribute (af_IOleObject_Close, "ms_abi");

   type af_IOleObject_SetMoniker is access
     function (This           : access IOleObject;
               dwWhichMoniker : Win32_Types.Unsigned_Long;
               pmk            : Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_SetMoniker);
   pragma Machine_Attribute (af_IOleObject_SetMoniker, "ms_abi");

   type af_IOleObject_GetMoniker is access
     function (This           : access IOleObject;
               dwAssign       : Win32_Types.Unsigned_Long;
               dwWhichMoniker : Win32_Types.Unsigned_Long;
               ppmk           : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetMoniker);
   pragma Machine_Attribute (af_IOleObject_GetMoniker, "ms_abi");

   type af_IOleObject_InitFromData is access
     function (This        : access IOleObject;
               pDataObject : Pointer_To_IDataObject;
               fCreation   : Win32_Types.Long;
               dwReserved  : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_InitFromData);
   pragma Machine_Attribute (af_IOleObject_InitFromData, "ms_abi");

   type af_IOleObject_GetClipboardData is access
     function (This         : access IOleObject;
               dwReserved   : Win32_Types.Unsigned_Long;
               ppDataObject : Pointer_To_Pointer_To_IDataObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetClipboardData);
   pragma Machine_Attribute (af_IOleObject_GetClipboardData, "ms_abi");

   type af_IOleObject_DoVerb is access
     function (This        : access IOleObject;
               iVerb       : Win32_Types.Long;
               lpmsg       : Pointer_To_MSG;
               pActiveSite : Pointer_To_IOleClientSite;
               lindex      : Win32_Types.Long;
               hwndParent  : wireHWND;
               lprcPosRect : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_DoVerb);
   pragma Machine_Attribute (af_IOleObject_DoVerb, "ms_abi");

   type af_IOleObject_EnumVerbs is access
     function (This          : access IOleObject;
               ppEnumOleVerb : Pointer_To_Pointer_To_IEnumOLEVERB)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_EnumVerbs);
   pragma Machine_Attribute (af_IOleObject_EnumVerbs, "ms_abi");

   type af_IOleObject_Update is access
     function (This : access IOleObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_Update);
   pragma Machine_Attribute (af_IOleObject_Update, "ms_abi");

   type af_IOleObject_IsUpToDate is access
     function (This : access IOleObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_IsUpToDate);
   pragma Machine_Attribute (af_IOleObject_IsUpToDate, "ms_abi");

   type af_IOleObject_GetUserClassID is access
     function (This   : access IOleObject;
               pClsid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetUserClassID);
   pragma Machine_Attribute (af_IOleObject_GetUserClassID, "ms_abi");

   type af_IOleObject_GetUserType is access
     function (This         : access IOleObject;
               dwFormOfType : Win32_Types.Unsigned_Long;
               pszUserType  : GNATCOM.Types.Pointer_To_LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetUserType);
   pragma Machine_Attribute (af_IOleObject_GetUserType, "ms_abi");

   type af_IOleObject_SetExtent is access
     function (This         : access IOleObject;
               dwDrawAspect : Win32_Types.Unsigned_Long;
               psizel       : Pointer_To_SIZEL)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_SetExtent);
   pragma Machine_Attribute (af_IOleObject_SetExtent, "ms_abi");

   type af_IOleObject_GetExtent is access
     function (This         : access IOleObject;
               dwDrawAspect : Win32_Types.Unsigned_Long;
               psizel       : Pointer_To_SIZEL)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetExtent);
   pragma Machine_Attribute (af_IOleObject_GetExtent, "ms_abi");

   type af_IOleObject_Advise is access
     function (This          : access IOleObject;
               pAdvSink      : Pointer_To_IAdviseSink;
               pdwConnection : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_Advise);
   pragma Machine_Attribute (af_IOleObject_Advise, "ms_abi");

   type af_IOleObject_Unadvise is access
     function (This         : access IOleObject;
               dwConnection : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_Unadvise);
   pragma Machine_Attribute (af_IOleObject_Unadvise, "ms_abi");

   type af_IOleObject_EnumAdvise is access
     function (This         : access IOleObject;
               ppenumAdvise : Pointer_To_Pointer_To_IEnumSTATDATA)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_EnumAdvise);
   pragma Machine_Attribute (af_IOleObject_EnumAdvise, "ms_abi");

   type af_IOleObject_GetMiscStatus is access
     function (This      : access IOleObject;
               dwAspect  : Win32_Types.Unsigned_Long;
               pdwStatus : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_GetMiscStatus);
   pragma Machine_Attribute (af_IOleObject_GetMiscStatus, "ms_abi");

   type af_IOleObject_SetColorScheme is access
     function (This    : access IOleObject;
               pLogpal : Pointer_To_LOGPALETTE)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleObject_SetColorScheme);
   pragma Machine_Attribute (af_IOleObject_SetColorScheme, "ms_abi");

   type IOleObjectVtbl;
   type Pointer_To_IOleObjectVtbl is access all IOleObjectVtbl;

   type IOleObject is
      record
         Vtbl : Pointer_To_IOleObjectVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleObject);

   type IOleObjectVtbl is
      record
         QueryInterface   : af_IOleObject_QueryInterface;
         AddRef           : af_IOleObject_AddRef;
         Release          : af_IOleObject_Release;
         SetClientSite    : af_IOleObject_SetClientSite;
         GetClientSite    : af_IOleObject_GetClientSite;
         SetHostNames     : af_IOleObject_SetHostNames;
         Close            : af_IOleObject_Close;
         SetMoniker       : af_IOleObject_SetMoniker;
         GetMoniker       : af_IOleObject_GetMoniker;
         InitFromData     : af_IOleObject_InitFromData;
         GetClipboardData : af_IOleObject_GetClipboardData;
         DoVerb           : af_IOleObject_DoVerb;
         EnumVerbs        : af_IOleObject_EnumVerbs;
         Update           : af_IOleObject_Update;
         IsUpToDate       : af_IOleObject_IsUpToDate;
         GetUserClassID   : af_IOleObject_GetUserClassID;
         GetUserType      : af_IOleObject_GetUserType;
         SetExtent        : af_IOleObject_SetExtent;
         GetExtent        : af_IOleObject_GetExtent;
         Advise           : af_IOleObject_Advise;
         Unadvise         : af_IOleObject_Unadvise;
         EnumAdvise       : af_IOleObject_EnumAdvise;
         GetMiscStatus    : af_IOleObject_GetMiscStatus;
         SetColorScheme   : af_IOleObject_SetColorScheme;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleObjectVtbl);

   function To_Pointer_To_IOleObject is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleObject);

   --  Element Name          : IPersistStream
   --  Element Type          : Interface

   IID_IPersistStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000109-0000-0000-C000-000000000046}");

   type af_IPersistStream_QueryInterface is access
     function (This   : access IPersistStream;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_QueryInterface);
   pragma Machine_Attribute (af_IPersistStream_QueryInterface, "ms_abi");

   type af_IPersistStream_AddRef is access
     function (This : access IPersistStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersistStream_AddRef);
   pragma Machine_Attribute (af_IPersistStream_AddRef, "ms_abi");

   type af_IPersistStream_Release is access
     function (This : access IPersistStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersistStream_Release);
   pragma Machine_Attribute (af_IPersistStream_Release, "ms_abi");

   type af_IPersistStream_GetClassID is access
     function (This     : access IPersistStream;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_GetClassID);
   pragma Machine_Attribute (af_IPersistStream_GetClassID, "ms_abi");

   type af_IPersistStream_IsDirty is access
     function (This : access IPersistStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_IsDirty);
   pragma Machine_Attribute (af_IPersistStream_IsDirty, "ms_abi");

   type af_IPersistStream_Load is access
     function (This : access IPersistStream;
               pstm : Pointer_To_IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_Load);
   pragma Machine_Attribute (af_IPersistStream_Load, "ms_abi");

   type af_IPersistStream_Save is access
     function (This        : access IPersistStream;
               pstm        : Pointer_To_IStream;
               fClearDirty : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_Save);
   pragma Machine_Attribute (af_IPersistStream_Save, "ms_abi");

   type af_IPersistStream_GetSizeMax is access
     function (This    : access IPersistStream;
               pcbSize : Pointer_To_uULARGE_INTEGER)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_GetSizeMax);
   pragma Machine_Attribute (af_IPersistStream_GetSizeMax, "ms_abi");

   type IPersistStreamVtbl;
   type Pointer_To_IPersistStreamVtbl is access all IPersistStreamVtbl;

   type IPersistStream is
      record
         Vtbl : Pointer_To_IPersistStreamVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IPersistStream);

   type IPersistStreamVtbl is
      record
         QueryInterface : af_IPersistStream_QueryInterface;
         AddRef         : af_IPersistStream_AddRef;
         Release        : af_IPersistStream_Release;
         GetClassID     : af_IPersistStream_GetClassID;
         IsDirty        : af_IPersistStream_IsDirty;
         Load           : af_IPersistStream_Load;
         Save           : af_IPersistStream_Save;
         GetSizeMax     : af_IPersistStream_GetSizeMax;
      end record;
   pragma Convention (C_Pass_By_Copy, IPersistStreamVtbl);

   function To_Pointer_To_IPersistStream is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IPersistStream);

   --  Element Name          : IPersist
   --  Element Type          : Interface

   IID_IPersist : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000010C-0000-0000-C000-000000000046}");

   type af_IPersist_QueryInterface is access
     function (This   : access IPersist;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersist_QueryInterface);
   pragma Machine_Attribute (af_IPersist_QueryInterface, "ms_abi");

   type af_IPersist_AddRef is access
     function (This : access IPersist)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersist_AddRef);
   pragma Machine_Attribute (af_IPersist_AddRef, "ms_abi");

   type af_IPersist_Release is access
     function (This : access IPersist)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersist_Release);
   pragma Machine_Attribute (af_IPersist_Release, "ms_abi");

   type af_IPersist_GetClassID is access
     function (This     : access IPersist;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersist_GetClassID);
   pragma Machine_Attribute (af_IPersist_GetClassID, "ms_abi");

   type IPersistVtbl;
   type Pointer_To_IPersistVtbl is access all IPersistVtbl;

   type IPersist is
      record
         Vtbl : Pointer_To_IPersistVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IPersist);

   type IPersistVtbl is
      record
         QueryInterface : af_IPersist_QueryInterface;
         AddRef         : af_IPersist_AddRef;
         Release        : af_IPersist_Release;
         GetClassID     : af_IPersist_GetClassID;
      end record;
   pragma Convention (C_Pass_By_Copy, IPersistVtbl);

   function To_Pointer_To_IPersist is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IPersist);

   --  Element Name          : ISequentialStream
   --  Element Type          : Interface

   IID_ISequentialStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0C733A30-2A1C-11CE-ADE5-00AA0044773D}");

   type af_ISequentialStream_QueryInterface is access
     function (This   : access ISequentialStream;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_QueryInterface);
   pragma Machine_Attribute (af_ISequentialStream_QueryInterface, "ms_abi");

   type af_ISequentialStream_AddRef is access
     function (This : access ISequentialStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ISequentialStream_AddRef);
   pragma Machine_Attribute (af_ISequentialStream_AddRef, "ms_abi");

   type af_ISequentialStream_Release is access
     function (This : access ISequentialStream)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ISequentialStream_Release);
   pragma Machine_Attribute (af_ISequentialStream_Release, "ms_abi");

   type af_ISequentialStream_RemoteRead is access
     function (This    : access ISequentialStream;
               pv      : Pointer_To_unsigned_char;
               cb      : Win32_Types.Unsigned_Long;
               pcbRead : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_RemoteRead);
   pragma Machine_Attribute (af_ISequentialStream_RemoteRead, "ms_abi");

   type af_ISequentialStream_RemoteWrite is access
     function (This       : access ISequentialStream;
               pv         : Pointer_To_unsigned_char;
               cb         : Win32_Types.Unsigned_Long;
               pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_RemoteWrite);
   pragma Machine_Attribute (af_ISequentialStream_RemoteWrite, "ms_abi");

   type ISequentialStreamVtbl;
   type Pointer_To_ISequentialStreamVtbl is access all ISequentialStreamVtbl;

   type ISequentialStream is
      record
         Vtbl : Pointer_To_ISequentialStreamVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ISequentialStream);

   type ISequentialStreamVtbl is
      record
         QueryInterface : af_ISequentialStream_QueryInterface;
         AddRef         : af_ISequentialStream_AddRef;
         Release        : af_ISequentialStream_Release;
         RemoteRead     : af_ISequentialStream_RemoteRead;
         RemoteWrite    : af_ISequentialStream_RemoteWrite;
      end record;
   pragma Convention (C_Pass_By_Copy, ISequentialStreamVtbl);

   function To_Pointer_To_ISequentialStream is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_ISequentialStream);

   --  Element Name          : IParseDisplayName
   --  Element Type          : Interface

   IID_IParseDisplayName : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000011A-0000-0000-C000-000000000046}");

   type af_IParseDisplayName_QueryInterface is access
     function (This   : access IParseDisplayName;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IParseDisplayName_QueryInterface);
   pragma Machine_Attribute (af_IParseDisplayName_QueryInterface, "ms_abi");

   type af_IParseDisplayName_AddRef is access
     function (This : access IParseDisplayName)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IParseDisplayName_AddRef);
   pragma Machine_Attribute (af_IParseDisplayName_AddRef, "ms_abi");

   type af_IParseDisplayName_Release is access
     function (This : access IParseDisplayName)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IParseDisplayName_Release);
   pragma Machine_Attribute (af_IParseDisplayName_Release, "ms_abi");

   type af_IParseDisplayName_ParseDisplayName is access
     function (This           : access IParseDisplayName;
               pbc            : Pointer_To_IBindCtx;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IParseDisplayName_ParseDisplayName);
   pragma Machine_Attribute (af_IParseDisplayName_ParseDisplayName, "ms_abi");

   type IParseDisplayNameVtbl;
   type Pointer_To_IParseDisplayNameVtbl is access all IParseDisplayNameVtbl;

   type IParseDisplayName is
      record
         Vtbl : Pointer_To_IParseDisplayNameVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IParseDisplayName);

   type IParseDisplayNameVtbl is
      record
         QueryInterface   : af_IParseDisplayName_QueryInterface;
         AddRef           : af_IParseDisplayName_AddRef;
         Release          : af_IParseDisplayName_Release;
         ParseDisplayName : af_IParseDisplayName_ParseDisplayName;
      end record;
   pragma Convention (C_Pass_By_Copy, IParseDisplayNameVtbl);

   function To_Pointer_To_IParseDisplayName is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IParseDisplayName);

   --  Element Name          : IOleWindow
   --  Element Type          : Interface

   IID_IOleWindow : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000114-0000-0000-C000-000000000046}");

   type af_IOleWindow_QueryInterface is access
     function (This   : access IOleWindow;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_QueryInterface);
   pragma Machine_Attribute (af_IOleWindow_QueryInterface, "ms_abi");

   type af_IOleWindow_AddRef is access
     function (This : access IOleWindow)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleWindow_AddRef);
   pragma Machine_Attribute (af_IOleWindow_AddRef, "ms_abi");

   type af_IOleWindow_Release is access
     function (This : access IOleWindow)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleWindow_Release);
   pragma Machine_Attribute (af_IOleWindow_Release, "ms_abi");

   type af_IOleWindow_GetWindow is access
     function (This  : access IOleWindow;
               phwnd : Pointer_To_wireHWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_GetWindow);
   pragma Machine_Attribute (af_IOleWindow_GetWindow, "ms_abi");

   type af_IOleWindow_ContextSensitiveHelp is access
     function (This       : access IOleWindow;
               fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleWindow_ContextSensitiveHelp, "ms_abi");

   type IOleWindowVtbl;
   type Pointer_To_IOleWindowVtbl is access all IOleWindowVtbl;

   type IOleWindow is
      record
         Vtbl : Pointer_To_IOleWindowVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleWindow);

   type IOleWindowVtbl is
      record
         QueryInterface       : af_IOleWindow_QueryInterface;
         AddRef               : af_IOleWindow_AddRef;
         Release              : af_IOleWindow_Release;
         GetWindow            : af_IOleWindow_GetWindow;
         ContextSensitiveHelp : af_IOleWindow_ContextSensitiveHelp;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleWindowVtbl);

   function To_Pointer_To_IOleWindow is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleWindow);

   --  Element Name          : IOleInPlaceObject
   --  Element Type          : Interface

   IID_IOleInPlaceObject : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000113-0000-0000-C000-000000000046}");

   type af_IOleInPlaceObject_QueryInterface is access
     function (This   : access IOleInPlaceObject;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_QueryInterface);
   pragma Machine_Attribute (af_IOleInPlaceObject_QueryInterface, "ms_abi");

   type af_IOleInPlaceObject_AddRef is access
     function (This : access IOleInPlaceObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceObject_AddRef);
   pragma Machine_Attribute (af_IOleInPlaceObject_AddRef, "ms_abi");

   type af_IOleInPlaceObject_Release is access
     function (This : access IOleInPlaceObject)
     return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceObject_Release);
   pragma Machine_Attribute (af_IOleInPlaceObject_Release, "ms_abi");

   type af_IOleInPlaceObject_GetWindow is access
     function (This  : access IOleInPlaceObject;
               phwnd : Pointer_To_wireHWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_GetWindow);
   pragma Machine_Attribute (af_IOleInPlaceObject_GetWindow, "ms_abi");

   type af_IOleInPlaceObject_ContextSensitiveHelp is access
     function (This       : access IOleInPlaceObject;
               fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleInPlaceObject_ContextSensitiveHelp, "ms_abi");

   type af_IOleInPlaceObject_InPlaceDeactivate is access
     function (This : access IOleInPlaceObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_InPlaceDeactivate);
   pragma Machine_Attribute (af_IOleInPlaceObject_InPlaceDeactivate, "ms_abi");

   type af_IOleInPlaceObject_UIDeactivate is access
     function (This : access IOleInPlaceObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_UIDeactivate);
   pragma Machine_Attribute (af_IOleInPlaceObject_UIDeactivate, "ms_abi");

   type af_IOleInPlaceObject_SetObjectRects is access
     function (This         : access IOleInPlaceObject;
               lprcPosRect  : Pointer_To_RECT;
               lprcClipRect : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_SetObjectRects);
   pragma Machine_Attribute (af_IOleInPlaceObject_SetObjectRects, "ms_abi");

   type af_IOleInPlaceObject_ReactivateAndUndo is access
     function (This : access IOleInPlaceObject)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceObject_ReactivateAndUndo);
   pragma Machine_Attribute (af_IOleInPlaceObject_ReactivateAndUndo, "ms_abi");

   type IOleInPlaceObjectVtbl;
   type Pointer_To_IOleInPlaceObjectVtbl is access all IOleInPlaceObjectVtbl;

   type IOleInPlaceObject is
      record
         Vtbl : Pointer_To_IOleInPlaceObjectVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleInPlaceObject);

   type IOleInPlaceObjectVtbl is
      record
         QueryInterface       : af_IOleInPlaceObject_QueryInterface;
         AddRef               : af_IOleInPlaceObject_AddRef;
         Release              : af_IOleInPlaceObject_Release;
         GetWindow            : af_IOleInPlaceObject_GetWindow;
         ContextSensitiveHelp : af_IOleInPlaceObject_ContextSensitiveHelp;
         InPlaceDeactivate    : af_IOleInPlaceObject_InPlaceDeactivate;
         UIDeactivate         : af_IOleInPlaceObject_UIDeactivate;
         SetObjectRects       : af_IOleInPlaceObject_SetObjectRects;
         ReactivateAndUndo    : af_IOleInPlaceObject_ReactivateAndUndo;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleInPlaceObjectVtbl);

   function To_Pointer_To_IOleInPlaceObject is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleInPlaceObject);

end GNATOCX;
