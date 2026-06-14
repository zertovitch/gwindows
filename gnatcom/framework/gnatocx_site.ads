------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                       G N A T O C X _ S I T E                            --
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

with Interfaces.C;

with GNATCOM.Create.COM_Interface;
with GNATCOM.Types;
with GNATCOM.GUID;
with System;
with Win32_Types;

package GNATOCX_Site is

   subtype IStorage is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IStream is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   type uLARGE_INTEGER;
   type uULARGE_INTEGER;
   type STATSTG;
   type uFILETIME;
   type SNB;
   type RemSNB;
   subtype IEnumSTATSTG is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype ISequentialStream is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IOleWindow is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype HWND is System.Address;
   type uRemotableHandle;
   type u_MIDL_IWinTypes_0009;
   subtype IOleInPlaceUIWindow is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   type RECT;
   subtype IOleInPlaceActiveObject is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IOleInPlaceFrame is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   type HMENU;
   type OleMenuGroupWidths;
   type HGLOBAL;
   type uuserHGLOBAL;
   type u_MIDL_IWinTypes_0003;
   type uFLAGGED_BYTE_BLOB;
   type MSG;
   type UINT_PTR;
   type LONG_PTR;
   type POINT;
   subtype IOleClientSite is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IMoniker is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IBindCtx is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   type BIND_OPTS2;
   type uCOSERVERINFO;
   type uCOAUTHINFO;
   type uCOAUTHIDENTITY;
   subtype IRunningObjectTable is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IEnumMoniker is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IEnumString is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IOleContainer is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IEnumUnknown is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IPersistStream is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IPersist is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IParseDisplayName is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   subtype IOleInPlaceSite is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;
   type OIFI;
   subtype HACCEL is Win32_Types.Long;
   type SIZE;
   subtype IGNATOCX is
     GNATCOM.Create.COM_Interface.COM_Interface_Type;

   type Pointer_To_IStorage is access all IStorage;
   type Pointer_To_IStream is access all IStream;
   type Pointer_To_unsigned_char is access all Interfaces.C.unsigned_char;
   type Pointer_To_uULARGE_INTEGER is access all uULARGE_INTEGER;
   type Pointer_To_STATSTG is access all STATSTG;
   type Pointer_To_Pointer_To_IStream is access all Pointer_To_IStream;
   type Pointer_To_Pointer_To_IStorage is access all Pointer_To_IStorage;
   type Pointer_To_RemSNB is access all RemSNB;
   type Pointer_To_IEnumSTATSTG is access all IEnumSTATSTG;
   type Pointer_To_Pointer_To_IEnumSTATSTG is
     access all Pointer_To_IEnumSTATSTG;
   type Pointer_To_uFILETIME is access all uFILETIME;
   type Pointer_To_ISequentialStream is access all ISequentialStream;
   type Pointer_To_IOleWindow is access all IOleWindow;
   type Pointer_To_uRemotableHandle is access all uRemotableHandle;
   type Pointer_To_HWND is access all HWND;
   type Pointer_To_IOleInPlaceUIWindow is access all IOleInPlaceUIWindow;
   type Pointer_To_RECT is access all RECT;
   type Pointer_To_IOleInPlaceActiveObject is
     access all IOleInPlaceActiveObject;
   type Pointer_To_IOleInPlaceFrame is access all IOleInPlaceFrame;
   pragma No_Strict_Aliasing (Pointer_To_IOleInPlaceFrame);
   type Pointer_To_OleMenuGroupWidths is access all OleMenuGroupWidths;
   type Pointer_To_uFLAGGED_BYTE_BLOB is access all uFLAGGED_BYTE_BLOB;
   type Pointer_To_uuserHGLOBAL is access all uuserHGLOBAL;
   type Pointer_To_MSG is access all MSG;
   type Pointer_To_IOleClientSite is access all IOleClientSite;
   type Pointer_To_IMoniker is access all IMoniker;
   type Pointer_To_IBindCtx is access all IBindCtx;
   type Pointer_To_uCOAUTHIDENTITY is access all uCOAUTHIDENTITY;
   type Pointer_To_uCOAUTHINFO is access all uCOAUTHINFO;
   type Pointer_To_uCOSERVERINFO is access all uCOSERVERINFO;
   type Pointer_To_BIND_OPTS2 is access all BIND_OPTS2;
   type Pointer_To_IRunningObjectTable is access all IRunningObjectTable;
   type Pointer_To_Pointer_To_IUnknown is
     access all GNATCOM.Types.Pointer_To_IUnknown;
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
   type Pointer_To_IPersistStream is access all IPersistStream;
   type Pointer_To_IPersist is access all IPersist;
   type Pointer_To_IParseDisplayName is access all IParseDisplayName;
   type Pointer_To_IOleInPlaceSite is access all IOleInPlaceSite;
   type Pointer_To_Pointer_To_IOleInPlaceFrame is
     access all Pointer_To_IOleInPlaceFrame;
   type Pointer_To_Pointer_To_IOleInPlaceUIWindow is
     access all Pointer_To_IOleInPlaceUIWindow;
   type Pointer_To_OIFI is access all OIFI;
   type Pointer_To_IGNATOCX is access all IGNATOCX;

   type SNB is
     new Win32_Types.Long;

   type HMENU is
     new Pointer_To_uRemotableHandle;

   type HGLOBAL is
     new Pointer_To_uuserHGLOBAL;

   type UINT_PTR is
     new Win32_Types.Unsigned_Long;

   type LONG_PTR is
     new Win32_Types.Long;

   LIBID_GNATOCXLib : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{352CDDDB-DADB-45DC-AEA8-3F726CC666A1}");

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
         dwLowDateTime  : Win32_Types.Unsigned_Long;
         dwHighDateTime : Win32_Types.Unsigned_Long;
      end record;
   pragma Convention (C_Pass_By_Copy, uFILETIME);
   for uFILETIME use
      record
         dwLowDateTime  at 0 range 0 .. 0 +
           Win32_Types.Unsigned_Long'Size - 1;
         dwHighDateTime at 0 range 32 .. 32 +
           Win32_Types.Unsigned_Long'Size - 1;
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

   --  Element Name          : IStream
   --  Element Type          : Interface

   IID_IStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000C-0000-0000-C000-000000000046}");

   type af_IStream_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_QueryInterface);
   pragma Machine_Attribute (af_IStream_QueryInterface, "ms_abi");

   type af_IStream_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStream_AddRef);
   pragma Machine_Attribute (af_IStream_AddRef, "ms_abi");

   type af_IStream_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStream_Release);
   pragma Machine_Attribute (af_IStream_Release, "ms_abi");

   type af_IStream_RemoteRead is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pv      : Pointer_To_unsigned_char;
               cb      : Win32_Types.Unsigned_Long;
               pcbRead : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteRead);
   pragma Machine_Attribute (af_IStream_RemoteRead, "ms_abi");

   type af_IStream_RemoteWrite is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pv         : Pointer_To_unsigned_char;
               cb         : Win32_Types.Unsigned_Long;
               pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteWrite);
   pragma Machine_Attribute (af_IStream_RemoteWrite, "ms_abi");

   type af_IStream_RemoteSeek is access
     function (This            : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dlibMove        : uLARGE_INTEGER;
               dwOrigin        : Win32_Types.Unsigned_Long;
               plibNewPosition : Pointer_To_uULARGE_INTEGER)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteSeek);
   pragma Machine_Attribute (af_IStream_RemoteSeek, "ms_abi");

   type af_IStream_SetSize is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               libNewSize : uULARGE_INTEGER)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_SetSize);
   pragma Machine_Attribute (af_IStream_SetSize, "ms_abi");

   type af_IStream_RemoteCopyTo is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstm       : Pointer_To_IStream;
               cb         : uULARGE_INTEGER;
               pcbRead    : Pointer_To_uULARGE_INTEGER;
               pcbWritten : Pointer_To_uULARGE_INTEGER)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_RemoteCopyTo);
   pragma Machine_Attribute (af_IStream_RemoteCopyTo, "ms_abi");

   type af_IStream_Commit is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               grfCommitFlags : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Commit);
   pragma Machine_Attribute (af_IStream_Commit, "ms_abi");

   type af_IStream_Revert is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Revert);
   pragma Machine_Attribute (af_IStream_Revert, "ms_abi");

   type af_IStream_LockRegion is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               libOffset  : uULARGE_INTEGER;
               cb         : uULARGE_INTEGER;
               dwLockType : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_LockRegion);
   pragma Machine_Attribute (af_IStream_LockRegion, "ms_abi");

   type af_IStream_UnlockRegion is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               libOffset  : uULARGE_INTEGER;
               cb         : uULARGE_INTEGER;
               dwLockType : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_UnlockRegion);
   pragma Machine_Attribute (af_IStream_UnlockRegion, "ms_abi");

   type af_IStream_Stat is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstatstg    : Pointer_To_STATSTG;
               grfStatFlag : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Stat);
   pragma Machine_Attribute (af_IStream_Stat, "ms_abi");

   type af_IStream_Clone is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppstm : Pointer_To_Pointer_To_IStream)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStream_Clone);
   pragma Machine_Attribute (af_IStream_Clone, "ms_abi");

   --  Element Name          : RemSNB
   --  Element Type          : Record

   type RemSNB is
      record
         ulCntStr  : Win32_Types.Unsigned_Long;
         ulCntChar : Win32_Types.Unsigned_Long;
         rgString  : GNATCOM.Types.Pointer_To_unsigned_short;
      end record;
   pragma Convention (C_Pass_By_Copy, RemSNB);

   --  Element Name          : IEnumSTATSTG
   --  Element Type          : Interface

   IID_IEnumSTATSTG : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000D-0000-0000-C000-000000000046}");

   type af_IEnumSTATSTG_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATSTG_QueryInterface);
   pragma Machine_Attribute (af_IEnumSTATSTG_QueryInterface, "ms_abi");

   type af_IEnumSTATSTG_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumSTATSTG_AddRef);
   pragma Machine_Attribute (af_IEnumSTATSTG_AddRef, "ms_abi");

   type af_IEnumSTATSTG_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumSTATSTG_Release);
   pragma Machine_Attribute (af_IEnumSTATSTG_Release, "ms_abi");

   type af_IEnumSTATSTG_RemoteNext is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_STATSTG;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATSTG_RemoteNext);
   pragma Machine_Attribute (af_IEnumSTATSTG_RemoteNext, "ms_abi");

   type af_IEnumSTATSTG_Skip is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATSTG_Skip);
   pragma Machine_Attribute (af_IEnumSTATSTG_Skip, "ms_abi");

   type af_IEnumSTATSTG_Reset is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATSTG_Reset);
   pragma Machine_Attribute (af_IEnumSTATSTG_Reset, "ms_abi");

   type af_IEnumSTATSTG_Clone is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenum : Pointer_To_Pointer_To_IEnumSTATSTG)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumSTATSTG_Clone);
   pragma Machine_Attribute (af_IEnumSTATSTG_Clone, "ms_abi");

   --  Element Name          : IStorage
   --  Element Type          : Interface

   IID_IStorage : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000B-0000-0000-C000-000000000046}");

   type af_IStorage_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_QueryInterface);
   pragma Machine_Attribute (af_IStorage_QueryInterface, "ms_abi");

   type af_IStorage_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStorage_AddRef);
   pragma Machine_Attribute (af_IStorage_AddRef, "ms_abi");

   type af_IStorage_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IStorage_Release);
   pragma Machine_Attribute (af_IStorage_Release, "ms_abi");

   type af_IStorage_CreateStream is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName  : GNATCOM.Types.LPWSTR;
               grfMode   : Win32_Types.Unsigned_Long;
               reserved1 : Win32_Types.Unsigned_Long;
               reserved2 : Win32_Types.Unsigned_Long;
               ppstm     : Pointer_To_Pointer_To_IStream)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_CreateStream);
   pragma Machine_Attribute (af_IStorage_CreateStream, "ms_abi");

   type af_IStorage_RemoteOpenStream is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName    : GNATCOM.Types.LPWSTR;
               cbReserved1 : Win32_Types.Unsigned_Long;
               reserved1   : Pointer_To_unsigned_char;
               grfMode     : Win32_Types.Unsigned_Long;
               reserved2   : Win32_Types.Unsigned_Long;
               ppstm       : Pointer_To_Pointer_To_IStream)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_RemoteOpenStream);
   pragma Machine_Attribute (af_IStorage_RemoteOpenStream, "ms_abi");

   type af_IStorage_CreateStorage is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName  : GNATCOM.Types.LPWSTR;
               grfMode   : Win32_Types.Unsigned_Long;
               reserved1 : Win32_Types.Unsigned_Long;
               reserved2 : Win32_Types.Unsigned_Long;
               ppstg     : Pointer_To_Pointer_To_IStorage)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_CreateStorage);
   pragma Machine_Attribute (af_IStorage_CreateStorage, "ms_abi");

   type af_IStorage_OpenStorage is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName     : GNATCOM.Types.LPWSTR;
               pstgPriority : Pointer_To_IStorage;
               grfMode      : Win32_Types.Unsigned_Long;
               snbExclude   : SNB;
               reserved     : Win32_Types.Unsigned_Long;
               ppstg        : Pointer_To_Pointer_To_IStorage)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_OpenStorage);
   pragma Machine_Attribute (af_IStorage_OpenStorage, "ms_abi");

   type af_IStorage_CopyTo is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ciidExclude  : Win32_Types.Unsigned_Long;
               rgiidExclude : GNATCOM.Types.Pointer_To_GUID;
               snbExclude   : SNB;
               pstgDest     : Pointer_To_IStorage)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_CopyTo);
   pragma Machine_Attribute (af_IStorage_CopyTo, "ms_abi");

   type af_IStorage_MoveElementTo is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName    : GNATCOM.Types.LPWSTR;
               pstgDest    : Pointer_To_IStorage;
               pwcsNewName : GNATCOM.Types.LPWSTR;
               grfFlags    : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_MoveElementTo);
   pragma Machine_Attribute (af_IStorage_MoveElementTo, "ms_abi");

   type af_IStorage_Commit is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               grfCommitFlags : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_Commit);
   pragma Machine_Attribute (af_IStorage_Commit, "ms_abi");

   type af_IStorage_Revert is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_Revert);
   pragma Machine_Attribute (af_IStorage_Revert, "ms_abi");

   type af_IStorage_RemoteEnumElements is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               reserved1   : Win32_Types.Unsigned_Long;
               cbReserved2 : Win32_Types.Unsigned_Long;
               reserved2   : Pointer_To_unsigned_char;
               reserved3   : Win32_Types.Unsigned_Long;
               ppenum      : Pointer_To_Pointer_To_IEnumSTATSTG)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_RemoteEnumElements);
   pragma Machine_Attribute (af_IStorage_RemoteEnumElements, "ms_abi");

   type af_IStorage_DestroyElement is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_DestroyElement);
   pragma Machine_Attribute (af_IStorage_DestroyElement, "ms_abi");

   type af_IStorage_RenameElement is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsOldName : GNATCOM.Types.LPWSTR;
               pwcsNewName : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_RenameElement);
   pragma Machine_Attribute (af_IStorage_RenameElement, "ms_abi");

   type af_IStorage_SetElementTimes is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pwcsName : GNATCOM.Types.LPWSTR;
               pctime   : Pointer_To_uFILETIME;
               patime   : Pointer_To_uFILETIME;
               pmtime   : Pointer_To_uFILETIME)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_SetElementTimes);
   pragma Machine_Attribute (af_IStorage_SetElementTimes, "ms_abi");

   type af_IStorage_SetClass is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               clsid : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_SetClass);
   pragma Machine_Attribute (af_IStorage_SetClass, "ms_abi");

   type af_IStorage_SetStateBits is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               grfStateBits : Win32_Types.Unsigned_Long;
               grfMask      : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_SetStateBits);
   pragma Machine_Attribute (af_IStorage_SetStateBits, "ms_abi");

   type af_IStorage_Stat is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstatstg    : Pointer_To_STATSTG;
               grfStatFlag : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IStorage_Stat);
   pragma Machine_Attribute (af_IStorage_Stat, "ms_abi");

   --  Element Name          : ISequentialStream
   --  Element Type          : Interface

   IID_ISequentialStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0C733A30-2A1C-11CE-ADE5-00AA0044773D}");

   type af_ISequentialStream_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_QueryInterface);
   pragma Machine_Attribute (af_ISequentialStream_QueryInterface, "ms_abi");

   type af_ISequentialStream_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ISequentialStream_AddRef);
   pragma Machine_Attribute (af_ISequentialStream_AddRef, "ms_abi");

   type af_ISequentialStream_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_ISequentialStream_Release);
   pragma Machine_Attribute (af_ISequentialStream_Release, "ms_abi");

   type af_ISequentialStream_RemoteRead is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pv      : Pointer_To_unsigned_char;
               cb      : Win32_Types.Unsigned_Long;
               pcbRead : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_RemoteRead);
   pragma Machine_Attribute (af_ISequentialStream_RemoteRead, "ms_abi");

   type af_ISequentialStream_RemoteWrite is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pv         : Pointer_To_unsigned_char;
               cb         : Win32_Types.Unsigned_Long;
               pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISequentialStream_RemoteWrite);
   pragma Machine_Attribute (af_ISequentialStream_RemoteWrite, "ms_abi");

   Size_Of_u_MIDL_IWinTypes_0009 : constant := 32;

   --  Element Name          : __MIDL_IWinTypes_0009
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0009_Range is Positive range 1 .. 2;
   type u_MIDL_IWinTypes_0009 (Which : u_MIDL_IWinTypes_0009_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc : Win32_Types.Long;
            when 2 =>
               hRemote : Win32_Types.Long;
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
         fContext : Win32_Types.Long;
         u        : u_MIDL_IWinTypes_0009;
      end record;
   pragma Convention (C_Pass_By_Copy, uRemotableHandle);
   for uRemotableHandle use
      record
         fContext at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         u        at 0 range 32 .. 32 + Size_Of_u_MIDL_IWinTypes_0009 - 1;
      end record;
   for uRemotableHandle'Size use Size_Of_uRemotableHandle;
   for uRemotableHandle'Alignment use 4;

   --  Element Name          : IOleWindow
   --  Element Type          : Interface

   IID_IOleWindow : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000114-0000-0000-C000-000000000046}");

   type af_IOleWindow_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_QueryInterface);
   pragma Machine_Attribute (af_IOleWindow_QueryInterface, "ms_abi");

   type af_IOleWindow_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleWindow_AddRef);
   pragma Machine_Attribute (af_IOleWindow_AddRef, "ms_abi");

   type af_IOleWindow_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleWindow_Release);
   pragma Machine_Attribute (af_IOleWindow_Release, "ms_abi");

   type af_IOleWindow_GetWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               phwnd : Pointer_To_HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_GetWindow);
   pragma Machine_Attribute (af_IOleWindow_GetWindow, "ms_abi");

   type af_IOleWindow_ContextSensitiveHelp is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnterMode : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleWindow_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleWindow_ContextSensitiveHelp, "ms_abi");

   Size_Of_RECT : constant := 128;

   --  Element Name          : RECT
   --  Element Type          : Record

   type RECT is
      record
         left   : Win32_Types.Long;
         top    : Win32_Types.Long;
         right  : Win32_Types.Long;
         bottom : Win32_Types.Long;
      end record;
   pragma Convention (C, RECT);
   for RECT use
      record
         left   at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         top    at 0 range 32 .. 32 + Win32_Types.Long'Size - 1;
         right  at 0 range 64 .. 64 + Win32_Types.Long'Size - 1;
         bottom at 0 range 96 .. 96 + Win32_Types.Long'Size - 1;
      end record;
   for RECT'Size use Size_Of_RECT;
   for RECT'Alignment use 4;

   --  Element Name          : IOleInPlaceActiveObject
   --  Element Type          : Interface

   IID_IOleInPlaceActiveObject : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000117-0000-0000-C000-000000000046}");

   type af_IOleInPlaceActiveObject_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceActiveObject_QueryInterface);
   pragma Machine_Attribute (af_IOleInPlaceActiveObject_QueryInterface, "ms_abi");

   type af_IOleInPlaceActiveObject_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceActiveObject_AddRef);
   pragma Machine_Attribute (af_IOleInPlaceActiveObject_AddRef, "ms_abi");

   type af_IOleInPlaceActiveObject_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceActiveObject_Release);
   pragma Machine_Attribute (af_IOleInPlaceActiveObject_Release, "ms_abi");

   type af_IOleInPlaceActiveObject_GetWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               phwnd : Pointer_To_HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceActiveObject_GetWindow);
   pragma Machine_Attribute (af_IOleInPlaceActiveObject_GetWindow, "ms_abi");

   type af_IOleInPlaceActiveObject_ContextSensitiveHelp is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnterMode : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention
     (StdCall, af_IOleInPlaceActiveObject_ContextSensitiveHelp);

   type af_IOleInPlaceActiveObject_RemoteTranslateAccelerator is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention
     (StdCall, af_IOleInPlaceActiveObject_RemoteTranslateAccelerator);

   type af_IOleInPlaceActiveObject_OnFrameWindowActivate is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fActivate : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention
     (StdCall, af_IOleInPlaceActiveObject_OnFrameWindowActivate);

   type af_IOleInPlaceActiveObject_OnDocWindowActivate is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fActivate : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention
     (StdCall, af_IOleInPlaceActiveObject_OnDocWindowActivate);

   type af_IOleInPlaceActiveObject_RemoteResizeBorder is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               prcBorder    : Pointer_To_RECT;
               riid         : GNATCOM.Types.Pointer_To_GUID;
               pUIWindow    : Pointer_To_IOleInPlaceUIWindow;
               fFrameWindow : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention
     (StdCall, af_IOleInPlaceActiveObject_RemoteResizeBorder);

   type af_IOleInPlaceActiveObject_EnableModeless is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnable : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceActiveObject_EnableModeless);
   pragma Machine_Attribute (af_IOleInPlaceActiveObject_EnableModeless, "ms_abi");

   --  Element Name          : IOleInPlaceUIWindow
   --  Element Type          : Interface

   IID_IOleInPlaceUIWindow : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000115-0000-0000-C000-000000000046}");

   type af_IOleInPlaceUIWindow_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_QueryInterface);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_QueryInterface, "ms_abi");

   type af_IOleInPlaceUIWindow_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_AddRef);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_AddRef, "ms_abi");

   type af_IOleInPlaceUIWindow_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_Release);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_Release, "ms_abi");

   type af_IOleInPlaceUIWindow_GetWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               phwnd : Pointer_To_HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_GetWindow);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_GetWindow, "ms_abi");

   type af_IOleInPlaceUIWindow_ContextSensitiveHelp is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnterMode : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_ContextSensitiveHelp, "ms_abi");

   type af_IOleInPlaceUIWindow_GetBorder is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               lprectBorder : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_GetBorder);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_GetBorder, "ms_abi");

   type af_IOleInPlaceUIWindow_RequestBorderSpace is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pborderwidths : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_RequestBorderSpace);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_RequestBorderSpace, "ms_abi");

   type af_IOleInPlaceUIWindow_SetBorderSpace is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pborderwidths : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_SetBorderSpace);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_SetBorderSpace, "ms_abi");

   type af_IOleInPlaceUIWindow_SetActiveObject is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pActiveObject : Pointer_To_IOleInPlaceActiveObject;
               pszObjName    : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceUIWindow_SetActiveObject);
   pragma Machine_Attribute (af_IOleInPlaceUIWindow_SetActiveObject, "ms_abi");

   type long_OleMenuGroupWidths_width_Array is
     array (Integer range 0 .. 5) of Win32_Types.Long;
   Size_Of_long_OleMenuGroupWidths_width_Array : constant :=
     Win32_Types.Long'Size * 6;

   Size_Of_OleMenuGroupWidths : constant := 192;

   --  Element Name          : OleMenuGroupWidths
   --  Element Type          : Record

   type OleMenuGroupWidths is
      record
         width : long_OleMenuGroupWidths_width_Array;
      end record;
   pragma Convention (C_Pass_By_Copy, OleMenuGroupWidths);
   for OleMenuGroupWidths use
      record
         width at 0 range 0 .. 0 +
           Size_Of_long_OleMenuGroupWidths_width_Array - 1;
      end record;
   for OleMenuGroupWidths'Size use Size_Of_OleMenuGroupWidths;
   for OleMenuGroupWidths'Alignment use 4;

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

   Size_Of_POINT : constant := 64;

   --  Element Name          : POINT
   --  Element Type          : Record

   type POINT is
      record
         x : Win32_Types.Long;
         y : Win32_Types.Long;
      end record;
   pragma Convention (C_Pass_By_Copy, POINT);
   for POINT use
      record
         x at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         y at 0 range 32 .. 32 + Win32_Types.Long'Size - 1;
      end record;
   for POINT'Size use Size_Of_POINT;
   for POINT'Alignment use 4;

   --  Element Name          : MSG
   --  Element Type          : Record

   type MSG is
      record
         hwnd    : GNATOCX_Site.HWND;
         message : Interfaces.C.unsigned;
         wParam  : UINT_PTR;
         lParam  : LONG_PTR;
         time    : Win32_Types.Unsigned_Long;
         pt      : POINT;
      end record;
   pragma Convention (C_Pass_By_Copy, MSG);

   --  Element Name          : IOleInPlaceFrame
   --  Element Type          : Interface

   IID_IOleInPlaceFrame : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000116-0000-0000-C000-000000000046}");

   type af_IOleInPlaceFrame_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_QueryInterface);
   pragma Machine_Attribute (af_IOleInPlaceFrame_QueryInterface, "ms_abi");

   type af_IOleInPlaceFrame_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceFrame_AddRef);
   pragma Machine_Attribute (af_IOleInPlaceFrame_AddRef, "ms_abi");

   type af_IOleInPlaceFrame_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceFrame_Release);
   pragma Machine_Attribute (af_IOleInPlaceFrame_Release, "ms_abi");

   type af_IOleInPlaceFrame_GetWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               phwnd : Pointer_To_HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_GetWindow);
   pragma Machine_Attribute (af_IOleInPlaceFrame_GetWindow, "ms_abi");

   type af_IOleInPlaceFrame_ContextSensitiveHelp is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnterMode : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleInPlaceFrame_ContextSensitiveHelp, "ms_abi");

   type af_IOleInPlaceFrame_GetBorder is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               lprectBorder : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_GetBorder);
   pragma Machine_Attribute (af_IOleInPlaceFrame_GetBorder, "ms_abi");

   type af_IOleInPlaceFrame_RequestBorderSpace is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pborderwidths : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_RequestBorderSpace);
   pragma Machine_Attribute (af_IOleInPlaceFrame_RequestBorderSpace, "ms_abi");

   type af_IOleInPlaceFrame_SetBorderSpace is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pborderwidths : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_SetBorderSpace);
   pragma Machine_Attribute (af_IOleInPlaceFrame_SetBorderSpace, "ms_abi");

   type af_IOleInPlaceFrame_SetActiveObject is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pActiveObject : Pointer_To_IOleInPlaceActiveObject;
               pszObjName    : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_SetActiveObject);
   pragma Machine_Attribute (af_IOleInPlaceFrame_SetActiveObject, "ms_abi");

   type af_IOleInPlaceFrame_InsertMenus is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               hmenuShared  : HMENU;
               lpMenuWidths : Pointer_To_OleMenuGroupWidths)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_InsertMenus);
   pragma Machine_Attribute (af_IOleInPlaceFrame_InsertMenus, "ms_abi");

   type af_IOleInPlaceFrame_SetMenu is access
     function (This             : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               hmenuShared      : HMENU;
               holemenu         : HGLOBAL;
               hwndActiveObject : HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_SetMenu);
   pragma Machine_Attribute (af_IOleInPlaceFrame_SetMenu, "ms_abi");

   type af_IOleInPlaceFrame_RemoveMenus is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               hmenuShared : HMENU)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_RemoveMenus);
   pragma Machine_Attribute (af_IOleInPlaceFrame_RemoveMenus, "ms_abi");

   type af_IOleInPlaceFrame_SetStatusText is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pszStatusText : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_SetStatusText);
   pragma Machine_Attribute (af_IOleInPlaceFrame_SetStatusText, "ms_abi");

   type af_IOleInPlaceFrame_EnableModeless is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnable : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_EnableModeless);
   pragma Machine_Attribute (af_IOleInPlaceFrame_EnableModeless, "ms_abi");

   type af_IOleInPlaceFrame_TranslateAccelerator is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               lpmsg : Pointer_To_MSG;
               wID   : Interfaces.C.unsigned_short)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceFrame_TranslateAccelerator);
   pragma Machine_Attribute (af_IOleInPlaceFrame_TranslateAccelerator, "ms_abi");

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
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_QueryInterface);
   pragma Machine_Attribute (af_IEnumMoniker_QueryInterface, "ms_abi");

   type af_IEnumMoniker_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumMoniker_AddRef);
   pragma Machine_Attribute (af_IEnumMoniker_AddRef, "ms_abi");

   type af_IEnumMoniker_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumMoniker_Release);
   pragma Machine_Attribute (af_IEnumMoniker_Release, "ms_abi");

   type af_IEnumMoniker_RemoteNext is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_Pointer_To_IMoniker;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_RemoteNext);
   pragma Machine_Attribute (af_IEnumMoniker_RemoteNext, "ms_abi");

   type af_IEnumMoniker_Skip is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Skip);
   pragma Machine_Attribute (af_IEnumMoniker_Skip, "ms_abi");

   type af_IEnumMoniker_Reset is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Reset);
   pragma Machine_Attribute (af_IEnumMoniker_Reset, "ms_abi");

   type af_IEnumMoniker_Clone is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenum : Pointer_To_Pointer_To_IEnumMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumMoniker_Clone);
   pragma Machine_Attribute (af_IEnumMoniker_Clone, "ms_abi");

   --  Element Name          : IRunningObjectTable
   --  Element Type          : Interface

   IID_IRunningObjectTable : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000010-0000-0000-C000-000000000046}");

   type af_IRunningObjectTable_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_QueryInterface);
   pragma Machine_Attribute (af_IRunningObjectTable_QueryInterface, "ms_abi");

   type af_IRunningObjectTable_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IRunningObjectTable_AddRef);
   pragma Machine_Attribute (af_IRunningObjectTable_AddRef, "ms_abi");

   type af_IRunningObjectTable_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IRunningObjectTable_Release);
   pragma Machine_Attribute (af_IRunningObjectTable_Release, "ms_abi");

   type af_IRunningObjectTable_Register is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               grfFlags      : Win32_Types.Unsigned_Long;
               punkObject    : GNATCOM.Types.Pointer_To_IUnknown;
               pmkObjectName : Pointer_To_IMoniker;
               pdwRegister   : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_Register);
   pragma Machine_Attribute (af_IRunningObjectTable_Register, "ms_abi");

   type af_IRunningObjectTable_Revoke is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dwRegister : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_Revoke);
   pragma Machine_Attribute (af_IRunningObjectTable_Revoke, "ms_abi");

   type af_IRunningObjectTable_IsRunning is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkObjectName : Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_IsRunning);
   pragma Machine_Attribute (af_IRunningObjectTable_IsRunning, "ms_abi");

   type af_IRunningObjectTable_GetObject is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkObjectName : Pointer_To_IMoniker;
               ppunkObject   : Pointer_To_Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_GetObject);
   pragma Machine_Attribute (af_IRunningObjectTable_GetObject, "ms_abi");

   type af_IRunningObjectTable_NoteChangeTime is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dwRegister : Win32_Types.Unsigned_Long;
               pfiletime  : Pointer_To_uFILETIME)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_NoteChangeTime);
   pragma Machine_Attribute (af_IRunningObjectTable_NoteChangeTime, "ms_abi");

   type af_IRunningObjectTable_GetTimeOfLastChange is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkObjectName : Pointer_To_IMoniker;
               pfiletime     : Pointer_To_uFILETIME)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_GetTimeOfLastChange);
   pragma Machine_Attribute (af_IRunningObjectTable_GetTimeOfLastChange, "ms_abi");

   type af_IRunningObjectTable_EnumRunning is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IRunningObjectTable_EnumRunning);
   pragma Machine_Attribute (af_IRunningObjectTable_EnumRunning, "ms_abi");

   --  Element Name          : IEnumString
   --  Element Type          : Interface

   IID_IEnumString : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000101-0000-0000-C000-000000000046}");

   type af_IEnumString_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_QueryInterface);
   pragma Machine_Attribute (af_IEnumString_QueryInterface, "ms_abi");

   type af_IEnumString_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumString_AddRef);
   pragma Machine_Attribute (af_IEnumString_AddRef, "ms_abi");

   type af_IEnumString_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumString_Release);
   pragma Machine_Attribute (af_IEnumString_Release, "ms_abi");

   type af_IEnumString_RemoteNext is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : GNATCOM.Types.Pointer_To_LPWSTR;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_RemoteNext);
   pragma Machine_Attribute (af_IEnumString_RemoteNext, "ms_abi");

   type af_IEnumString_Skip is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Skip);
   pragma Machine_Attribute (af_IEnumString_Skip, "ms_abi");

   type af_IEnumString_Reset is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Reset);
   pragma Machine_Attribute (af_IEnumString_Reset, "ms_abi");

   type af_IEnumString_Clone is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenum : Pointer_To_Pointer_To_IEnumString)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumString_Clone);
   pragma Machine_Attribute (af_IEnumString_Clone, "ms_abi");

   --  Element Name          : IBindCtx
   --  Element Type          : Interface

   IID_IBindCtx : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000E-0000-0000-C000-000000000046}");

   type af_IBindCtx_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_QueryInterface);
   pragma Machine_Attribute (af_IBindCtx_QueryInterface, "ms_abi");

   type af_IBindCtx_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IBindCtx_AddRef);
   pragma Machine_Attribute (af_IBindCtx_AddRef, "ms_abi");

   type af_IBindCtx_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IBindCtx_Release);
   pragma Machine_Attribute (af_IBindCtx_Release, "ms_abi");

   type af_IBindCtx_RegisterObjectBound is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               punk : GNATCOM.Types.Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RegisterObjectBound);
   pragma Machine_Attribute (af_IBindCtx_RegisterObjectBound, "ms_abi");

   type af_IBindCtx_RevokeObjectBound is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               punk : GNATCOM.Types.Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RevokeObjectBound);
   pragma Machine_Attribute (af_IBindCtx_RevokeObjectBound, "ms_abi");

   type af_IBindCtx_ReleaseBoundObjects is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_ReleaseBoundObjects);
   pragma Machine_Attribute (af_IBindCtx_ReleaseBoundObjects, "ms_abi");

   type af_IBindCtx_RemoteSetBindOptions is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbindopts : Pointer_To_BIND_OPTS2)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RemoteSetBindOptions);
   pragma Machine_Attribute (af_IBindCtx_RemoteSetBindOptions, "ms_abi");

   type af_IBindCtx_RemoteGetBindOptions is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbindopts : Pointer_To_BIND_OPTS2)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RemoteGetBindOptions);
   pragma Machine_Attribute (af_IBindCtx_RemoteGetBindOptions, "ms_abi");

   type af_IBindCtx_GetRunningObjectTable is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pprot : Pointer_To_Pointer_To_IRunningObjectTable)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_GetRunningObjectTable);
   pragma Machine_Attribute (af_IBindCtx_GetRunningObjectTable, "ms_abi");

   type af_IBindCtx_RegisterObjectParam is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pszKey : GNATCOM.Types.LPWSTR;
               punk   : GNATCOM.Types.Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RegisterObjectParam);
   pragma Machine_Attribute (af_IBindCtx_RegisterObjectParam, "ms_abi");

   type af_IBindCtx_GetObjectParam is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pszKey : GNATCOM.Types.LPWSTR;
               ppunk  : Pointer_To_Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_GetObjectParam);
   pragma Machine_Attribute (af_IBindCtx_GetObjectParam, "ms_abi");

   type af_IBindCtx_EnumObjectParam is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenum : Pointer_To_Pointer_To_IEnumString)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_EnumObjectParam);
   pragma Machine_Attribute (af_IBindCtx_EnumObjectParam, "ms_abi");

   type af_IBindCtx_RevokeObjectParam is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pszKey : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IBindCtx_RevokeObjectParam);
   pragma Machine_Attribute (af_IBindCtx_RevokeObjectParam, "ms_abi");

   --  Element Name          : IMoniker
   --  Element Type          : Interface

   IID_IMoniker : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000000F-0000-0000-C000-000000000046}");

   type af_IMoniker_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_QueryInterface);
   pragma Machine_Attribute (af_IMoniker_QueryInterface, "ms_abi");

   type af_IMoniker_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IMoniker_AddRef);
   pragma Machine_Attribute (af_IMoniker_AddRef, "ms_abi");

   type af_IMoniker_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IMoniker_Release);
   pragma Machine_Attribute (af_IMoniker_Release, "ms_abi");

   type af_IMoniker_GetClassID is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetClassID);
   pragma Machine_Attribute (af_IMoniker_GetClassID, "ms_abi");

   type af_IMoniker_IsDirty is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsDirty);
   pragma Machine_Attribute (af_IMoniker_IsDirty, "ms_abi");

   type af_IMoniker_Load is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstm : Pointer_To_IStream)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Load);
   pragma Machine_Attribute (af_IMoniker_Load, "ms_abi");

   type af_IMoniker_Save is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstm        : Pointer_To_IStream;
               fClearDirty : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Save);
   pragma Machine_Attribute (af_IMoniker_Save, "ms_abi");

   type af_IMoniker_GetSizeMax is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pcbSize : Pointer_To_uULARGE_INTEGER)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetSizeMax);
   pragma Machine_Attribute (af_IMoniker_GetSizeMax, "ms_abi");

   type af_IMoniker_RemoteBindToObject is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc        : Pointer_To_IBindCtx;
               pmkToLeft  : Pointer_To_IMoniker;
               riidResult : GNATCOM.Types.Pointer_To_GUID;
               ppvResult  : Pointer_To_Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RemoteBindToObject);
   pragma Machine_Attribute (af_IMoniker_RemoteBindToObject, "ms_abi");

   type af_IMoniker_RemoteBindToStorage is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc       : Pointer_To_IBindCtx;
               pmkToLeft : Pointer_To_IMoniker;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObj    : Pointer_To_Pointer_To_IUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RemoteBindToStorage);
   pragma Machine_Attribute (af_IMoniker_RemoteBindToStorage, "ms_abi");

   type af_IMoniker_Reduce is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc            : Pointer_To_IBindCtx;
               dwReduceHowFar : Win32_Types.Unsigned_Long;
               ppmkToLeft     : Pointer_To_Pointer_To_IMoniker;
               ppmkReduced    : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Reduce);
   pragma Machine_Attribute (af_IMoniker_Reduce, "ms_abi");

   type af_IMoniker_ComposeWith is access
     function (This              : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkRight          : Pointer_To_IMoniker;
               fOnlyIfNotGeneric : Win32_Types.Long;
               ppmkComposite     : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_ComposeWith);
   pragma Machine_Attribute (af_IMoniker_ComposeWith, "ms_abi");

   type af_IMoniker_Enum is access
     function (This          : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fForward      : Win32_Types.Long;
               ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Enum);
   pragma Machine_Attribute (af_IMoniker_Enum, "ms_abi");

   type af_IMoniker_IsEqual is access
     function (This            : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkOtherMoniker : Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsEqual);
   pragma Machine_Attribute (af_IMoniker_IsEqual, "ms_abi");

   type af_IMoniker_Hash is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pdwHash : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Hash);
   pragma Machine_Attribute (af_IMoniker_Hash, "ms_abi");

   type af_IMoniker_IsRunning is access
     function (This            : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc             : Pointer_To_IBindCtx;
               pmkToLeft       : Pointer_To_IMoniker;
               pmkNewlyRunning : Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsRunning);
   pragma Machine_Attribute (af_IMoniker_IsRunning, "ms_abi");

   type af_IMoniker_GetTimeOfLastChange is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc       : Pointer_To_IBindCtx;
               pmkToLeft : Pointer_To_IMoniker;
               pfiletime : Pointer_To_uFILETIME)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetTimeOfLastChange);
   pragma Machine_Attribute (af_IMoniker_GetTimeOfLastChange, "ms_abi");

   type af_IMoniker_Inverse is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppmk : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_Inverse);
   pragma Machine_Attribute (af_IMoniker_Inverse, "ms_abi");

   type af_IMoniker_CommonPrefixWith is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkOther   : Pointer_To_IMoniker;
               ppmkPrefix : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_CommonPrefixWith);
   pragma Machine_Attribute (af_IMoniker_CommonPrefixWith, "ms_abi");

   type af_IMoniker_RelativePathTo is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pmkOther    : Pointer_To_IMoniker;
               ppmkRelPath : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_RelativePathTo);
   pragma Machine_Attribute (af_IMoniker_RelativePathTo, "ms_abi");

   type af_IMoniker_GetDisplayName is access
     function (This            : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc             : Pointer_To_IBindCtx;
               pmkToLeft       : Pointer_To_IMoniker;
               ppszDisplayName : GNATCOM.Types.Pointer_To_LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_GetDisplayName);
   pragma Machine_Attribute (af_IMoniker_GetDisplayName, "ms_abi");

   type af_IMoniker_ParseDisplayName is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc            : Pointer_To_IBindCtx;
               pmkToLeft      : Pointer_To_IMoniker;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_ParseDisplayName);
   pragma Machine_Attribute (af_IMoniker_ParseDisplayName, "ms_abi");

   type af_IMoniker_IsSystemMoniker is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pdwMksys : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IMoniker_IsSystemMoniker);
   pragma Machine_Attribute (af_IMoniker_IsSystemMoniker, "ms_abi");

   --  Element Name          : IEnumUnknown
   --  Element Type          : Interface

   IID_IEnumUnknown : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000100-0000-0000-C000-000000000046}");

   type af_IEnumUnknown_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_QueryInterface);
   pragma Machine_Attribute (af_IEnumUnknown_QueryInterface, "ms_abi");

   type af_IEnumUnknown_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumUnknown_AddRef);
   pragma Machine_Attribute (af_IEnumUnknown_AddRef, "ms_abi");

   type af_IEnumUnknown_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IEnumUnknown_Release);
   pragma Machine_Attribute (af_IEnumUnknown_Release, "ms_abi");

   type af_IEnumUnknown_RemoteNext is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt         : Win32_Types.Unsigned_Long;
               rgelt        : Pointer_To_Pointer_To_IUnknown;
               pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_RemoteNext);
   pragma Machine_Attribute (af_IEnumUnknown_RemoteNext, "ms_abi");

   type af_IEnumUnknown_Skip is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt : Win32_Types.Unsigned_Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Skip);
   pragma Machine_Attribute (af_IEnumUnknown_Skip, "ms_abi");

   type af_IEnumUnknown_Reset is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Reset);
   pragma Machine_Attribute (af_IEnumUnknown_Reset, "ms_abi");

   type af_IEnumUnknown_Clone is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppenum : Pointer_To_Pointer_To_IEnumUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IEnumUnknown_Clone);
   pragma Machine_Attribute (af_IEnumUnknown_Clone, "ms_abi");

   --  Element Name          : IOleContainer
   --  Element Type          : Interface

   IID_IOleContainer : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000011B-0000-0000-C000-000000000046}");

   type af_IOleContainer_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_QueryInterface);
   pragma Machine_Attribute (af_IOleContainer_QueryInterface, "ms_abi");

   type af_IOleContainer_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleContainer_AddRef);
   pragma Machine_Attribute (af_IOleContainer_AddRef, "ms_abi");

   type af_IOleContainer_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleContainer_Release);
   pragma Machine_Attribute (af_IOleContainer_Release, "ms_abi");

   type af_IOleContainer_ParseDisplayName is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc            : Pointer_To_IBindCtx;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_ParseDisplayName);
   pragma Machine_Attribute (af_IOleContainer_ParseDisplayName, "ms_abi");

   type af_IOleContainer_EnumObjects is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               grfFlags : Win32_Types.Unsigned_Long;
               ppenum   : Pointer_To_Pointer_To_IEnumUnknown)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_EnumObjects);
   pragma Machine_Attribute (af_IOleContainer_EnumObjects, "ms_abi");

   type af_IOleContainer_LockContainer is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fLock : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleContainer_LockContainer);
   pragma Machine_Attribute (af_IOleContainer_LockContainer, "ms_abi");

   --  Element Name          : IOleClientSite
   --  Element Type          : Interface

   IID_IOleClientSite : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000118-0000-0000-C000-000000000046}");

   type af_IOleClientSite_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_QueryInterface);
   pragma Machine_Attribute (af_IOleClientSite_QueryInterface, "ms_abi");

   type af_IOleClientSite_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleClientSite_AddRef);
   pragma Machine_Attribute (af_IOleClientSite_AddRef, "ms_abi");

   type af_IOleClientSite_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleClientSite_Release);
   pragma Machine_Attribute (af_IOleClientSite_Release, "ms_abi");

   type af_IOleClientSite_SaveObject is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_SaveObject);
   pragma Machine_Attribute (af_IOleClientSite_SaveObject, "ms_abi");

   type af_IOleClientSite_GetMoniker is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dwAssign       : Win32_Types.Unsigned_Long;
               dwWhichMoniker : Win32_Types.Unsigned_Long;
               ppmk           : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_GetMoniker);
   pragma Machine_Attribute (af_IOleClientSite_GetMoniker, "ms_abi");

   type af_IOleClientSite_GetContainer is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppContainer : Pointer_To_Pointer_To_IOleContainer)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_GetContainer);
   pragma Machine_Attribute (af_IOleClientSite_GetContainer, "ms_abi");

   type af_IOleClientSite_ShowObject is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_ShowObject);
   pragma Machine_Attribute (af_IOleClientSite_ShowObject, "ms_abi");

   type af_IOleClientSite_OnShowWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fShow : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_OnShowWindow);
   pragma Machine_Attribute (af_IOleClientSite_OnShowWindow, "ms_abi");

   type af_IOleClientSite_RequestNewObjectLayout is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleClientSite_RequestNewObjectLayout);
   pragma Machine_Attribute (af_IOleClientSite_RequestNewObjectLayout, "ms_abi");

   --  Element Name          : IPersistStream
   --  Element Type          : Interface

   IID_IPersistStream : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000109-0000-0000-C000-000000000046}");

   type af_IPersistStream_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_QueryInterface);
   pragma Machine_Attribute (af_IPersistStream_QueryInterface, "ms_abi");

   type af_IPersistStream_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersistStream_AddRef);
   pragma Machine_Attribute (af_IPersistStream_AddRef, "ms_abi");

   type af_IPersistStream_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersistStream_Release);
   pragma Machine_Attribute (af_IPersistStream_Release, "ms_abi");

   type af_IPersistStream_GetClassID is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_GetClassID);
   pragma Machine_Attribute (af_IPersistStream_GetClassID, "ms_abi");

   type af_IPersistStream_IsDirty is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_IsDirty);
   pragma Machine_Attribute (af_IPersistStream_IsDirty, "ms_abi");

   type af_IPersistStream_Load is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstm : Pointer_To_IStream)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_Load);
   pragma Machine_Attribute (af_IPersistStream_Load, "ms_abi");

   type af_IPersistStream_Save is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pstm        : Pointer_To_IStream;
               fClearDirty : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_Save);
   pragma Machine_Attribute (af_IPersistStream_Save, "ms_abi");

   type af_IPersistStream_GetSizeMax is access
     function (This    : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pcbSize : Pointer_To_uULARGE_INTEGER)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersistStream_GetSizeMax);
   pragma Machine_Attribute (af_IPersistStream_GetSizeMax, "ms_abi");

   --  Element Name          : IPersist
   --  Element Type          : Interface

   IID_IPersist : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000010C-0000-0000-C000-000000000046}");

   type af_IPersist_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersist_QueryInterface);
   pragma Machine_Attribute (af_IPersist_QueryInterface, "ms_abi");

   type af_IPersist_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersist_AddRef);
   pragma Machine_Attribute (af_IPersist_AddRef, "ms_abi");

   type af_IPersist_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IPersist_Release);
   pragma Machine_Attribute (af_IPersist_Release, "ms_abi");

   type af_IPersist_GetClassID is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pClassID : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IPersist_GetClassID);
   pragma Machine_Attribute (af_IPersist_GetClassID, "ms_abi");

   --  Element Name          : IParseDisplayName
   --  Element Type          : Interface

   IID_IParseDisplayName : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{0000011A-0000-0000-C000-000000000046}");

   type af_IParseDisplayName_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IParseDisplayName_QueryInterface);
   pragma Machine_Attribute (af_IParseDisplayName_QueryInterface, "ms_abi");

   type af_IParseDisplayName_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IParseDisplayName_AddRef);
   pragma Machine_Attribute (af_IParseDisplayName_AddRef, "ms_abi");

   type af_IParseDisplayName_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IParseDisplayName_Release);
   pragma Machine_Attribute (af_IParseDisplayName_Release, "ms_abi");

   type af_IParseDisplayName_ParseDisplayName is access
     function (This           : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               pbc            : Pointer_To_IBindCtx;
               pszDisplayName : GNATCOM.Types.LPWSTR;
               pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
               ppmkOut        : Pointer_To_Pointer_To_IMoniker)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IParseDisplayName_ParseDisplayName);
   pragma Machine_Attribute (af_IParseDisplayName_ParseDisplayName, "ms_abi");

   --  Element Name          : OIFI
   --  Element Type          : Record

   type OIFI is
      record
         cb            : Interfaces.C.unsigned;
         fMDIApp       : Win32_Types.Long;
         hwndFrame     : HWND;
         haccel        : GNATOCX_Site.HACCEL;
         cAccelEntries : Interfaces.C.unsigned;
      end record;
   pragma Convention (C_Pass_By_Copy, OIFI);

   Size_Of_SIZE : constant := 64;

   --  Element Name          : SIZE
   --  Element Type          : Record

   type SIZE is
      record
         cx : Win32_Types.Long;
         cy : Win32_Types.Long;
      end record;
   pragma Convention (C_Pass_By_Copy, SIZE);
   for SIZE use
      record
         cx at 0 range 0 .. 0 + Win32_Types.Long'Size - 1;
         cy at 0 range 32 .. 32 + Win32_Types.Long'Size - 1;
      end record;
   for SIZE'Size use Size_Of_SIZE;
   for SIZE'Alignment use 4;

   --  Element Name          : IOleInPlaceSite
   --  Element Type          : Interface

   IID_IOleInPlaceSite : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000119-0000-0000-C000-000000000046}");

   type af_IOleInPlaceSite_QueryInterface is access
     function (This   : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_QueryInterface);
   pragma Machine_Attribute (af_IOleInPlaceSite_QueryInterface, "ms_abi");

   type af_IOleInPlaceSite_AddRef is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceSite_AddRef);
   pragma Machine_Attribute (af_IOleInPlaceSite_AddRef, "ms_abi");

   type af_IOleInPlaceSite_Release is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return Win32_Types.Unsigned_Long;
   pragma Convention (StdCall, af_IOleInPlaceSite_Release);
   pragma Machine_Attribute (af_IOleInPlaceSite_Release, "ms_abi");

   type af_IOleInPlaceSite_GetWindow is access
     function (This  : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               phwnd : Pointer_To_HWND)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_GetWindow);
   pragma Machine_Attribute (af_IOleInPlaceSite_GetWindow, "ms_abi");

   type af_IOleInPlaceSite_ContextSensitiveHelp is access
     function (This       : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fEnterMode : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_ContextSensitiveHelp);
   pragma Machine_Attribute (af_IOleInPlaceSite_ContextSensitiveHelp, "ms_abi");

   type af_IOleInPlaceSite_CanInPlaceActivate is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_CanInPlaceActivate);
   pragma Machine_Attribute (af_IOleInPlaceSite_CanInPlaceActivate, "ms_abi");

   type af_IOleInPlaceSite_OnInPlaceActivate is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_OnInPlaceActivate);
   pragma Machine_Attribute (af_IOleInPlaceSite_OnInPlaceActivate, "ms_abi");

   type af_IOleInPlaceSite_OnUIActivate is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_OnUIActivate);
   pragma Machine_Attribute (af_IOleInPlaceSite_OnUIActivate, "ms_abi");

   type af_IOleInPlaceSite_GetWindowContext is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppFrame      : Pointer_To_Pointer_To_IOleInPlaceFrame;
               ppDoc        : Pointer_To_Pointer_To_IOleInPlaceUIWindow;
               lprcPosRect  : Pointer_To_RECT;
               lprcClipRect : Pointer_To_RECT;
               lpFrameInfo  : Pointer_To_OIFI)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_GetWindowContext);
   pragma Machine_Attribute (af_IOleInPlaceSite_GetWindowContext, "ms_abi");

   type af_IOleInPlaceSite_Scroll is access
     function (This         : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               scrollExtant : SIZE)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_Scroll);
   pragma Machine_Attribute (af_IOleInPlaceSite_Scroll, "ms_abi");

   type af_IOleInPlaceSite_OnUIDeactivate is access
     function (This      : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               fUndoable : Win32_Types.Long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_OnUIDeactivate);
   pragma Machine_Attribute (af_IOleInPlaceSite_OnUIDeactivate, "ms_abi");

   type af_IOleInPlaceSite_OnInPlaceDeactivate is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_OnInPlaceDeactivate);
   pragma Machine_Attribute (af_IOleInPlaceSite_OnInPlaceDeactivate, "ms_abi");

   type af_IOleInPlaceSite_DiscardUndoState is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_DiscardUndoState);
   pragma Machine_Attribute (af_IOleInPlaceSite_DiscardUndoState, "ms_abi");

   type af_IOleInPlaceSite_DeactivateAndUndo is access
     function (This : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_DeactivateAndUndo);
   pragma Machine_Attribute (af_IOleInPlaceSite_DeactivateAndUndo, "ms_abi");

   type af_IOleInPlaceSite_OnPosRectChange is access
     function (This        : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               lprcPosRect : Pointer_To_RECT)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IOleInPlaceSite_OnPosRectChange);
   pragma Machine_Attribute (af_IOleInPlaceSite_OnPosRectChange, "ms_abi");

end GNATOCX_Site;
