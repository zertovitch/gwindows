with GNATCOM.Iinterface;

package GNATOCX.IStream_Interface is

   type IStream_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IStream_Type);

   function Pointer (This : IStream_Type)
     return Pointer_To_IStream;

   procedure Attach (This    : in out IStream_Type;
                     Pointer : in     Pointer_To_IStream);

   procedure RemoteRead
     (This    : IStream_Type;
      pv      : Pointer_To_unsigned_char;
      cb      : Interfaces.C.unsigned_long;
      pcbRead : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure RemoteWrite
     (This       : IStream_Type;
      pv         : Pointer_To_unsigned_char;
      cb         : Interfaces.C.unsigned_long;
      pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure RemoteSeek
     (This            : IStream_Type;
      dlibMove        : uLARGE_INTEGER;
      dwOrigin        : Interfaces.C.unsigned_long;
      plibNewPosition : Pointer_To_uULARGE_INTEGER);

   procedure SetSize
     (This       : IStream_Type;
      libNewSize : uULARGE_INTEGER);

   procedure RemoteCopyTo
     (This       : IStream_Type;
      pstm       : Pointer_To_IStream;
      cb         : uULARGE_INTEGER;
      pcbRead    : Pointer_To_uULARGE_INTEGER;
      pcbWritten : Pointer_To_uULARGE_INTEGER);

   procedure Commit
     (This           : IStream_Type;
      grfCommitFlags : Interfaces.C.unsigned_long);

   procedure Revert
     (This : IStream_Type);

   procedure LockRegion
     (This       : IStream_Type;
      libOffset  : uULARGE_INTEGER;
      cb         : uULARGE_INTEGER;
      dwLockType : Interfaces.C.unsigned_long);

   procedure UnlockRegion
     (This       : IStream_Type;
      libOffset  : uULARGE_INTEGER;
      cb         : uULARGE_INTEGER;
      dwLockType : Interfaces.C.unsigned_long);

   procedure Stat
     (This        : IStream_Type;
      pstatstg    : Pointer_To_STATSTG;
      grfStatFlag : Interfaces.C.unsigned_long);

   procedure Clone
     (This  : IStream_Type;
      ppstm : Pointer_To_Pointer_To_IStream);

end GNATOCX.IStream_Interface;
