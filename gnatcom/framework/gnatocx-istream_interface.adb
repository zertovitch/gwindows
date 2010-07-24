with GNATCOM.Errors;

package body GNATOCX.IStream_Interface is

   procedure Initialize (This : in out IStream_Type) is
   begin
      Set_IID (This, IID_IStream);
   end Initialize;

   function Pointer (This : IStream_Type)
     return Pointer_To_IStream
   is
   begin
      return To_Pointer_To_IStream (Address (This));
   end Pointer;

   procedure Attach (This    : in out IStream_Type;
                     Pointer : in     Pointer_To_IStream)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteRead
     (This    : IStream_Type;
      pv      : Pointer_To_unsigned_char;
      cb      : Interfaces.C.unsigned_long;
      pcbRead : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteRead
         (Pointer (This),
          pv,
          cb,
          pcbRead));

   end RemoteRead;

   procedure RemoteWrite
     (This       : IStream_Type;
      pv         : Pointer_To_unsigned_char;
      cb         : Interfaces.C.unsigned_long;
      pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteWrite
         (Pointer (This),
          pv,
          cb,
          pcbWritten));

   end RemoteWrite;

   procedure RemoteSeek
     (This            : IStream_Type;
      dlibMove        : uLARGE_INTEGER;
      dwOrigin        : Interfaces.C.unsigned_long;
      plibNewPosition : Pointer_To_uULARGE_INTEGER)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteSeek
         (Pointer (This),
          dlibMove,
          dwOrigin,
          plibNewPosition));

   end RemoteSeek;

   procedure SetSize
     (This       : IStream_Type;
      libNewSize : uULARGE_INTEGER)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetSize
         (Pointer (This),
          libNewSize));

   end SetSize;

   procedure RemoteCopyTo
     (This       : IStream_Type;
      pstm       : Pointer_To_IStream;
      cb         : uULARGE_INTEGER;
      pcbRead    : Pointer_To_uULARGE_INTEGER;
      pcbWritten : Pointer_To_uULARGE_INTEGER)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteCopyTo
         (Pointer (This),
          pstm,
          cb,
          pcbRead,
          pcbWritten));

   end RemoteCopyTo;

   procedure Commit
     (This           : IStream_Type;
      grfCommitFlags : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Commit
         (Pointer (This),
          grfCommitFlags));

   end Commit;

   procedure Revert
     (This : IStream_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Revert
         (Pointer (This)));

   end Revert;

   procedure LockRegion
     (This       : IStream_Type;
      libOffset  : uULARGE_INTEGER;
      cb         : uULARGE_INTEGER;
      dwLockType : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.LockRegion
         (Pointer (This),
          libOffset,
          cb,
          dwLockType));

   end LockRegion;

   procedure UnlockRegion
     (This       : IStream_Type;
      libOffset  : uULARGE_INTEGER;
      cb         : uULARGE_INTEGER;
      dwLockType : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.UnlockRegion
         (Pointer (This),
          libOffset,
          cb,
          dwLockType));

   end UnlockRegion;

   procedure Stat
     (This        : IStream_Type;
      pstatstg    : Pointer_To_STATSTG;
      grfStatFlag : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Stat
         (Pointer (This),
          pstatstg,
          grfStatFlag));

   end Stat;

   procedure Clone
     (This  : IStream_Type;
      ppstm : Pointer_To_Pointer_To_IStream)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppstm));

   end Clone;

end GNATOCX.IStream_Interface;
