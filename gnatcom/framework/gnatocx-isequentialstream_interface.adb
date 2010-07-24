with GNATCOM.Errors;

package body GNATOCX.ISequentialStream_Interface is

   procedure Initialize (This : in out ISequentialStream_Type) is
   begin
      Set_IID (This, IID_ISequentialStream);
   end Initialize;

   function Pointer (This : ISequentialStream_Type)
     return Pointer_To_ISequentialStream
   is
   begin
      return To_Pointer_To_ISequentialStream (Address (This));
   end Pointer;

   procedure Attach (This    : in out ISequentialStream_Type;
                     Pointer : in     Pointer_To_ISequentialStream)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteRead
     (This    : ISequentialStream_Type;
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
     (This       : ISequentialStream_Type;
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

end GNATOCX.ISequentialStream_Interface;
