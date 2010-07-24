with GNATCOM.Iinterface;

package GNATOCX.ISequentialStream_Interface is

   type ISequentialStream_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ISequentialStream_Type);

   function Pointer (This : ISequentialStream_Type)
     return Pointer_To_ISequentialStream;

   procedure Attach (This    : in out ISequentialStream_Type;
                     Pointer : in     Pointer_To_ISequentialStream);

   procedure RemoteRead
     (This    : ISequentialStream_Type;
      pv      : Pointer_To_unsigned_char;
      cb      : Interfaces.C.unsigned_long;
      pcbRead : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure RemoteWrite
     (This       : ISequentialStream_Type;
      pv         : Pointer_To_unsigned_char;
      cb         : Interfaces.C.unsigned_long;
      pcbWritten : GNATCOM.Types.Pointer_To_unsigned_long);

end GNATOCX.ISequentialStream_Interface;
