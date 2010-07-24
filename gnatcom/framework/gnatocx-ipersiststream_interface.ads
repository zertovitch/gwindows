with GNATCOM.Iinterface;

package GNATOCX.IPersistStream_Interface is

   type IPersistStream_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IPersistStream_Type);

   function Pointer (This : IPersistStream_Type)
     return Pointer_To_IPersistStream;

   procedure Attach (This    : in out IPersistStream_Type;
                     Pointer : in     Pointer_To_IPersistStream);

   procedure GetClassID
     (This     : IPersistStream_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID);

   procedure IsDirty
     (This : IPersistStream_Type);

   procedure Load
     (This : IPersistStream_Type;
      pstm : Pointer_To_IStream);

   procedure Save
     (This        : IPersistStream_Type;
      pstm        : Pointer_To_IStream;
      fClearDirty : Interfaces.C.long);

   procedure GetSizeMax
     (This    : IPersistStream_Type;
      pcbSize : Pointer_To_uULARGE_INTEGER);

end GNATOCX.IPersistStream_Interface;
