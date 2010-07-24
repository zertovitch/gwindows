with GNATCOM.Errors;

package body GNATOCX.IPersistStream_Interface is

   procedure Initialize (This : in out IPersistStream_Type) is
   begin
      Set_IID (This, IID_IPersistStream);
   end Initialize;

   function Pointer (This : IPersistStream_Type)
     return Pointer_To_IPersistStream
   is
   begin
      return To_Pointer_To_IPersistStream (Address (This));
   end Pointer;

   procedure Attach (This    : in out IPersistStream_Type;
                     Pointer : in     Pointer_To_IPersistStream)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GetClassID
     (This     : IPersistStream_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetClassID
         (Pointer (This),
          pClassID));

   end GetClassID;

   procedure IsDirty
     (This : IPersistStream_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsDirty
         (Pointer (This)));

   end IsDirty;

   procedure Load
     (This : IPersistStream_Type;
      pstm : Pointer_To_IStream)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Load
         (Pointer (This),
          pstm));

   end Load;

   procedure Save
     (This        : IPersistStream_Type;
      pstm        : Pointer_To_IStream;
      fClearDirty : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Save
         (Pointer (This),
          pstm,
          fClearDirty));

   end Save;

   procedure GetSizeMax
     (This    : IPersistStream_Type;
      pcbSize : Pointer_To_uULARGE_INTEGER)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetSizeMax
         (Pointer (This),
          pcbSize));

   end GetSizeMax;

end GNATOCX.IPersistStream_Interface;
