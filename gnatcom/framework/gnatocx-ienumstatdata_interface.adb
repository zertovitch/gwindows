with GNATCOM.Errors;

package body GNATOCX.IEnumSTATDATA_Interface is

   procedure Initialize (This : in out IEnumSTATDATA_Type) is
   begin
      Set_IID (This, IID_IEnumSTATDATA);
   end Initialize;

   function Pointer (This : IEnumSTATDATA_Type)
     return Pointer_To_IEnumSTATDATA
   is
   begin
      return To_Pointer_To_IEnumSTATDATA (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumSTATDATA_Type;
                     Pointer : in     Pointer_To_IEnumSTATDATA)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumSTATDATA_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_STATDATA;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteNext
         (Pointer (This),
          celt,
          rgelt,
          pceltFetched));

   end RemoteNext;

   procedure Skip
     (This : IEnumSTATDATA_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumSTATDATA_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumSTATDATA_Type;
      ppenum : Pointer_To_Pointer_To_IEnumSTATDATA)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumSTATDATA_Interface;
