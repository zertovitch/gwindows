with GNATCOM.Errors;

package body GNATOCX.IEnumFORMATETC_Interface is

   procedure Initialize (This : in out IEnumFORMATETC_Type) is
   begin
      Set_IID (This, IID_IEnumFORMATETC);
   end Initialize;

   function Pointer (This : IEnumFORMATETC_Type)
     return Pointer_To_IEnumFORMATETC
   is
   begin
      return To_Pointer_To_IEnumFORMATETC (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumFORMATETC_Type;
                     Pointer : in     Pointer_To_IEnumFORMATETC)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumFORMATETC_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_FORMATETC;
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
     (This : IEnumFORMATETC_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumFORMATETC_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumFORMATETC_Type;
      ppenum : Pointer_To_Pointer_To_IEnumFORMATETC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumFORMATETC_Interface;
