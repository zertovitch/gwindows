with GNATCOM.Errors;

package body GNATOCX.IEnumString_Interface is

   procedure Initialize (This : in out IEnumString_Type) is
   begin
      Set_IID (This, IID_IEnumString);
   end Initialize;

   function Pointer (This : IEnumString_Type)
     return Pointer_To_IEnumString
   is
   begin
      return To_Pointer_To_IEnumString (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumString_Type;
                     Pointer : in     Pointer_To_IEnumString)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumString_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : GNATCOM.Types.Pointer_To_LPWSTR;
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
     (This : IEnumString_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumString_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumString_Type;
      ppenum : Pointer_To_Pointer_To_IEnumString)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumString_Interface;
