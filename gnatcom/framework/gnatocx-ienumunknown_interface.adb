with GNATCOM.Errors;

package body GNATOCX.IEnumUnknown_Interface is

   procedure Initialize (This : in out IEnumUnknown_Type) is
   begin
      Set_IID (This, IID_IEnumUnknown);
   end Initialize;

   function Pointer (This : IEnumUnknown_Type)
     return Pointer_To_IEnumUnknown
   is
   begin
      return To_Pointer_To_IEnumUnknown (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumUnknown_Type;
                     Pointer : in     Pointer_To_IEnumUnknown)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumUnknown_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown;
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
     (This : IEnumUnknown_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumUnknown_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumUnknown_Type;
      ppenum : Pointer_To_Pointer_To_IEnumUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumUnknown_Interface;
