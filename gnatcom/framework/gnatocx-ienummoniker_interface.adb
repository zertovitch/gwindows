with GNATCOM.Errors;

package body GNATOCX.IEnumMoniker_Interface is

   procedure Initialize (This : in out IEnumMoniker_Type) is
   begin
      Set_IID (This, IID_IEnumMoniker);
   end Initialize;

   function Pointer (This : IEnumMoniker_Type)
     return Pointer_To_IEnumMoniker
   is
   begin
      return To_Pointer_To_IEnumMoniker (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumMoniker_Type;
                     Pointer : in     Pointer_To_IEnumMoniker)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumMoniker_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_Pointer_To_IMoniker;
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
     (This : IEnumMoniker_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumMoniker_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumMoniker_Type;
      ppenum : Pointer_To_Pointer_To_IEnumMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumMoniker_Interface;
