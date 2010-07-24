with GNATCOM.Errors;

package body GNATOCX.IEnumOLEVERB_Interface is

   procedure Initialize (This : in out IEnumOLEVERB_Type) is
   begin
      Set_IID (This, IID_IEnumOLEVERB);
   end Initialize;

   function Pointer (This : IEnumOLEVERB_Type)
     return Pointer_To_IEnumOLEVERB
   is
   begin
      return To_Pointer_To_IEnumOLEVERB (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumOLEVERB_Type;
                     Pointer : in     Pointer_To_IEnumOLEVERB)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteNext
     (This         : IEnumOLEVERB_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_OLEVERB;
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
     (This : IEnumOLEVERB_Type;
      celt : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          celt));

   end Skip;

   procedure Reset
     (This : IEnumOLEVERB_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   procedure Clone
     (This   : IEnumOLEVERB_Type;
      ppenum : Pointer_To_Pointer_To_IEnumOLEVERB)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum));

   end Clone;

end GNATOCX.IEnumOLEVERB_Interface;
