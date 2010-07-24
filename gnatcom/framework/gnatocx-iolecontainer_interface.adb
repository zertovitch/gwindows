with GNATCOM.Errors;

package body GNATOCX.IOleContainer_Interface is

   procedure Initialize (This : in out IOleContainer_Type) is
   begin
      Set_IID (This, IID_IOleContainer);
   end Initialize;

   function Pointer (This : IOleContainer_Type)
     return Pointer_To_IOleContainer
   is
   begin
      return To_Pointer_To_IOleContainer (Address (This));
   end Pointer;

   procedure Attach (This    : in out IOleContainer_Type;
                     Pointer : in     Pointer_To_IOleContainer)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure ParseDisplayName
     (This           : IOleContainer_Type;
      pbc            : Pointer_To_IBindCtx;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ParseDisplayName
         (Pointer (This),
          pbc,
          pszDisplayName,
          pchEaten,
          ppmkOut));

   end ParseDisplayName;

   procedure EnumObjects
     (This     : IOleContainer_Type;
      grfFlags : Interfaces.C.unsigned_long;
      ppenum   : Pointer_To_Pointer_To_IEnumUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumObjects
         (Pointer (This),
          grfFlags,
          ppenum));

   end EnumObjects;

   procedure LockContainer
     (This  : IOleContainer_Type;
      fLock : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.LockContainer
         (Pointer (This),
          fLock));

   end LockContainer;

end GNATOCX.IOleContainer_Interface;
