with GNATCOM.Iinterface;

package GNATOCX.IOleContainer_Interface is

   type IOleContainer_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IOleContainer_Type);

   function Pointer (This : IOleContainer_Type)
     return Pointer_To_IOleContainer;

   procedure Attach (This    : in out IOleContainer_Type;
                     Pointer : in     Pointer_To_IOleContainer);

   procedure ParseDisplayName
     (This           : IOleContainer_Type;
      pbc            : Pointer_To_IBindCtx;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker);

   procedure EnumObjects
     (This     : IOleContainer_Type;
      grfFlags : Interfaces.C.unsigned_long;
      ppenum   : Pointer_To_Pointer_To_IEnumUnknown);

   procedure LockContainer
     (This  : IOleContainer_Type;
      fLock : Interfaces.C.long);

end GNATOCX.IOleContainer_Interface;
