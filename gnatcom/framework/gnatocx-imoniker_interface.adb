with GNATCOM.Errors;

package body GNATOCX.IMoniker_Interface is

   procedure Initialize (This : in out IMoniker_Type) is
   begin
      Set_IID (This, IID_IMoniker);
   end Initialize;

   function Pointer (This : IMoniker_Type)
     return Pointer_To_IMoniker
   is
   begin
      return To_Pointer_To_IMoniker (Address (This));
   end Pointer;

   procedure Attach (This    : in out IMoniker_Type;
                     Pointer : in     Pointer_To_IMoniker)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GetClassID
     (This     : IMoniker_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetClassID
         (Pointer (This),
          pClassID));

   end GetClassID;

   procedure IsDirty
     (This : IMoniker_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsDirty
         (Pointer (This)));

   end IsDirty;

   procedure Load
     (This : IMoniker_Type;
      pstm : Pointer_To_IStream)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Load
         (Pointer (This),
          pstm));

   end Load;

   procedure Save
     (This        : IMoniker_Type;
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
     (This    : IMoniker_Type;
      pcbSize : Pointer_To_uULARGE_INTEGER)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetSizeMax
         (Pointer (This),
          pcbSize));

   end GetSizeMax;

   procedure RemoteBindToObject
     (This       : IMoniker_Type;
      pbc        : Pointer_To_IBindCtx;
      pmkToLeft  : Pointer_To_IMoniker;
      riidResult : GNATCOM.Types.Pointer_To_GUID;
      ppvResult  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteBindToObject
         (Pointer (This),
          pbc,
          pmkToLeft,
          riidResult,
          ppvResult));

   end RemoteBindToObject;

   procedure RemoteBindToStorage
     (This      : IMoniker_Type;
      pbc       : Pointer_To_IBindCtx;
      pmkToLeft : Pointer_To_IMoniker;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      ppvObj    : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteBindToStorage
         (Pointer (This),
          pbc,
          pmkToLeft,
          riid,
          ppvObj));

   end RemoteBindToStorage;

   procedure Reduce
     (This           : IMoniker_Type;
      pbc            : Pointer_To_IBindCtx;
      dwReduceHowFar : Interfaces.C.unsigned_long;
      ppmkToLeft     : Pointer_To_Pointer_To_IMoniker;
      ppmkReduced    : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reduce
         (Pointer (This),
          pbc,
          dwReduceHowFar,
          ppmkToLeft,
          ppmkReduced));

   end Reduce;

   procedure ComposeWith
     (This              : IMoniker_Type;
      pmkRight          : Pointer_To_IMoniker;
      fOnlyIfNotGeneric : Interfaces.C.long;
      ppmkComposite     : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ComposeWith
         (Pointer (This),
          pmkRight,
          fOnlyIfNotGeneric,
          ppmkComposite));

   end ComposeWith;

   procedure Enum
     (This          : IMoniker_Type;
      fForward      : Interfaces.C.long;
      ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Enum
         (Pointer (This),
          fForward,
          ppenumMoniker));

   end Enum;

   procedure IsEqual
     (This            : IMoniker_Type;
      pmkOtherMoniker : Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsEqual
         (Pointer (This),
          pmkOtherMoniker));

   end IsEqual;

   procedure Hash
     (This    : IMoniker_Type;
      pdwHash : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Hash
         (Pointer (This),
          pdwHash));

   end Hash;

   procedure IsRunning
     (This            : IMoniker_Type;
      pbc             : Pointer_To_IBindCtx;
      pmkToLeft       : Pointer_To_IMoniker;
      pmkNewlyRunning : Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsRunning
         (Pointer (This),
          pbc,
          pmkToLeft,
          pmkNewlyRunning));

   end IsRunning;

   procedure GetTimeOfLastChange
     (This      : IMoniker_Type;
      pbc       : Pointer_To_IBindCtx;
      pmkToLeft : Pointer_To_IMoniker;
      pfiletime : Pointer_To_uFILETIME)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetTimeOfLastChange
         (Pointer (This),
          pbc,
          pmkToLeft,
          pfiletime));

   end GetTimeOfLastChange;

   procedure Inverse
     (This : IMoniker_Type;
      ppmk : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Inverse
         (Pointer (This),
          ppmk));

   end Inverse;

   procedure CommonPrefixWith
     (This       : IMoniker_Type;
      pmkOther   : Pointer_To_IMoniker;
      ppmkPrefix : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CommonPrefixWith
         (Pointer (This),
          pmkOther,
          ppmkPrefix));

   end CommonPrefixWith;

   procedure RelativePathTo
     (This        : IMoniker_Type;
      pmkOther    : Pointer_To_IMoniker;
      ppmkRelPath : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RelativePathTo
         (Pointer (This),
          pmkOther,
          ppmkRelPath));

   end RelativePathTo;

   procedure GetDisplayName
     (This            : IMoniker_Type;
      pbc             : Pointer_To_IBindCtx;
      pmkToLeft       : Pointer_To_IMoniker;
      ppszDisplayName : GNATCOM.Types.Pointer_To_LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetDisplayName
         (Pointer (This),
          pbc,
          pmkToLeft,
          ppszDisplayName));

   end GetDisplayName;

   procedure ParseDisplayName
     (This           : IMoniker_Type;
      pbc            : Pointer_To_IBindCtx;
      pmkToLeft      : Pointer_To_IMoniker;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ParseDisplayName
         (Pointer (This),
          pbc,
          pmkToLeft,
          pszDisplayName,
          pchEaten,
          ppmkOut));

   end ParseDisplayName;

   procedure IsSystemMoniker
     (This     : IMoniker_Type;
      pdwMksys : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsSystemMoniker
         (Pointer (This),
          pdwMksys));

   end IsSystemMoniker;

end GNATOCX.IMoniker_Interface;
