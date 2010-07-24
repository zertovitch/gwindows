with GNATCOM.Iinterface;

package GNATOCX.IMoniker_Interface is

   type IMoniker_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IMoniker_Type);

   function Pointer (This : IMoniker_Type)
     return Pointer_To_IMoniker;

   procedure Attach (This    : in out IMoniker_Type;
                     Pointer : in     Pointer_To_IMoniker);

   procedure GetClassID
     (This     : IMoniker_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID);

   procedure IsDirty
     (This : IMoniker_Type);

   procedure Load
     (This : IMoniker_Type;
      pstm : Pointer_To_IStream);

   procedure Save
     (This        : IMoniker_Type;
      pstm        : Pointer_To_IStream;
      fClearDirty : Interfaces.C.long);

   procedure GetSizeMax
     (This    : IMoniker_Type;
      pcbSize : Pointer_To_uULARGE_INTEGER);

   procedure RemoteBindToObject
     (This       : IMoniker_Type;
      pbc        : Pointer_To_IBindCtx;
      pmkToLeft  : Pointer_To_IMoniker;
      riidResult : GNATCOM.Types.Pointer_To_GUID;
      ppvResult  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown);

   procedure RemoteBindToStorage
     (This      : IMoniker_Type;
      pbc       : Pointer_To_IBindCtx;
      pmkToLeft : Pointer_To_IMoniker;
      riid      : GNATCOM.Types.Pointer_To_GUID;
      ppvObj    : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown);

   procedure Reduce
     (This           : IMoniker_Type;
      pbc            : Pointer_To_IBindCtx;
      dwReduceHowFar : Interfaces.C.unsigned_long;
      ppmkToLeft     : Pointer_To_Pointer_To_IMoniker;
      ppmkReduced    : Pointer_To_Pointer_To_IMoniker);

   procedure ComposeWith
     (This              : IMoniker_Type;
      pmkRight          : Pointer_To_IMoniker;
      fOnlyIfNotGeneric : Interfaces.C.long;
      ppmkComposite     : Pointer_To_Pointer_To_IMoniker);

   procedure Enum
     (This          : IMoniker_Type;
      fForward      : Interfaces.C.long;
      ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker);

   procedure IsEqual
     (This            : IMoniker_Type;
      pmkOtherMoniker : Pointer_To_IMoniker);

   procedure Hash
     (This    : IMoniker_Type;
      pdwHash : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure IsRunning
     (This            : IMoniker_Type;
      pbc             : Pointer_To_IBindCtx;
      pmkToLeft       : Pointer_To_IMoniker;
      pmkNewlyRunning : Pointer_To_IMoniker);

   procedure GetTimeOfLastChange
     (This      : IMoniker_Type;
      pbc       : Pointer_To_IBindCtx;
      pmkToLeft : Pointer_To_IMoniker;
      pfiletime : Pointer_To_uFILETIME);

   procedure Inverse
     (This : IMoniker_Type;
      ppmk : Pointer_To_Pointer_To_IMoniker);

   procedure CommonPrefixWith
     (This       : IMoniker_Type;
      pmkOther   : Pointer_To_IMoniker;
      ppmkPrefix : Pointer_To_Pointer_To_IMoniker);

   procedure RelativePathTo
     (This        : IMoniker_Type;
      pmkOther    : Pointer_To_IMoniker;
      ppmkRelPath : Pointer_To_Pointer_To_IMoniker);

   procedure GetDisplayName
     (This            : IMoniker_Type;
      pbc             : Pointer_To_IBindCtx;
      pmkToLeft       : Pointer_To_IMoniker;
      ppszDisplayName : GNATCOM.Types.Pointer_To_LPWSTR);

   procedure ParseDisplayName
     (This           : IMoniker_Type;
      pbc            : Pointer_To_IBindCtx;
      pmkToLeft      : Pointer_To_IMoniker;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker);

   procedure IsSystemMoniker
     (This     : IMoniker_Type;
      pdwMksys : GNATCOM.Types.Pointer_To_unsigned_long);

end GNATOCX.IMoniker_Interface;
