with GNATCOM.Iinterface;

package GNATOCX.IParseDisplayName_Interface is

   type IParseDisplayName_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IParseDisplayName_Type);

   function Pointer (This : IParseDisplayName_Type)
     return Pointer_To_IParseDisplayName;

   procedure Attach (This    : in out IParseDisplayName_Type;
                     Pointer : in     Pointer_To_IParseDisplayName);

   procedure ParseDisplayName
     (This           : IParseDisplayName_Type;
      pbc            : Pointer_To_IBindCtx;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker);

end GNATOCX.IParseDisplayName_Interface;
