with GNATCOM.Errors;

package body GNATOCX.IParseDisplayName_Interface is

   procedure Initialize (This : in out IParseDisplayName_Type) is
   begin
      Set_IID (This, IID_IParseDisplayName);
   end Initialize;

   function Pointer (This : IParseDisplayName_Type)
     return Pointer_To_IParseDisplayName
   is
   begin
      return To_Pointer_To_IParseDisplayName (Address (This));
   end Pointer;

   procedure Attach (This    : in out IParseDisplayName_Type;
                     Pointer : in     Pointer_To_IParseDisplayName)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure ParseDisplayName
     (This           : IParseDisplayName_Type;
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

end GNATOCX.IParseDisplayName_Interface;
