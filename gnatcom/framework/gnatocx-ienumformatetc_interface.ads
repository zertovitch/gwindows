with GNATCOM.Iinterface;

package GNATOCX.IEnumFORMATETC_Interface is

   type IEnumFORMATETC_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumFORMATETC_Type);

   function Pointer (This : IEnumFORMATETC_Type)
     return Pointer_To_IEnumFORMATETC;

   procedure Attach (This    : in out IEnumFORMATETC_Type;
                     Pointer : in     Pointer_To_IEnumFORMATETC);

   procedure RemoteNext
     (This         : IEnumFORMATETC_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_FORMATETC;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumFORMATETC_Type;
      celt : Interfaces.C.unsigned_long);

   procedure Reset
     (This : IEnumFORMATETC_Type);

   procedure Clone
     (This   : IEnumFORMATETC_Type;
      ppenum : Pointer_To_Pointer_To_IEnumFORMATETC);

end GNATOCX.IEnumFORMATETC_Interface;
